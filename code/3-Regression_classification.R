####Classification Regression ###

setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version")
data <- read.csv("data_merged_classified_deduplicated_FINAL.csv", stringsAsFactors = FALSE)


# -----------------------------------------------------------
# 0.  SET‑UP
# -----------------------------------------------------------
library(tidyverse)
library(dplyr)      # data wrangling
library(lme4)       # multilevel GLM
library(broom.mixed)  # tidy() for lme4 objects
# If you want Bayesian later: library(brms)

# -----------------------------------------------------------
# 1.  READ & PREP DATA
# -----------------------------------------------------------

# Safety classification as a factor (use your preferred reference level)
data$safety_classification <- factor(data$safety_classification)

# Institution group as factor (“Academic AI”, “Corporate AI”)
data$institution_group     <- factor(data$institution_group)


data <- data %>%
  mutate(across(c(ab, title_clean), ~ replace_na(.x, "")))

data <- data %>%
  mutate(across(c(cited_by_count), ~ replace_na(.x, 0)))

# -----------------------------------------------------------
# 2.  BUILD BINARY TARGETS
# -----------------------------------------------------------
# Top‑10 % and Top‑1 % cut‑offs
thresh_10 <- quantile(data$cited_by_count, 0.90, na.rm = TRUE)
thresh_01 <- quantile(data$cited_by_count, 0.99, na.rm = TRUE)

data <- data %>% 
  mutate(top10 = ifelse(cited_by_count >= thresh_10, 1, 0),
         top01 = ifelse(cited_by_count >= thresh_01, 1, 0))

# -----------------------------------------------------------
# 3A.  QUICK SINGLE‑LEVEL LOGIT & PROBIT
# -----------------------------------------------------------
logit10  <- glm(top10 ~ safety_classification  + institution_group , #+ not_ai_safety 
                family = binomial(link = "logit"), data = data)

probit01 <- glm(top01 ~ safety_classification + institution_group, #+ not_ai_safety 
                family = binomial(link = "probit"), data = data)

summary(logit10)
summary(probit01)

# -----------------------------------------------------------
# 3B.  MULTILEVEL (RANDOM‑EFFECT) LOGIT
#     Random intercept for institution_group
# -----------------------------------------------------------
mlogit01 <- glmer(
  top01 ~ safety_classification +
    (1 | institution_group),   # random intercept
  family = binomial(link = "logit"),
  data   = data,
  control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
)

# OPTIONAL: let the safety slope vary by group, too
mlogit01_rslope <- glmer(
  top01 ~ safety_classification + 
    (safety_classification | institution_group),
  family  = binomial(link = "logit"),
  data    = data,
  control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
)

tidy(mlogit01)          # nice coefficient table
tidy(mlogit10_rslope)

mlogit_fixedInt <- glm(
  top01 ~ safety_classification * institution_group,
  data   = data,
  family = binomial
)

# -----------------------------------------------------------
# 4.  PREDICTED PROBABILITIES (example)
# -----------------------------------------------------------
newdat <- expand.grid(
  safety_classification = levels(data$safety_classification),
  #not_ai_safety         = c(0,1),
  institution_group     = "Corporate AI"
)
newdat$pred_top10 <- predict(mlogit10, newdat, type = "response")
newdat

# -----------------------------------------------------------
# 5.  (OPTIONAL) FAST BAYESIAN LOGIT WITH VARIATIONAL FIT
# -----------------------------------------------------------

library(brms)
library(cmdstanr)
#remotes::install_github("stan-dev/cmdstanr")

#set_cmdstan_path("/Users/Ilan/cmdstan/cmdstan-2.36.0")
set_cmdstan_path("/Users/Ilan/cmdstan/cmdstan-2.36.0/cmdstan-2.32.0")

priors <- c(
  # Cauchy(0, 2.5) on every slope (incl. topic dummies)
  prior(cauchy(0, 2.5), class = "b"),
  # Weakly‑informative Cauchy on the intercept
  prior(cauchy(0, 10),  class = "Intercept"),
  # Half‑Exponential(1) on the SD of the random intercept
  prior(exponential(1), class = "sd"),
 prior(lkj(2), class = cor) 
)

#options(brms.backend = "rstan")



fit_bayes <- brm(
  top10 ~ safety_classification + (safety_classification| institution_group),
  family    = bernoulli(link = "logit"),
  data      = data,
  prior     = priors,
  backend   = "cmdstanr", #"cmdstanr",   # fast sampler
  iter      = 6000, chains = 4, cores = 4, seed = 42,
  algorithm = "meanfield"
  # if you need a *really* quick fit, swap the sampler:
  # algorithm = "meanfield"   # variational Bayes
)

# Average marginal effect of not_ai_safety
library(marginaleffects)
avg_slopes(fit_bayes, variables = "not_ai_safety")


summary(fit_bayes)

 summary(bayes_logit10)
