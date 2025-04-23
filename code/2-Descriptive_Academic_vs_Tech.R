##### academic vs. tech - descriptive statistics ####

setwd("~/Desktop/Ilan Folder/2024/NYC AI/Papers/Gaps/Gaps/research/Second version - KEY")
data <- read.csv("data_merged_classified_deduplicated_FINAL.csv", stringsAsFactors = FALSE)

library(tidyverse)
library(dplyr)

dim(data)

# Define your institutional ROR IDs
ror_ids <- c(
  "00971b260",  # DeepMind
  "00njsd438",  # Google
  "01zbnvs85",  # Meta
  "00d0nc645",  # Microsoft
  "05wx9n238",  # OpenAI
  "056y0v115",  # Anthropic
  "00f54p054",  # Stanford  
  "042nb2s44",  # MIT  
  "05x2bcf33",  # CMU  
  "01an7q238",   # Berkeley
  "0190ak572", # New York University
  "00cvxb145" #University of Washington
)

# Define the publication year range
#2020-2025 march 31


# Create institution mapping 
institution_map <- tibble(
  ror_id = c(
    "00971b260",  # DeepMind
    "00njsd438",  # Google
    "01zbnvs85",  # Meta
    "00d0nc645",  # Microsoft
    "05wx9n238",  # OpenAI
    "056y0v115",  # Anthropic
    "00f54p054",  # Stanford  
    "042nb2s44",  # MIT  
    "05x2bcf33",  # CMU  
    "01an7q238",   # Berkeley
    "0190ak572", # New York University
    "00cvxb145" #University of Washington
  ),
  institution_name = c(
    "DeepMind",
    "Google",
    "Meta",
    "Microsoft",
    "OpenAI",
    "Anthropic",
    "Stanford",
    "MIT",
    "CMU",
    "UC Berkeley",
    "New York University",
    "University of Washington"
  )
)



data <- data %>%
  mutate(safety_label = case_when(
    safety_classification == "not_ai_safety" ~ "Not AI Safety",
    safety_classification == "testing_and_evaluation" ~ "Testing & Evaluation",
    safety_classification == "alignment_pre_deployment" ~ "Alignment",
    safety_classification == "policy_and_governance" ~ "Policy & Governance",
    safety_classification == "privacy_and_security" ~ "Privacy & Security",
    safety_classification == "ethics_and_bias" ~ "Ethics & Bias",
    safety_classification == "interpretability_and_transparency" ~ "Interpretability & Transparency",
    safety_classification == "post_deployment_risks_and_model_traits" ~ "Post-Deployment & Model Traits",
    safety_classification == "multi_agent_and_agentic_safety" ~ "Multi-Agent & Agentic",
    TRUE ~ safety_classification
  ))


data$not_ai_safety <- data$safety_classification == "not_ai_safety"
table(data$not_ai_safety)

#check to make sure is unique assignment
data %>% group_by(institution_group, institution_name) %>%
  summarise(n=n()) %>%
  View()


llm_institution_metrics_fractional <- data %>%
  # Group by clean_ror_id 
  group_by(clean_ror_id) %>%
  summarise(
    publication_count = sum(fraction),  # Fractional publication count
    citation_count = sum(fraction * cited_by_count, na.rm = TRUE),  # Fractional citation count
    mean_citations_per_paper = if(sum(fraction) > 0) sum(fraction * cited_by_count, na.rm = TRUE) / sum(fraction) else 0,
    .groups = "drop"
  ) %>%
  # Now join with institution map to add names
  left_join(institution_map, by = c("clean_ror_id" = "ror_id")) %>%
  # Combine Google and DeepMind data
  mutate(
    is_google_deepmind = clean_ror_id %in% c("00971b260", "00njsd438")  # DeepMind and Google ROR IDs
  ) %>%
  mutate(
    institution_name = case_when(
      is_google_deepmind ~ "Google DeepMind",
      TRUE ~ institution_name
    )
  ) %>%
  # Group again to combine Google and DeepMind metrics
  group_by(institution_name) %>%
  summarise(
    clean_ror_id = paste(clean_ror_id, collapse = ";"), #keep both of their ror ids OLD: clean_ror_id = first(clean_ror_id), 
    publication_count = sum(publication_count),
    citation_count = sum(citation_count),
    mean_citations_per_paper = sum(citation_count) / sum(publication_count),
    .groups = "drop"
  ) %>%
  # Add institution grouping
  mutate(
    institution_group = case_when(
      institution_name %in% c("CMU", "Stanford", "University of Washington", 
                              "UC Berkeley", "MIT", "New York University"
      ) ~ "Academic AI",
      !is.na(institution_name) ~ "Corporate AI")
  ) %>%
  filter(!is.na(institution_group))



# Calculate group totals
group_totals <- llm_institution_metrics_fractional %>%
  group_by(institution_group) %>%
  reframe(
    institution_name = paste0("Total ", institution_group),
    clean_ror_id = "TOTAL",
    publication_count = sum(publication_count),
    citation_count = sum(citation_count),
    mean_citations_per_paper = sum(citation_count) / sum(publication_count)
  ) %>%
  unique()

# Add the group totals to the metrics
llm_institution_metrics_with_totals <- bind_rows(
  llm_institution_metrics_fractional,
  group_totals
) %>%
  arrange(institution_group, desc(publication_count))

View(llm_institution_metrics_with_totals)


library(xtable)
library(dplyr)
library(knitr)
library(kableExtra)

llm_institution_metrics_with_totals   %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary of institutions with publication counts and total citations."
  ) %>%
  kable_styling(
    latex_options = c("striped", "HOLD_position")
  )


safety_data <- data %>%
  filter(not_ai_safety == FALSE)  # This filters to just safety papers

safety_institution_metrics_fractional <- safety_data %>%
  group_by(clean_ror_id) %>%
  summarise(
    publication_count = sum(fraction, na.rm = TRUE),
    citation_count = sum(fraction * cited_by_count, na.rm = TRUE),
    mean_citations_per_paper = if (sum(fraction, na.rm = TRUE) > 0)
      sum(fraction * cited_by_count, na.rm = TRUE) / sum(fraction)
    else 0,
    .groups = "drop"
  ) %>%
  left_join(institution_map, by = c("clean_ror_id" = "ror_id")) %>%
  mutate(
    is_google_deepmind = clean_ror_id %in% c("00971b260", "00njsd438"),
    institution_name = case_when(
      is_google_deepmind ~ "Google DeepMind",
      TRUE ~ institution_name
    )
  ) %>%
  group_by(institution_name) %>%
  summarise(
    clean_ror_id = paste(clean_ror_id, collapse = ";"),
    publication_count = sum(publication_count),
    citation_count = sum(citation_count),
    mean_citations_per_paper = sum(citation_count) / sum(publication_count),
    .groups = "drop"
  ) %>%
  mutate(
    institution_group = case_when(
      institution_name %in% c("CMU", "Stanford", "University of Washington",
                              "UC Berkeley", "MIT", "New York University") ~ "Academic AI",
      !is.na(institution_name) ~ "Corporate AI"
    )
  ) %>%
  filter(!is.na(institution_group))

group_totals_safety <- safety_institution_metrics_fractional %>%
  group_by(institution_group) %>%
  reframe(
    institution_name = paste0("Total ", institution_group),
    clean_ror_id = "TOTAL",
    publication_count = sum(publication_count),
    citation_count = sum(citation_count),
    mean_citations_per_paper = sum(citation_count) / sum(publication_count)
  ) %>%
  unique()

safety_institution_metrics_with_totals <- bind_rows(
  safety_institution_metrics_fractional,
  group_totals_safety
   ) %>%
  arrange(institution_group, desc(publication_count))


comparison_table <- llm_institution_metrics_with_totals %>%
  select(
    institution_group, institution_name, clean_ror_id,
    llm_pub_count = publication_count,
    llm_citation_count = citation_count,
    llm_mean_citations = mean_citations_per_paper
  ) %>%
  left_join(
    safety_institution_metrics_with_totals %>%
      select(
        institution_name,
        safety_pub_count = publication_count,
        safety_citation_count = citation_count
      ),
    by = "institution_name"
  ) %>%
  mutate(
    # Calculate percentages only where we have counts
    safety_pub_percent = round(100 * safety_pub_count / llm_pub_count),
    safety_citation_percent = round(100 * safety_citation_count / llm_citation_count),
    
    # Rounding for LaTeX table
    llm_pub_count_rounded = round(llm_pub_count),
    llm_citation_count_rounded = round(llm_citation_count),
    llm_mean_citations_rounded = round(llm_mean_citations)
    ) %>%
  arrange(institution_group, desc(llm_pub_count))

View(comparison_table)

comparison_table %>%
  select( institution_name, safety_pub_percent, safety_citation_percent, llm_pub_count, llm_citation_count, safety_pub_count, 
          safety_citation_count) %>%
  View()

##latex table ###

latex_table <- "\\begin{table}[H]
\\centering
\\caption{\\large{Academic vs Corporate LLM Research with AI Safety Metrics}}
\\begin{tabular}[t]{lrrrrrr}
\\toprule
Institution & Count & \\% Safety Count & Total Citations & \\% Safety Cite & Mean Cite\\\\
\\midrule\n"

# Add rows for each institution
row_counter <- 0
for(i in 1:nrow(comparison_table)) {
  if (startsWith(comparison_table$institution_name[i], "Total")) {
    next  # Skip totals for now
  }
  
  # Alternate row shading
  cell_color <- if (row_counter %% 2 == 0) "\\cellcolor{gray!10}" else ""
  
  latex_table <- paste0(
    latex_table,
    cell_color, comparison_table$institution_name[i], " & ",
    cell_color, format(comparison_table$llm_pub_count_rounded[i], big.mark = ","), " & ",
    cell_color, comparison_table$safety_pub_percent[i], "\\% & ",
    cell_color, format(comparison_table$llm_citation_count_rounded[i], big.mark = ","), " & ",
    cell_color, comparison_table$safety_citation_percent[i], "\\% & ",
    cell_color, comparison_table$llm_mean_citations_rounded[i], " \\\\\n"
  )
  
  row_counter <- row_counter + 1
}

latex_table <- paste0(latex_table, "\\bottomrule\n\\bottomrule\n")

# Add total rows

academic_total <- comparison_table %>%
  filter(institution_name == "Total Academic AI") %>%
  select(llm_pub_count_rounded, safety_pub_percent, llm_citation_count_rounded, 
         safety_citation_percent, llm_mean_citations_rounded)

corporate_total <- comparison_table %>%
  filter(institution_name == "Total Corporate AI") %>%
  select(llm_pub_count_rounded, safety_pub_percent, llm_citation_count_rounded, 
         safety_citation_percent, llm_mean_citations_rounded)

# Academic total row
latex_table <- paste0(
  latex_table,
  "Academic AI & ",
  format(academic_total$llm_pub_count_rounded, big.mark = ","), " & ",
  academic_total$safety_pub_percent, "\\% & ",
  format(academic_total$llm_citation_count_rounded, big.mark = ","), " & ",
  academic_total$safety_citation_percent, "\\% & ",
  academic_total$llm_mean_citations_rounded, " \\\\\n"
)

# Corporate total row with shading
latex_table <- paste0(
  latex_table,
  "\\cellcolor{gray!10}{Corporate AI} & ",
  "\\cellcolor{gray!10}{", format(corporate_total$llm_pub_count_rounded, big.mark = ","), "} & ",
  "\\cellcolor{gray!10}{", corporate_total$safety_pub_percent, "\\%} & ",
  "\\cellcolor{gray!10}{", format(corporate_total$llm_citation_count_rounded, big.mark = ","), "} & ",
  "\\cellcolor{gray!10}{", corporate_total$safety_citation_percent, "\\%} & ",
  "\\cellcolor{gray!10}{", corporate_total$llm_mean_citations_rounded, "} \\\\\n"
)

# Close the table
latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{table}")

# Output the LaTeX code
cat(latex_table)

#Now without the two safety coloumns
latex_table <- "\\begin{table}[H]
\\centering
\\caption{\\large{Academic vs Corporate LLM Research (No Safety Columns)}}
\\begin{tabular}[t]{lrrr}
\\toprule
Institution & Count & Total Citations & Mean Cite\\\\
\\midrule\n"

# Add rows for each institution
row_counter <- 0
for(i in 1:nrow(comparison_table)) {
  if (startsWith(comparison_table$institution_name[i], "Total")) {
    next  # Skip totals for now
  }
  
  # Alternate row shading
  cell_color <- if (row_counter %% 2 == 0) "\\cellcolor{gray!10}" else ""
  
  latex_table <- paste0(
    latex_table,
    cell_color, comparison_table$institution_name[i], " & ",
    cell_color, format(comparison_table$llm_pub_count_rounded[i], big.mark = ","), " & ",
    cell_color, format(comparison_table$llm_citation_count_rounded[i], big.mark = ","), " & ",
    cell_color, comparison_table$llm_mean_citations_rounded[i], " \\\\\n"
  )
  
  row_counter <- row_counter + 1
}

latex_table <- paste0(latex_table, "\\bottomrule\n\\bottomrule\n")

# Totals
academic_total <- comparison_table %>%
  filter(institution_name == "Total Academic AI") %>%
  select(llm_pub_count_rounded, llm_citation_count_rounded, llm_mean_citations_rounded)

corporate_total <- comparison_table %>%
  filter(institution_name == "Total Corporate AI") %>%
  select(llm_pub_count_rounded, llm_citation_count_rounded, llm_mean_citations_rounded)

# Academic total row
latex_table <- paste0(
  latex_table,
  "Academic AI & ",
  format(academic_total$llm_pub_count_rounded, big.mark = ","), " & ",
  format(academic_total$llm_citation_count_rounded, big.mark = ","), " & ",
  academic_total$llm_mean_citations_rounded, " \\\\\n"
)

# Corporate total row
latex_table <- paste0(
  latex_table,
  "\\cellcolor{gray!10}{Corporate AI} & ",
  "\\cellcolor{gray!10}{", format(corporate_total$llm_pub_count_rounded, big.mark = ","), "} & ",
  "\\cellcolor{gray!10}{", format(corporate_total$llm_citation_count_rounded, big.mark = ","), "} & ",
  "\\cellcolor{gray!10}{", corporate_total$llm_mean_citations_rounded, "} \\\\\n"
)

# Close the table
latex_table <- paste0(latex_table, "\\bottomrule\n\\end{tabular}\n\\end{table}")

# Output the LaTeX code
cat(latex_table)



library(lubridate)
#data$year <- year(ymd(data$publication_date))
#table(is.na(data$year))
### VISUALISATION ###


data %>% 
  filter(safety_classification=="post_deployment_risks_and_model_traits") %>%
  select(title_clean, cited_by_count) %>%
  View()

data %>%
  group_by(institution_name,publication_year) %>%
  filter(publication_year<2025) %>%
  summarise(paper_count = sum(fraction, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = publication_year, y = paper_count)) +
  geom_line() +
  facet_wrap(~institution_name, scales = "free")+
  labs(
    title = "",
    x = "",
    y = "Number of Papers"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

library(dplyr)
library(ggplot2)

data <- data %>%
  mutate(period = factor(
    ifelse(publication_year %in% c(2020, 2021, 2022), "2020--2022", "2023--2025"),
    levels = c("2020--2022", "2023--2025"),
    ordered = TRUE
  ))


fig1 <- data %>%
  filter(not_ai_safety == FALSE) %>%  # AI safety papers
  filter(!is.na(institution_group)) %>%
  filter(institution_group=="Corporate AI") %>%
  group_by(safety_label, period) %>%
  summarise(
    paper_count = sum(fraction, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(classification_total = ave(paper_count, safety_label, FUN = sum)) %>%
  
  ggplot(aes(
    x = reorder(safety_label, classification_total),
    y = paper_count,
    fill = factor(period, levels = c("2020--2022", "2023--2025"))
  )) +
  geom_bar(
    stat = "identity",
    color = "black",
    size = 0.3,
    position = position_stack(reverse = TRUE)  # <-- Reverse the stacking order
  ) +
  #facet_wrap(~institution_group, scales="free") +
  labs(
    title = "",
    x = "",
    y = "Fractional Paper Count",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 14)
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  scale_fill_brewer(palette = "Set2") +
  coord_flip()




library(dplyr)
library(ggplot2)

fig2 <- data %>%
  filter(not_ai_safety == FALSE) %>%  # AI safety papers
  filter(!is.na(institution_group)) %>%
  group_by(safety_label) %>%
  summarise(
    # Sum of fraction * citation_count for fractional citation totals
    fractional_citations = sum(fraction * cited_by_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Used for reordering bars by total citations
  mutate(classification_total = ave(fractional_citations, safety_label, FUN = sum)) %>%
  
  ggplot(aes(
    x = reorder(safety_label, classification_total),  # Reorder by total fractional citations
    y = fractional_citations,
    fill = factor(safety_label)
  )) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +  # Outline for better visibility
  labs(
    title = "",
    x = "",
    y = "Fractional Citations",  # Updated axis label
    fill = NULL
  ) +
  #facet_wrap(~institution_group, scales="free") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 13),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 14)
  ) +
  theme(axis.title.x = element_text(margin = margin(t = 20)))+
  scale_fill_brewer(palette = "Set3") +
  coord_flip()

library(cowplot)

# Combine the two plots
combined_plot <- plot_grid(fig1, fig2, nrow=2)
print(combined_plot)


library(dplyr)
library(ggplot2)

# no fractions being applied
data %>%
  filter(not_ai_safety == FALSE) %>%               # Keep AI safety papers
  filter(!is.na(institution_group)) %>%            # Drop rows with NA institution group
  group_by(institution_group, safety_label) %>%
  summarise(
    total_citations = sum(cited_by_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # This will help reorder the bars by total citations within each safety_classification
  mutate(classification_total = ave(total_citations, safety_label, FUN = sum)) %>%
  ggplot(aes(
    x = reorder(safety_label, classification_total),  # Reorder by overall total citations
    y = total_citations,
    fill = factor(safety_label)
  )) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +
  labs(
    title = "",
    x = "",
    y = "Total Citations",
    fill = NULL
  ) +
  facet_wrap(~institution_group, scales = "free") +  # One facet per institution group
  theme_minimal() +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip()

## next two key graphs
library(forcats)

# First, create a dataset with the position information

data_summary_frac <- data %>%
  filter(!is.na(institution_group)) %>%
  filter(not_ai_safety == FALSE) %>%               # Keep AI safety papers
  group_by(institution_name, institution_group, safety_label) %>%
  summarise(fraction_count = sum(fraction, na.rm = TRUE), .groups = "drop") %>%
  group_by(institution_name) %>%
  mutate(company_total = sum(fraction_count)) %>%
  ungroup() %>%
  mutate(company = fct_reorder(institution_name, -company_total))

name_replacements <- c(
  "University of Washington" = "U. of Washington",
  "New York University" = "NYU"
)

data_summary_frac <- data_summary_frac %>%
  mutate(
    # Create a display name for plotting
    display_name = case_when(
      institution_name %in% names(name_replacements) ~ name_replacements[institution_name],
      TRUE ~ institution_name
    )
  ) %>%
  # Recalculate the company factor using display_name but keeping the same ordering
  mutate(company = fct_reorder(display_name, -company_total)) %>%
  # Reorder institution_group to put Corporate AI first
  mutate(institution_group = factor(institution_group, levels = c("Corporate AI", "Academic AI")))

data_with_positions <- data_summary_frac %>%
  group_by(company, institution_group) %>%
  arrange(company, desc(safety_label)) %>%
  mutate(
    ypos = cumsum(fraction_count) - 0.5 * fraction_count
  ) %>%
  ungroup()

# Then filter this dataset for the text
text_data <- data_with_positions %>% 
  filter(fraction_count >= 10)

# Now use your original plot but with specific positioning for text
ggplot(data_summary_frac, aes(
  x = company,
  y = (fraction_count),
  fill = safety_label
)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +
  geom_text(
    data = text_data,
    aes(y = ypos, label = round(fraction_count, 0)),
    color = "black",
    size = 2.8
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "",
    x = "",
    y = "Number of Papers",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 0.9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right",
    #legend.direction = "horizontal",
    legend.justification = "center"
    #legend.box.margin = margin(b = -10)
  ) +
  facet_wrap(~institution_group, scales = "free_x") # 6 by 10 landscape

### Citation graph
# First, create a dataset with the position information
# Reorder the institution_group factor to put "Corporate AI" first

name_replacements <- c(
  "University of Washington" = "U. of Washington",
  "New York University" = "NYU"
)

data_summary_company_citeimpact <- data %>%
  filter(!is.na(institution_group)) %>%
  filter(not_ai_safety == FALSE) %>%
  group_by(institution_name, institution_group, safety_label) %>%
  summarise(
    citation_weighted_count = sum(fraction * cited_by_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(institution_name) %>%
  mutate(company_total = sum(citation_weighted_count)) %>%
  ungroup() %>%
  mutate(company = fct_reorder(institution_name, -company_total))

data_summary_company_citeimpact <- data_summary_company_citeimpact %>%
  mutate(institution_group = factor(institution_group, 
                                    levels = c("Corporate AI", "Academic AI")))

data_summary_company_citeimpact <- data_summary_company_citeimpact %>%
  mutate(
    # Create a display name for plotting that shortens the long names
    display_name = case_when(
      institution_name %in% names(name_replacements) ~ name_replacements[institution_name],
      TRUE ~ institution_name
    ),
    # Create a factor with the new display name but maintain the same ordering
    company = fct_reorder(display_name, -company_total)
  )

data_with_positions <- data_summary_company_citeimpact %>%
  group_by(company, institution_group) %>%
  arrange(company, desc(safety_label)) %>%  # Order matters for stacking
  mutate(
    ypos = cumsum(citation_weighted_count) - 0.5 * citation_weighted_count  # Calculate center positions
  ) %>%
  ungroup()

# Then filter this dataset for the text
text_data <- data_with_positions %>% 
  filter(citation_weighted_count >= 150)  # Only show text for values >= 50

# Now use your original plot but with specific positioning for text
ggplot(data_summary_company_citeimpact, aes(
  x = company,
  y = citation_weighted_count,
  fill = safety_label
)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +
  # Use the pre-calculated positions
  geom_text(
    data = text_data,
    aes(y = ypos, label = round(citation_weighted_count, 0)),
    color = "black",
    size = 3
  ) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "",
    x = "",
    y = "Total Citations",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 0.9, vjust = 1),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "right",
    #legend.direction = "horizontal",
    legend.justification = "center"
    #legend.box.margin = margin(b = -10)
  ) +
  facet_wrap(~institution_group, scales = "free_x") # 6 by 10 landscape


combined_plot2 <- plot_grid(p1, p2, nrow=2)
print(combined_plot2)


### OLD
data_summary <- data %>%
  group_by(institution_display_name, safety_classification) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(company_total = sum(count)) %>%
  ungroup() %>%
  arrange(desc(company_total)) %>%
  #mutate(company = factor(company, levels = unique(company))) %>%
  mutate(company = fct_reorder(institution_display_name, -company_total))

data$dummy_year <- ifelse(data$year > 2022, 1, 0)
data$dummy_year <- ifelse(data$year > 2022, "2023 & 2024", "2020 - 2022")
data_summary_years <- data %>%
  group_by(company, dummy_year, classification) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(company_total = sum(count)) %>%
  ungroup() %>%
  arrange(desc(company_total)) %>%
  #mutate(company = factor(company, levels = unique(company))) %>%
  mutate(company = fct_reorder(company, -company_total))

data_summary_cite <- data %>%
  group_by(company, classification) %>% 
  summarise(
    paper_count = n(),  # Total number of papers
    total_citations = sum(citation_count, na.rm = TRUE),  # Total citations for the group
    .groups = "drop") %>% 
  mutate(citation_weighted_count = total_citations * paper_count)

data_summary_citeimpact <- data %>%
  group_by(company, classification) %>% 
  summarise(
    paper_count = n(),  # Total number of papers
    total_citations = sum(citation_count, na.rm = TRUE),  # Total citations for the group
    .groups = "drop") %>% 
  mutate(citation_weighted_count = total_citations / paper_count)

ggplot(data_summary, aes(x = reorder(company, -count, FUN = sum),  # sum 'count' by company
                         y = log2(count), fill = classification)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +  # Add thin black outlines
  geom_text(
    aes(label = count), 
    position = position_stack(vjust = 0.5), 
    color = "black", 
    size = 3.5
  ) +  # Add text labels inside the bars
  scale_fill_brewer(palette = "Set3") +  # Use a distinct color palette
  labs(
    title = "",
    x = "",
    y = "Number of Papers   (Log2 Scale)",
    fill=""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size =12),
    legend.text = element_text(size = 9)
  )

ggplot(data_summary_citeimpact, aes(x = reorder(company, -citation_weighted_count , FUN = sum),  # sum 'count' by company
                                    y = (citation_weighted_count ), fill = classification)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +  # Add thin black outlines
  geom_text(
    aes(label = round(citation_weighted_count,0)), 
    position = position_stack(vjust = 0.5), 
    color = "black", 
    size = 2.5
  ) +  # Add text labels inside the bars
  scale_fill_brewer(palette = "Set3") +  # Use a distinct color palette
  labs(
    title = "",
    x = "",
    y = "Average Citations per Paper",
    fill=""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size =12),
    legend.text = element_text(size = 9)
  )


ggplot(data_summary_years, 
       aes(x = reorder(company, -count, FUN = sum),  # sum 'count' by company
           y = log2(count), fill = classification)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +  # Add thin black outlines
  facet_wrap(~ dummy_year, scales="free")+
  geom_text(
    aes(label = count), 
    position = position_stack(vjust = 0.5), 
    color = "black", 
    size = 1.5
  ) +  # Add text labels inside the bars
  scale_fill_brewer(palette = "Set3") +  # Use a distinct color palette
  labs(
    title = "",
    x = "",
    y = "",
    fill=""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size =8) 
  )


ggplot(data_summary_cite, aes(x = reorder(company, -citation_weighted_count , FUN = sum),  # sum 'count' by company
                              y = (citation_weighted_count ), fill = classification)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +  # Add thin black outlines
  # Add text labels inside the bars
  scale_fill_brewer(palette = "Set3") +  # Use a distinct color palette
  labs(
    title = "",
    x = "",
    y = "Total Impact (x citation)",
    fill=""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size =12),
    axis.title.y = element_text(size = 12)
  )

#### ~~~ diversity and uniformity: is there more concentration and less diversity? Not really... ~~~ #### 
library(dplyr)
library(ggplot2)
library(tidytext)   # for text tokenization
library(textstem) 
library(tidyverse)
library(DescTools)
library(hhi) 

library(dplyr)
library(DescTools) # for Gini()

topic_diversity <- data %>%
  group_by(institution_group, safety_classification) %>%
  summarize(
    count = n(),          
    .groups = "drop"
  ) %>%
  group_by(institution_group) %>% # Now group only by institution_group so we can get proportions within each group
  mutate(prop = count / sum(count)) %>%
  summarize(
    shannon_diversity       = -sum(prop * log(prop)),
    variance_of_proportions = var(prop),
    gini                    = Gini(prop),
    hhi                     = sum(prop^2),
    .groups = "drop"
  )



topic_diversity_t <- data %>%
  group_by(institution_group, publication_year, safety_classification) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(institution_group, safety_classification,publication_year) %>%
  mutate(prop = n / sum(n)) %>%
  summarize(
    # Shannon's diversity index
    shannon_diversity = -sum(prop * log(prop)), #if decreasing over time then more uniformity
    # Alternatively, you might compute the variance of topic proportions
    variance_of_proportions = var(prop),
    gini = Gini(prop),
    hhi = sum(prop^2)           # Sum of squared shares
  )#one topic has all the proportions


# Suppose df has 'year' and 'abstract' columns
# 1) Unnest tokens
# 2) Remove stop words
# 3) Lemmatize
# 4) Remove short tokens or purely numeric tokens

data("stop_words")  # from tidytext

df_tokens <- data %>%
  # Unnest tokens from the abstract
  unnest_tokens(output = "word", 
                input = "abstract", 
                token = "words",
                to_lower = TRUE, 
                drop = TRUE) %>%
  # Remove English stop words
  anti_join(stop_words, by = "word") %>%
  # Remove purely numeric tokens
  filter(!grepl("^[0-9]+$", word)) %>%
  # Lemmatize each token
  mutate(word = lemmatize_words(word))

# Glimpse the tokenized data
head(df_tokens)

# Compute Type-Token Ratio (TTR) by year
lexical_diversity <- df_tokens %>%
  group_by(year) %>%
  summarize(
    total_words = n(),
    unique_words = n_distinct(word),
    TTR = unique_words / total_words
  )

lexical_diversity

# Plot TTR over time
ggplot(lexical_diversity, aes(x = year, y = TTR)) +
  geom_line() +
  geom_point() +
  labs(title = "Lexical Diversity (Type-Token Ratio) Over Time",
       x = "Year", y = "TTR")


# If you want Shannon diversity for words
shannon_lexical <- df_tokens %>%
  group_by(year, word) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(prop = count / sum(count)) %>%
  summarise(shannon_words = -sum(prop * log(prop))) 

shannon_lexical

# Suppose you want to see if TTR changes are entirely due to "year" trend:
model <- lm(TTR ~ year, data = lexical_diversity)
summary(model)

# Get the residuals, which might reflect "year-adjusted" TTR
lexical_diversity$TTR_resid <- resid(model)

# Inspect or plot the residual TTR
ggplot(lexical_diversity, aes(x = year, y = TTR_resid)) +
  geom_line() +
  geom_point() +
  labs(title = "Year-Adjusted Lexical Diversity (Residual TTR)",
       x = "Year", y = "Residual TTR")






