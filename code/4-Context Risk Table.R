### CONTEXT AND RISKS TABLE ###
setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version")
data <- read.csv("data_merged_classified_deduplicated_FINAL.csv", stringsAsFactors = FALSE)

#Descriptive table 1
data %>% group_by(institution_group, not_ai_safety) %>% 
  summarize(papers = n())

data %>% group_by(institution_group, not_ai_safety) %>% 
  summarize(papersfrac = sum(fraction))

# Load required libraries
library(dplyr)
library(tidyr)
library(xtable) # For LaTeX table output

# Assuming your data is in a data frame called 'data'
# If you need to load it:
# data <- read.csv("your_data_file.csv")

# Clean data: convert NA in text columns to empty strings
data <- data %>%
  mutate(across(c(ab, title_clean), ~ replace_na(.x, "")))

#testing if evals is pre or post deployment with classifcations from Claude and o3.
#testing_evals <- data %>% filter(safety_classification=="testing_and_evaluation") %>%
 # select(ab, title_clean, institution_group)
#write.csv(testing_evals, "testing_evals.csv")
#GPT o3: https://chatgpt.com/share/680279a5-f6f4-800f-85ec-2dd9f39f1ab6
# real‑world telemetry, user‑study, or live‑monitoring work (70, post-deployment)
# pre‑deployment (mostly benchmark‑only or lab‑style safety tests), 307,
# uncertain: 56
#Claude: https://claude.site/artifacts/b3eac8da-33ab-44f9-9710-192f4c3c46db
# if push it on uncertain classification just puts them in pre-deployment
# but either way post-deployment is not more than 35\% and evenly split between academic and corporate AI

data %>%
  filter(not_ai_safety == FALSE) %>%
  arrange(desc(cited_by_count)) %>%
  select(cited_by_count,  institution_name) %>%
  slice_head(n = 10)



# Define function to filter data by risk area
# make sure to pass perl=TRUE to grepl()
filter_risk_area <- function(data, pattern, safety_filter = NULL) {
  filtered_data <- data %>%
    filter(
      grepl(pattern, ab, ignore.case = TRUE, perl = TRUE) |
        grepl(pattern, title_clean, ignore.case = TRUE, perl = TRUE)
    )
  if (!is.null(safety_filter)) {
    filtered_data <- filtered_data %>%
      filter(not_ai_safety == safety_filter)
  }
  filtered_data
}

risk_areas <- list(
  Disclosure = paste0(
    "\\bmodel card(?:s)?\\b|",        
    "\\bdata card(?:s)?\\b|",         
    "audit(?:s|ing)?\\b|",            
    "\\bmodel standard(?:s)?\\b|",    
    "\\bevaluation standard(?:s)?\\b|",
    "\\btesting standard(?:s)?\\b"
  ),
  Medical = paste0(
    "\\bhospital(?:s)?\\b|",         
    "\\bhealth insurance(?:s)?\\b|", 
    "\\bclinician(?:s)?\\b"
  ),
  Commercial = paste0(
    "\\badvert(?:|s|isement|isements)\\b|",
    "\\bmarketing\\b|",
    "\\bhiring\\b|",
    "\\brecruiting\\b"
  ),
  Misinfo = paste0(
    "\\bspam\\b|",
    "\\bphishing\\b|",
    "\\bdisinformation\\b|",
    "\\bmisinformation\\b"
  ),
  Finance = paste0(
    "\\bfinance(?:d)?\\b|",
    "\\bfinancial\\b"
  ),
  Behavioral = paste0(
    "\\bsycophant(?:ic|s)?\\b|",
    "\\bpersuasion(?:s)?\\b|",
    "\\bpersuasive\\b|",
    "\\breward[- ]hacking\\b"
  ),
  Copyright = paste0(
    "\\baccess violation(?:s)?\\b|",  
    "\\bcopyright violat(?:ion|ions)?\\b|",
    "\\bcontent attribution\\b|",
    "\\bdataset licens(?:e|ing|es)?\\b|",
    "\\bdata attribution\\b|",
    "\\bcopyrighted material\\b|",
    "\\bcopyright law\\b|",
    "\\bC2PA\\b|",
    "\\bContent Authenticity Initiative\\b"
  ),
  Accuracy = paste0(
    "\\bhallucination(?:s)?\\b|",
    "\\bcoding errors?\\b|",
    "\\bcoding inaccuracy(?:s)?\\b|",
    "\\bfactual inaccuracy(?:s)?\\b|",
    "\\bfactual errors?\\b"
  )
)



# Calculate proportion of AI safety papers
ai_safety_prop <- mean(!data$not_ai_safety, na.rm = TRUE)
print(paste("Proportion of AI Safety papers:", ai_safety_prop))

# Create data frames to store results
all_fractions <- data.frame(
  risk_area = names(risk_areas),
  Academic_AI = numeric(length(risk_areas)),
  Corporate_AI = numeric(length(risk_areas))
)

all_citations <- data.frame(
  risk_area = names(risk_areas),
  Academic_AI = numeric(length(risk_areas)),
  Corporate_AI = numeric(length(risk_areas))
)

safety_fractions <- data.frame(
  risk_area = names(risk_areas),
  Academic_AI = numeric(length(risk_areas)),
  Corporate_AI = numeric(length(risk_areas))
)

safety_citations <- data.frame(
  risk_area = names(risk_areas),
  Academic_AI = numeric(length(risk_areas)),
  Corporate_AI = numeric(length(risk_areas))
)

# Process data for all papers
for (i in 1:length(risk_areas)) {
  name <- names(risk_areas)[i]
  pattern <- risk_areas[[i]]
  filtered <- filter_risk_area(data, pattern)
  
  # Calculate metrics by institution group
  metrics <- filtered %>%
    group_by(institution_group) %>%
    summarize(
      paper_fraction = sum(fraction, na.rm = TRUE),
      citations = sum(cited_by_count*fraction, na.rm = TRUE)
    )
  
  # Print debug info
  print(paste("Risk area:", name, "- Number of papers:", nrow(filtered)))
  print(metrics)
  
  # Add results to data frames
  for (group in c("Academic AI", "Corporate AI")) {
    group_metrics <- metrics %>% filter(institution_group == group)
    
    if (nrow(group_metrics) > 0) {
      # Use the proper column names for data frame
      group_col <- gsub(" ", "_", group)
      all_fractions[i, group_col] <- group_metrics$paper_fraction
      all_citations[i, group_col] <- group_metrics$citations
    }
  }
}

# Process data for AI safety papers
for (i in 1:length(risk_areas)) {
  name <- names(risk_areas)[i]
  pattern <- risk_areas[[i]]
  filtered <- filter_risk_area(data, pattern, safety_filter = FALSE)
  
  # Calculate metrics by institution group
  metrics <- filtered %>%
    group_by(institution_group) %>%
    summarize(
      paper_fraction = sum(fraction, na.rm = TRUE),
      citations = sum(cited_by_count*fraction, na.rm = TRUE)
    )
  
  # Print debug info
  print(paste("Safety papers risk area:", name, "- Number of papers:", nrow(filtered)))
  print(metrics)
  
  # Add results to data frames
  for (group in c("Academic AI", "Corporate AI")) {
    group_metrics <- metrics %>% filter(institution_group == group)
    
    if (nrow(group_metrics) > 0) {
      # Use the proper column names for data frame
      group_col <- gsub(" ", "_", group)
      safety_fractions[i, group_col] <- group_metrics$paper_fraction
      safety_citations[i, group_col] <- group_metrics$citations
    }
  }
}

# Calculate safety percentage for each risk area
all_fractions$Safety_Percent <- sapply(1:nrow(all_fractions), function(i) {
  total_papers <- all_fractions$Academic_AI[i] + all_fractions$Corporate_AI[i]
  safety_papers <- safety_fractions$Academic_AI[i] + safety_fractions$Corporate_AI[i]
  
  if (total_papers > 0) {
    return(100 * (safety_papers / total_papers))
  } else {
    return(0)
  }
})


###first ordering before generating the table###
total_papers <- all_fractions$Academic_AI + all_fractions$Corporate_AI
ord <- order(total_papers, decreasing = TRUE)
all_fractions_ord   <- all_fractions[ord, ]
all_citations_ord   <- all_citations[ord, ]
safety_fractions_ord <- safety_fractions[ord, ]
safety_citations_ord <- safety_citations[ord, ]
all_fractions_ord$Safety_Percent <- with(all_fractions_ord,
                                         100 * (safety_fractions_ord$Academic_AI + safety_fractions_ord$Corporate_AI) /
                                           (Academic_AI + Corporate_AI)
)

# Generate LaTeX for the combined table
generate_combined_table <- function(fractions_df, citations_df, caption) {
  # Combine the tables
  latex <- paste0("\\begin{table}[htbp]\n",
                  "\\centering\n",
                  "\\caption{", caption, "}\n",
                  "\\begin{tabular}{l|cc|cc|c}\n",
                  "\\hline\n",
                  "& \\multicolumn{2}{c|}{By Paper Fraction} & \\multicolumn{2}{c|}{By Citations} & \\\\\n",
                  "Risk Area & Academic AI & Corporate AI & Academic AI & Corporate AI & \\% Safety \\\\\n",
                  "\\hline\n")
  
  # Add each risk area row
  for (i in 1:nrow(fractions_df)) {
    risk_area <- fractions_df$risk_area[i]
    acad_frac <- sprintf("%.0f", fractions_df$Academic_AI[i])
    corp_frac <- sprintf("%.0f", fractions_df$Corporate_AI[i])
    acad_cite <- sprintf("%.0f", citations_df$Academic_AI[i])
    corp_cite <- sprintf("%.0f", citations_df$Corporate_AI[i])
    safety_pct <- sprintf("%.0f\\%%", fractions_df$Safety_Percent[i])
    
    latex <- paste0(latex,
                    risk_area, " & ", 
                    acad_frac, " & ", 
                    corp_frac, " & ", 
                    acad_cite, " & ", 
                    corp_cite, " & ",
                    safety_pct, " \\\\\n")
  }
  
  # Close the table
  latex <- paste0(latex,
                  "\\hline\n",
                  "\\end{tabular}\n",
                  "\\end{table}")
  
  return(latex)
}

# Generate combined table
combined_table <- generate_combined_table(
  all_fractions, 
  all_citations, 
  "Paper Distribution by Risk Area, Institution, and Safety Percentage"
)

# Print the table
cat("Combined Table:\n\n")
cat(combined_table)



###Ordered Table####

