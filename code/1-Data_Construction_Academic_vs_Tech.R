library(openalexR)
library(dplyr)
library(ggplot2)

# Set your contact email
options(openalexR.mailto = "xxx")

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


####### LLLM FOCUS ######
####### LLLM FOCUS ######

from_year <- 2020
to_year <- 2025

# Step 1: get LLM subsample using keywords in the filter
fetch_llm_publications <- function(ror_id, from_year, to_year) {
  tryCatch({
    Sys.sleep(1)
    
    message(paste("Fetching LLM papers for institution:", ror_id))
    
    publications <- oa_fetch(
      entity = "works",
      `authorships.institutions.ror` = ror_id,
      `from_publication_date` = paste0(from_year, "-01-01"),
      `to_publication_date` = paste0(to_year, "-03-31"),
      
      search = "\"language model*\" OR \"large language model*\" OR \"LLM*\" OR \"GPT\" 
       OR \"BERT\" OR \"transformer\" OR \"generative model*\" OR \"foundation model*\"",
      
      options = list(mailto = "ilanstrauss@gmail.com"),
      
      verbose = TRUE
    )
    
    if (!is.null(publications)) {
      # Add ror_id as a simple column
      publications$ror_id <- ror_id
      
      message(paste("Found", nrow(publications), "LLM-related publications"))
    }
    
    return(publications)
  }, error = function(e) {
    warning(paste("Failed to fetch data for ROR ID:", ror_id, "-", e$message))
    return(NULL)
  })
}


all_llm_research <- lapply(ror_ids, function(ror_id) {
  fetch_llm_publications(ror_id, from_year, to_year)
})


all_llm_research_df <- bind_rows(all_llm_research)
dim(all_llm_research_df)

# Add institution names
all_llm_research_df <- all_llm_research_df %>%
  left_join(institution_map, by = "ror_id")

all_llm_research_df <- all_llm_research_df  %>%
  mutate(
    institution_group = case_when(
      institution_name %in% c("CMU", "Stanford", "University of Washington", 
                              "UC Berkeley", "MIT", "New York University"
                              ) ~ "Academic AI",
      !is.na(institution_name) ~ "Corporate AI")
  ) %>%
  filter(!is.na(institution_group))


all_llm_research_df <- all_llm_research_df %>%
filter(type!="erratum") %>%
  filter(type!="paratext") %>%
  filter(type!="retraction") %>%
  filter(type!="peer-review") %>%
  filter(type!="report") %>%
  filter(type!="letter") %>%
  filter(type!="editorial") %>%
  filter(type!="libguides") %>%
  filter(type!="review") %>%
  filter(type!="supplementary-materials")

table(all_llm_research_df$type)

# Step 2: Identify publications with missing abstracts
missing_abstract <- is.na(all_llm_research_df$ab) | all_llm_research_df$ab == ""

table(missing_abstract)

# Step 3: Apply the abstract scraping code to those publications
# Enhanced R script for retrieving missing abstracts from OpenAlex data
# Based on the Python script provided

# Install and load required packages
required_packages <- c("httr", "rvest", "jsonlite", "dplyr", "stringr", "purrr", "tidyr")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# First, load all the function definitions from above (get_abstract_from_doi, 
# get_doi_from_openalex, get_springer_abstract, extract_abstract_from_web)

# Define API keys
springer_api_key <- "5adfc218680a6f071444e9b9f2a49079"

# Define publisher lists
springer_list <- c("Springer Science+Business Media", "Springer Nature", "Springer International Publishing")
nature_list <- c("Nature Portfolio") 
elsevier_list <- c("Elsevier BV")

# Identify missing abstracts
missing_abstract <- is.na(all_llm_research_df$ab) | all_llm_research_df$ab == ""
table(missing_abstract)
missing_indices <- which(missing_abstract)

# Process in small batches to avoid overwhelming servers
batch_size <- 10
abstract_count <- 0

for (i in seq(1, length(missing_indices), by = batch_size)) {
  batch_end <- min(i + batch_size - 1, length(missing_indices))
  batch_indices <- missing_indices[i:batch_end]
  
  message(paste("Processing batch", ceiling(i/batch_size), 
                "of", ceiling(length(missing_indices)/batch_size),
                paste0("(", i, "-", batch_end, " of ", length(missing_indices), ")"))
  )
  
  # Process each paper in the batch
  for (idx in batch_indices) {
    paper_id <- all_llm_research_df$id[idx]
    message(paste("Processing paper:", paper_id))
    
    # Step 1: If DOI is missing, try to get it from OpenAlex
    if((is.na(all_llm_research_df$doi[idx]) || all_llm_research_df$doi[idx] == "") && 
       !is.na(all_llm_research_df$id[idx])) {
      message("  - Getting DOI from OpenAlex")
      all_llm_research_df$doi[idx] <- get_doi_from_openalex(all_llm_research_df$id[idx])
      Sys.sleep(0.5)  # Brief delay
    }
    
    # Step 2: Try to get abstract using DOI via Crossref
    if(!is.na(all_llm_research_df$doi[idx]) && all_llm_research_df$doi[idx] != "" && 
       (is.na(all_llm_research_df$ab[idx]) || all_llm_research_df$ab[idx] == "")) {
      message("  - Getting abstract from Crossref")
      all_llm_research_df$ab[idx] <- get_abstract_from_doi(all_llm_research_df$doi[idx])
      Sys.sleep(0.5)  # Brief delay
    }
    
    # Step 3: Try publisher-specific methods if abstract still missing
    if(is.na(all_llm_research_df$ab[idx]) || all_llm_research_df$ab[idx] == "") {
      host <- all_llm_research_df$host_organization[idx]
      
      # For Springer papers
      if(!is.na(host) && host %in% springer_list && 
         !is.na(all_llm_research_df$doi[idx]) && all_llm_research_df$doi[idx] != "") {
        message("  - Getting abstract from Springer API")
        all_llm_research_df$ab[idx] <- get_springer_abstract(all_llm_research_df$doi[idx], springer_api_key)
        Sys.sleep(0.5)
      }
      
      # For any papers, try web scraping if URL is available
      if((is.na(all_llm_research_df$ab[idx]) || all_llm_research_df$ab[idx] == "") && 
         !is.na(all_llm_research_df$url[idx]) && all_llm_research_df$url[idx] != "") {
        message("  - Getting abstract by scraping website")
        all_llm_research_df$ab[idx] <- extract_abstract_from_web(all_llm_research_df$url[idx])
        Sys.sleep(0.5)
      }
    }
    
    # Check if we got an abstract
    if(!is.na(all_llm_research_df$ab[idx]) && all_llm_research_df$ab[idx] != "") {
      abstract_count <- abstract_count + 1
    }
  }
  
  # Save progress periodically
  if(i %% (batch_size * 5) == 0 || i >= length(missing_indices)) {
    temp_output <- paste0("all_llm_research_df_with_abstracts_temp_", i, ".csv")
    message(paste("Saving progress to:", temp_output))
    write.csv(all_llm_research_df, temp_output, row.names = FALSE)
  }
  
  # Longer delay between batches
  Sys.sleep(2)
}

# Final report
message("==== Abstract Retrieval Summary ====")
message(paste("Initially missing abstracts:", sum(missing_abstract)))
message(paste("Successfully retrieved:", abstract_count))
message(paste("Remaining missing abstracts:", sum(is.na(all_llm_research_df$ab) | all_llm_research_df$ab == "")))
message(paste("Completion rate:", round(abstract_count/sum(missing_abstract)*100, 2), "%"))
table(is.na(all_llm_research_df$ab))


## GET remaining missing abstracts using external code "abstracts-missing - python"

library(jsonlite)
saveRDS(all_llm_research_df, "all_llm_research-missings-ab.rds")

# Write to JSON with pretty formatting and preserving vectors as arrays
write_json(all_llm_research_df, "all_llm_research-missings-ab.json", 
           pretty = TRUE, 
           auto_unbox = FALSE,  # Keep vectors as arrays
           null = "null",       # Preserve NULL values
           na = "null")         # Convert NA to null



library(jsonlite)

# Read the JSON file back into R
setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version")
data <- fromJSON("all_llm_research-fixed-most-ab.json", simplifyDataFrame = TRUE)
dim(data)
table(is.na(data$ab))

datafrac <- data %>%
  mutate(paper_id = id) %>% # Create a paper identifier
  tidyr::unnest(author) %>% # Unnest the authors
  mutate(clean_ror_id = ifelse(     # Extract clean ROR ID from the URL format, handling potential NAs
    !is.na(institution_ror),
    sub(".*/(\\w+)$", "\\1", institution_ror),
    NA_character_
  )) %>%
  group_by(paper_id, au_id) %>% # For each author in each paper, prioritize affiliations in our list
  mutate(
    is_target_institution = clean_ror_id %in% ror_ids #flag if this affiliation is in our list of interest
  ) %>%
  # First sort by whether it's a target institution (TRUE comes before FALSE)
  # Then take the first one (either the first target institution or just the first if none are targets)
  arrange(paper_id, au_id, desc(is_target_institution)) %>%
  slice(1) %>%
  ungroup() %>%
  # Continue with the rest of your analysis
  # Count authors per institution for each paper
  group_by(paper_id, clean_ror_id) %>%
  mutate(
    institution_author_count = n() # Number of authors for each institution in a paper
  ) %>%
  group_by(paper_id) %>%
  mutate(
    total_authors = n_distinct(au_id) # Total unique authors
  ) %>%
  ungroup() %>%
  mutate(
    fraction = institution_author_count / total_authors # Fraction based on number of authors
  ) %>%
  # Select distinct paper-institution combinations
  distinct(paper_id, clean_ror_id, .keep_all = TRUE) %>%
  # Filter to only our institutions (for the final result)
  filter(clean_ror_id %in% ror_ids) %>%
  # Join with institution names for better display
  left_join(institution_map, by = c("clean_ror_id" = "ror_id"))

#The above ensures that each author only has one affiliation to ensure totals work out
high_fraction <- datafrac  %>%
  filter(fraction > 1) %>%    # should never be more than one
  arrange(desc(fraction)) %>%
  select(fraction, title, clean_ror_id, paper_id, institution_author_count, total_authors)
  
#removing duplicated titles
datafrac <- datafrac %>%
filter(type != "dataset")

datafrac  <- datafrac %>%
  # Create a type preference variable (lower number = higher preference)
  mutate(type_preference = case_when(
    type == "article" ~ 1,
    type == "book-chapter" ~ 2,
    type == "preprint" ~ 3,
    TRUE ~ 4  # Any other types
  )) %>%
  # Group by title and keep the most preferred type
  group_by(title) %>%
  arrange(type_preference, paper_id) %>%  # Sort by preference, then paper_id
  slice(1) %>%  # Keep only the first (most preferred) record per title
  ungroup() %>%
  # Remove the helper column
  select(-type_preference)

title_duplicates <- datafrac %>%
  group_by(title) %>%
  filter(n_distinct(paper_id) > 1) %>%  # Only keep titles with multiple paper_ids
  arrange(title, paper_id) %>%  # Sort for easier viewing
  ungroup() %>%
  select(fraction, type, institution_author_count, total_authors, title, clean_ror_id, ror_id, paper_id)
table(title_duplicates$type) #should be empty




### INCLUDING OPENAI and ANTHROPIC DATA ###

setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version/openai-anthropic")
ao_data <- read.csv("anthropic_openai_final.csv", stringsAsFactors = FALSE)
table(ao_data$Year, ao_data$Company)

ao_data <- ao_data %>% rename(
  url = URL,
  ab = Abstract,
  title = Title,  # This remains the same
  cited_by_count = Citation_count,
  publication_year = Year,
  institution_name = Company
) %>%
  mutate(fraction = 1)


ao_data <- ao_data %>%
  mutate(
    ror_id = case_when(
      institution_name == "OpenAI" ~ "05wx9n238",
      institution_name == "Anthropic" ~ "056y0v115",
      TRUE ~ NA_character_  # Handle any other values
    ),
    clean_ror_id = case_when(
      institution_name == "OpenAI" ~ "05wx9n238",
      institution_name == "Anthropic" ~ "056y0v115", 
      TRUE ~ NA_character_  # Handle any other values
    )
  )

common_cols <- intersect(names(datafrac), names(ao_data))
ao_cols <- intersect(names(datafrac), names(ao_data))

# Now bind the rows using only common columns
merged_data <- bind_rows(
  datafrac,
  ao_data %>% select(all_of(ao_cols))
)

merged_data <- merged_data %>%
select(-contains("institution_name"))

merged_data <- merged_data %>%
       left_join(institution_map, by = c("clean_ror_id" = "ror_id"))

# Find duplicates 
title_duplicates <- merged_data %>%
  filter(institution_name == "OpenAI" | institution_name == "Anthropic") %>%
  #filter(n_distinct(paper_id) > 1) %>%  # Only keep titles with multiple paper_ids
  arrange(institution_name, title) %>%  # Sort for easier viewing
  ungroup() %>%
  select(fraction, title, clean_ror_id, ror_id, paper_id, institution_name, institution_author_count, total_authors)
View(title_duplicates)

merged_data <- merged_data %>%
  filter( !(title %in% c("Generative Language Modeling for Automated Theorem Proving", 
                        "Scaling laws for neural language models")))  #they are duplicates

# remove these complex objects
merged_data  <- merged_data  %>% 
  select(-concepts, -topics)

# Find all list columns & remove
list_cols <- sapply(merged_data , is.list)
list_colsnames <- names(merged_data )[list_cols]

merged_data <- merged_data  %>% 
  select(-all_of(list_colsnames))


# Save final result
setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version")
write.csv(merged_data, "papers_complete.csv", row.names = FALSE)

merged_data <- read.csv("papers_complete.csv")


#things still missing
table(is.na(merged_data$cited_by_count))
table(is.na(merged_data$ab))

# Calculate institution metrics with fractional counting for both publications and citations
# Calculate the institution-level metrics first

#time trends


###### SAFETY FOCUS ########
# Define safety and responsible AI keywords

library(purrr)

# More comprehensive approach with word pairs
safety_keywords_with_variants <- list(
  c("safety", "safeties"),
  c("control", "controls", "controlled", "controlling"),
  c("security", "securities"),
  c("privacy", "privacies"),
  c("bias", "biases", "biased"),
  c("fair", "fairness"),
  c("explainable", "explainability"),
  c("interpretable", "interpretability"),
  c("transparent", "transparency"),
  c("governance"),
  c("risk", "risks", "risky"),
  c("mitigate", "mitigation", "mitigating"),
  c("evaluate", "evaluation", "evaluations", "eval", "benchmark", "benchmarks"),
  c("test", "testing", "tests"),
  c("align", "alignment", "alignments"),
  c("ethic", "ethics", "ethical"),
  c("responsible", "responsibility", "responsibilities"),
  c("accountable", "accountability"),
  c("oversight"),
  c("robust", "robustness"),
  c("trustworthy", "trust", "trusted"),
  c("value alignment")
)

# Updated function using the word lists
is_safety_paper <- function(title, ab, min_matches = 1) {
  match_count <- 0
  matched_keyword_groups <- c()
  
  # Check title
  if (!is.na(title)) {
    title_lower <- tolower(title)
    for (i in seq_along(safety_keywords_with_variants)) {
      keyword_group <- safety_keywords_with_variants[[i]]
      for (variant in keyword_group) {
        if (grepl(paste0("\\b", variant, "\\b"), title_lower, ignore.case = TRUE)) {
          match_count <- match_count + 1
          matched_keyword_groups <- c(matched_keyword_groups, i)
          if (match_count >= min_matches) return(TRUE)
          break  # Found a match in this group, move to next group
        }
      }
    }
  }
  
  # Check abstract (ab)
  if (!is.na(ab)) {
    ab_lower <- tolower(ab)
    for (i in seq_along(safety_keywords_with_variants)) {
      if (!(i %in% matched_keyword_groups)) {  # Avoid counting same keyword group twice
        keyword_group <- safety_keywords_with_variants[[i]]
        for (variant in keyword_group) {
          if (grepl(paste0("\\b", variant, "\\b"), ab_lower, ignore.case = TRUE)) {
            match_count <- match_count + 1
            matched_keyword_groups <- c(matched_keyword_groups, i)
            if (match_count >= min_matches) return(TRUE)
            break  # Found a match in this group, move to next group
          }
        }
      }
    }
  }
  
  return(FALSE)
}


# Filter the LLM dataset for safety papers
papers_safety <- merged_data %>%
  mutate(is_safety = map2_lgl(title, ab, is_safety_paper)) 

papers_safety <- papers_safety %>%
  filter(is_safety==TRUE)

dim(papers_safety)
dim(merged_data)

# Save final result
setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version")
write.csv(papers_safety, "papers_complete_safety.csv", row.names = FALSE)




#####################################################################
# COMBINING EVERYTHING INCLUDING CLASSIFIED DATASET FROM GPT.
#### Reload with corrected classifications for safety (GPT + human) and 8 safety classification categories
setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version")
safety_main <- read.csv( "papers_complete_safety.csv")


papers_complete <- read.csv("papers_complete.csv")
dim(papers_complete)


###### Loading up GPT classified safety and 8 safety categories ########
library(readr)
data_classified <- read_delim("classified_dataset_partial.csv", 
                   delim = ",",
                   quote = "\"",
                   escape_backslash = TRUE,
                   col_names = TRUE,
                   trim_ws = TRUE)

data_classified <- read_delim("classified_dataset_partial_key.csv", 
                              delim = ",",
                              quote = "\"",
                              escape_backslash = TRUE,
                              col_names = TRUE,
                              trim_ws = TRUE)

dim(data_classified)

data_classified$safety_classification[309] <- "not_ai_safety"  #corrected a classification error (abstract was spilling over)

# Find rows where the safety_classification matches the text
#matching_rows <- which(data_classified$safety_classification == "focusing on software engineering and deployment challenges. It is not concerned with AI safety categories such as alignment")
#print(matching_rows)
#data_classified[matching_rows, ]

# Get the row number first
#problem_row <- which(data_classified$safety_classification == "focusing on software engineering and deployment challenges. It is not concerned with AI safety categories such as alignment")

# If found, fix that specific row
#if(length(problem_row) > 0) {
  # Extract the incorrectly split content
 # part1 <- data_classified$ab[problem_row]
#  part2 <- data_classified$safety_classification[problem_row]
  
  # Combine them back together for the abstract
 # data_classified$ab[problem_row] <- paste0(part1, " ", part2)
  
  # Set the correct safety_classification (if you know what it should be)
  #data_classified$safety_classification[problem_row] <- "not_ai_safety"  # Replace with the correct value
#}

unique(data_classified$safety_classification)
table(data_classified$safety_classification)

issue_rows <- which(data_classified$safety_classification == "Issues")
View(data_classified[issue_rows,])
data_classified <- data_classified[-issue_rows, ] 
data_classified$safety_classification[issue_rows] <- "not_ai_safety"

## MATCHING ON TITLE FIRST CLEAN

# First, check which rows have invalid UTF-8
invalid_rows <- which(!validUTF8(data_classified$title))
print(paste("Number of rows with invalid UTF-8:", length(invalid_rows)))

# If there are any invalid rows, print the first few
if(length(invalid_rows) > 0) {
  print("First few problematic titles:")
  print(data_classified$title[invalid_rows[1:min(5, length(invalid_rows))]])
}

# Try a safer cleaning approach with stringi
library(stringi)

# Create a safe clean function that won't fail on invalid UTF-8
safe_clean <- function(x) {
  # First fix encoding issues
  x_fixed <- stri_enc_toutf8(x, validate = FALSE)
  # Then normalize and convert to ASCII
  x_clean <- stri_trans_general(x_fixed, "Latin-ASCII")
  # Finally lowercase and trim
  return(tolower(trimws(x_clean)))
}

# Apply to both dataframes
data_classified$title_clean <- sapply(data_classified$title, safe_clean)
safety_main$title_clean <- sapply(safety_main$title, safe_clean)
papers_complete$title_clean <- sapply(papers_complete$title, safe_clean)


# Check if there are still any invalid UTF-8 characters
any_invalid <- any(!validUTF8(data_classified$title_clean))
print(paste("Still have invalid UTF-8:", any_invalid))

# GPT inserted duplicates, which we are now removing
data_classified <- data_classified %>%
  distinct(title_clean, clean_ror_id, .keep_all = TRUE)

## SAFETY MERGE
data_combined_safety <- merge(x = safety_main, 
                            y = data_classified[, c("title_clean", "safety_classification", "reasoning_summary", "clean_ror_id")],
                            by = c("title_clean", "clean_ror_id"), 
                            all.x = TRUE)

summary(data_combined_safety)

dim(data_combined_safety)
dim(papers_complete)

#duplicates due to multiple "types" (e.g.,articles and pre-prints) for the same title and author combination
duplicates_df <- data_combined_safety %>%
  group_by(title_clean, clean_ror_id) %>%
  filter(n() > 1) 

View(duplicates_df)

data_combined_safetydd <- data_combined_safety %>%
  # Create a type preference variable (lower number = higher preference)
  mutate( type_preference = case_when(
    type == "article" ~ 1,
    type == "book-chapter" ~ 2,
    type == "preprint" ~ 3,
    TRUE ~ 4  # Any other types
  )) %>%
  # Group by title and keep the most preferred type
  group_by(title_clean, clean_ror_id) %>%
  arrange(type_preference, title_clean) %>%  # Sort by preference, then paper_id
  slice(1) %>%  # Keep only the first (most preferred) record per title
  ungroup() %>%
  # Remove the helper column
  select(-type_preference)


duplicates_df <- data_combined_safetydd  %>%
  group_by(title_clean, clean_ror_id) %>%
  filter(n() > 1) 


papers_completedd <- papers_complete %>%
  # Create a type preference variable (lower number = higher preference)
  mutate( type_preference = case_when(
    type == "article" ~ 1,
    type == "book-chapter" ~ 2,
    type == "preprint" ~ 3,
    TRUE ~ 4  # Any other types
  )) %>%
  # Group by title and keep the most preferred type
  group_by(title_clean, clean_ror_id) %>%
  arrange(type_preference, title_clean) %>%  # Sort by preference, then paper_id
  slice(1) %>%  # Keep only the first (most preferred) record per title
  ungroup() %>%
  # Remove the helper column
  select(-type_preference)

duplicates_df <- papers_completedd %>%
  group_by(title_clean, clean_ror_id) %>%
  filter(n() > 1) 

dim(data_combined_safetydd)
dim(papers_completedd)

data_combined_final <- data.frame()

data_combined_final$safety_classification <- data_combined_safetydd$safety_classification

### MERGING ALL PAPERS BACK TO MAIN DATASET SAFETY AND NON-SAFETY ######
data_combined_final <- full_join(x = data_combined_safetydd, 
                       y = papers_completedd) #Match on All Shared Columns since no keys were specified in by(.)
                       #by = c("title_clean", "clean_ror_id"), 
                       #all.x = TRUE)


dim(papers_completedd)
dim(data_combined_final)

data_combined_finaldd <- data_combined_final %>%
  # Create a type preference variable (lower number = higher preference)
  mutate( type_preference = case_when(
    type == "article" ~ 1,
    type == "book-chapter" ~ 2,
    type == "preprint" ~ 3,
    TRUE ~ 4  # Any other types
  )) %>%
  # Group by title and keep the most preferred type
  group_by(title_clean, clean_ror_id) %>%
  arrange(type_preference, title_clean) %>%  # Sort by preference, then paper_id
  slice(1) %>%  # Keep only the first (most preferred) record per title
  ungroup() %>%
  # Remove the helper column
  select(-type_preference)


data_combined_finaldd <- data_combined_finaldd %>%
  mutate(safety_classification = replace_na(safety_classification, "not_ai_safety"))

table(data_combined_finaldd$safety_classification=="not_ai_safety")
table(is.na(data_combined_finaldd$safety_classification))


#table(data_combined_final$fraction.x == data_combined_final$fraction.y)

#false_rows <- subset(
 # data_combined_final,
  #fraction.x != fraction.y
#)

#false_rows %>%
 # select(type.x, title_clean, clean_ror_id, fraction.x, fraction.y, 
  #       total_authors.x, institution_author_count.x, total_authors.y, institution_author_count.y) %>%
 #View()


data_combined_final %>%
  filter(clean_ror_id == "05wx9n238") %>%  # OpenAI ROR ID
  arrange(desc(total_authors)) %>%  # Sort by fraction (highest first)
  mutate(check = (institution_author_count/total_authors)) %>%
  select(type, title_clean, clean_ror_id, fraction, check, 
           total_authors, institution_author_count, total_authors, institution_author_count) %>%
    View()
  

#fraction = institution_author_count / total_authors # Fraction based on number of authors
# Select distinct paper-institution combinations
# distinct(paper_id, clean_ror_id, .keep_all = TRUE) %>%
  

data <- data %>%
  mutate(institution_name = str_trim(tolower(institution_name))) %>%
  select(-institution_name) %>%
  left_join(institution_map, by = c("clean_ror_id" = "ror_id")) %>%
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
  mutate(
    institution_group = case_when(
      institution_name %in% c("CMU", "Stanford", "University of Washington", 
                              "UC Berkeley", "MIT", "New York University"
      ) ~ "Academic AI",
      !is.na(institution_name) ~ "Corporate AI")
  ) %>%
  filter(!is.na(institution_group))

data_combined_finaldd <- data


setwd("/Users/Ilan/Desktop/Ilan Folder/2024/NYC AI/Papers/2024/Gaps/research/second version")
write.csv(data_combined_finaldd, "data_merged_classified_deduplicated_FINAL.csv", row.names = FALSE)
