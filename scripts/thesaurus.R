# Load required libraries
library(tidyverse)

# Read the institution list CSV
institutions <- read_csv("outputs/institutions_list.csv")

# Read the organization data
org_data <- read_delim("outputs/org.txt", delim = "\t", quote = "\"")

# Function to standardize institution names
standardize_name <- function(name) {
  name <- toupper(name)
  # Remove extra spaces
  name <- str_squish(name)
  # Remove leading/trailing quotes
  name <- str_remove_all(name, "^\"|\"$")
  return(name)
}

# Standardize both datasets
institutions$AFF_STD <- standardize_name(institutions$AFF)
org_data$organization_std <- standardize_name(org_data$organization)

# Create matching rules - find similar patterns
create_thesaurus <- function(inst_df, org_df) {
  
  thesaurus <- data.frame(
    label = character(),
    replace_by = character(),
    stringsAsFactors = FALSE
  )
  
  # Exact matches first
  for (i in 1:nrow(org_df)) {
    org_name <- org_df$organization_std[i]
    org_original <- org_df$organization[i]
    
    # Check if this organization name appears in institutions list
    matches <- inst_df %>%
      filter(str_detect(AFF_STD, fixed(org_name)) | 
             str_detect(org_name, fixed(AFF_STD)))
    
    if (nrow(matches) > 0) {
      # Use the most frequent match as the standard
      best_match <- matches %>%
        arrange(desc(Freq)) %>%
        slice(1)
      
      thesaurus <- bind_rows(thesaurus, 
                            data.frame(label = org_original,
                                     replace_by = best_match$AFF))
    }
  }
  
  # Remove duplicates, keep first occurrence
  thesaurus <- thesaurus %>%
    distinct(label, .keep_all = TRUE)
  
  return(thesaurus)
}

# Generate thesaurus
thesaurus <- create_thesaurus(institutions, org_data)

# Write to tab-delimited file
write_delim(thesaurus, 
            "institution_thesaurus.txt", 
            delim = "\t",
            quote = "none",
            col_names = TRUE)

# Also save as CSV for easier viewing
write_csv(thesaurus, "institution_thesaurus.csv")

# Print summary
cat("Thesaurus generated successfully!\n")
cat("Total mappings:", nrow(thesaurus), "\n")
cat("Files saved:\n")
cat("  - institution_thesaurus.txt (tab-delimited)\n")
cat("  - institution_thesaurus.csv (comma-delimited)\n")

# Preview first few entries
head(thesaurus, 10)