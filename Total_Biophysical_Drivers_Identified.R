# BH 07/05/2026 Total_Biophysical_Drivers_Identified.R
#
# PURPOSE:
#   Quantify biophysical drivers identified in kelp research, with both
#   overall summaries and species-specific breakdowns. Produces counts and
#   percentages of studies examining each driver across three mutually
#   exclusive kelp species categories (Saccharina latissima only,
#   Laminaria hyperborea only, Both species, or None).
#
# KEY FEATURES:
#   - Species categorization: mutually exclusive categories (sac_only,
#     lam_only, both, other_only, none)
#   - Two complementary outputs:
#     * Overall driver summary (total papers per driver)
#     * Species-stratified analysis (counts and percentages per species)
#   - Percentages calculated as: (papers mentioning driver / total papers
#     for that species) × 100
#
# OUTPUTS:
#   1. total_counts_per_driver.csv - Overall driver frequency ranking
#   2. driver_summary_by_species.csv - Species-specific driver analysis
#      with counts, totals, and percentages
#
# USAGE:
#   Place Codebook_Kelp_Long.csv in working directory, then:
#   source("Total_Biophysical_Drivers_Identified.R")
#
# NOTES:
#   - All counts represent distinct studies (Study_no) to avoid
#     double-counting papers that mention multiple drivers
#   - Species categorization prioritizes exact matches (both > individual)
#   - Drivers consolidated from four separate columns (Bio_physical_Driver_001-004)
#   - Percentages sum to >100% per species as papers can report multiple drivers
#   - Useful for identifying primary environmental drivers per species and
#     research focus areas across kelp literature

library(tidyverse)
library(stringr)

# 1. Load the codebook
df <- read_csv("Codebook_Kelp_Long.csv")


# 1a. Create mutually exclusive species category
# ---- ADD SECTION 5: Species categorization ----
species_col_name <- intersect(colnames(df), "Kelp_species_mentioned")
if (length(species_col_name) == 0) {
  species_col_name <- colnames(df)[str_detect(colnames(df), regex("^Kelp_species_mentioned", ignore_case = TRUE))]
  species_col_name <- species_col_name[1]
}
if (is.null(species_col_name) || length(species_col_name) == 0) {
  stop("Kelp_species_mentioned column not found.")
} else {
  cat("Using species column:", species_col_name, "\n")
}

# Normalize species field - handle spaces carefully
df <- df %>% mutate(
  species_raw = if_else(is.na(.data[[species_col_name]]), "", str_squish(as.character(.data[[species_col_name]])))
)

# CHECK FOR "BOTH" FIRST, THEN individual species (priority order matters!)
df <- df %>%
  mutate(
    # Step 1: Check for explicit "Both" mention
    is_both = str_detect(species_raw, regex("^both$|^both\\s|\\sboth$", ignore_case = TRUE)),
    
    # Step 2: If not "both", check for individual species
    sac = if_else(is_both, TRUE, 
                  str_detect(species_raw, regex("saccharina|s\\.?\\s*latissima|laminaria saccharina", ignore_case = TRUE))),
    lam = if_else(is_both, TRUE, 
                  str_detect(species_raw, regex("laminaria|l\\.?\\s*hyperborea|hyperborea", ignore_case = TRUE))),
    
    # Don't double-count Saccharina as Laminaria
    lam = lam & !str_detect(species_raw, regex("saccharina", ignore_case = TRUE)),
    other_species = FALSE
  )

# Create mutually exclusive species category
df <- df %>%
  mutate(
    sac_only = sac & !lam & !is_both,
    lam_only = lam & !sac & !is_both,
    both_species = is_both | (sac & lam),
    other_only = other_species & !sac & !lam & !is_both,
    species_category = case_when(
      both_species ~ "both",
      sac_only ~ "sac_only",
      lam_only ~ "lam_only",
      other_only ~ "other_only",
      TRUE ~ "none"
    )
  )

cat("✓ species_category created\n\n")


# 2. Extract and clean the drivers
driver_summary <- df %>%
  # Pivot the four driver columns into one long column
  pivot_longer(cols = starts_with("Bio_physical_Driver"), 
               values_to = "Driver", 
               names_to = "Driver_col") %>%
  # Remove empty entries
  filter(!is.na(Driver), Driver != "") %>%
  mutate(
    # Clean up whitespace
    Driver = str_trim(Driver) %>% str_squish(),
    # Consolidate Nutrients (fixes spelling differences)
    Driver = if_else(str_detect(Driver, "(?i)Nutrient"), "Nutrient availability", Driver)
  ) %>%
  # Count unique studies per driver
  group_by(Driver) %>%
  summarise(Total_Papers = n_distinct(Study_no)) %>%
  # Sort from highest to lowest
  arrange(desc(Total_Papers))

# 3. View the results
print(driver_summary)

# Optional: Save to CSV for Excel
write_csv(driver_summary, "total_counts_per_driver.csv")

# ---- NEW: Species-specific driver analysis ----

# ---- Species-specific driver analysis (CORRECTED) ----

# First, get total unique papers per species
species_totals <- df %>%
  group_by(species_category) %>%
  summarise(total_unique_papers = n_distinct(Study_no), .groups = "drop")

# Then calculate driver percentages
driver_by_species <- df %>%
  # Pivot drivers to long format
  pivot_longer(cols = starts_with("Bio_physical_Driver"), 
               values_to = "Driver", 
               names_to = "Driver_col") %>%
  # Remove empty entries
  filter(!is.na(Driver), Driver != "") %>%
  mutate(
    # Clean up whitespace
    Driver = str_trim(Driver) %>% str_squish(),
    # Consolidate Nutrients
    Driver = if_else(str_detect(Driver, "(?i)Nutrient"), "Nutrient availability", Driver)
  ) %>%
  # Count unique studies per driver AND species category
  group_by(species_category, Driver) %>%
  summarise(n_papers = n_distinct(Study_no), .groups = "drop") %>%
  # Join with total papers per species
  left_join(species_totals, by = "species_category") %>%
  # Calculate percentage of TOTAL PAPERS for that species
  mutate(
    percentage = round((n_papers / total_unique_papers) * 100, 1)
  ) %>%
  arrange(species_category, desc(n_papers)) %>%
  select(species_category, Driver, n_papers, total_unique_papers, percentage)

# View results
cat("\n=== DRIVER ANALYSIS BY SPECIES (% of unique papers) ===\n")
print(driver_by_species)

# Save to CSV
write_csv(driver_by_species, "driver_summary_by_species.csv")
cat("\nSaved driver_summary_by_species.csv\n")

