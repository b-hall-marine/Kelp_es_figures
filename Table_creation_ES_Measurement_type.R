# BH 07/05/2026  Measurement_Type_Summary_Tables.R
#
# PURPOSE:
#   Generate summary tables quantifying the use of different
#   ES measurement methodologies across kelp species groups and ecosystem
#   services. Produces seven complementary outputs in both wide and long
#   formats to support results reporting and data exploration.
#
# KEY FEATURES:
#   - Data reshaped from wide to long format with standardized categories
#   - Seven output tables with different grouping/aggregation perspectives
#   - Measurement type consolidation
#   - Summary statistics and ranking of most-used methodologies
#   - Long and wide format options for flexibility in reporting
#
# OUTPUTS:
#   1. measurement_type_by_species.csv - Species comparison matrix
#   2. measurement_type_by_service.csv - Ecosystem service comparison matrix
#   3. measurement_type_by_species_and_service_long.csv - Long-format detail
#   4. measurement_type_species_service_matrix.csv - Combined wide format
#   5. measurement_type_summary_stats.csv - Overall aggregate statistics
#   6. top_measurement_types_overall.csv - Ranked measurement types (%)
#   7. ES_measurement_type_heatmap_data.csv - Raw heatmap data
#
# USAGE:
#   Place Codebook_Kelp_Long.csv in working directory, then:
#   source("Measurement_Type_Summary_Tables.R")
#
# NOTES:
#   - All counts represent distinct studies (Study_no) to avoid
#     double-counting multi-service papers
#   - Percentages calculated as: (study_count / total_studies) × 100
#   - Empty cells in wide format indicate zero studies for that combination
#   - Ideal for extracting specific statistics for methods/results sections

library(tidyverse)
library(stringr)

# 1. Load Data
df <- read_csv("Codebook_Kelp_Long.csv")

# 2. Reshape and Clean (same as before)
df_long <- df %>%
  pivot_longer(cols = starts_with("ES_Name"), values_to = "ES", names_to = "ES_col") %>%
  pivot_longer(cols = starts_with("ES_Measurement_type"), values_to = "Measurement_Type", names_to = "Measurement_col") %>%
  filter(!is.na(ES), !is.na(Measurement_Type), ES != "", Measurement_Type != "") %>%
  mutate(
    ES = str_trim(ES) %>% str_squish(),
    Measurement_Type = str_trim(Measurement_Type) %>% str_squish(),
    Species_Group = case_when(
      str_detect(`Kelp_species_mentioned`, regex("both", ignore_case = TRUE)) ~ "Both",
      str_detect(`Kelp_species_mentioned`, regex("saccharina|latissima|sac lat", ignore_case = TRUE)) ~ "Saccharina latissima",
      str_detect(`Kelp_species_mentioned`, regex("laminaria|hyperborea", ignore_case = TRUE)) ~ "Laminaria hyperborea",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Species_Group != "Other")

# 3. Spelling Corrections & Consolidation (same as before)
df_cleaned <- df_long %>%
  mutate(
    ES = case_when(
      str_detect(ES, "(?i)Primary Productivity") ~ "Primary Productivity",
      str_detect(ES, "(?i)Nutrient filtration") ~ "Nutrient filtration",
      TRUE ~ ES
    ),
    Measurement_Type = case_when(
      str_detect(Measurement_Type, "(?i)Biomass") ~ "Biomass or Harvest",
      str_detect(Measurement_Type, "(?i)Chemical") ~ "Chemical studies or product",
      str_detect(Measurement_Type, "(?i)Water Quality") ~ "Water Quality",
      str_detect(Measurement_Type, "(?i)Associated Fisheries") ~ "Associated Fisheries",
      str_detect(Measurement_Type, "(?i)Coastal Protection") ~ "Coastal Protection",
      str_detect(Measurement_Type, "(?i)Carbon") ~ "Carbon Sequestration",
      TRUE ~ Measurement_Type
    )
  )

# ---- TABLE 1: Measurement Type by Species ----
measurement_by_species <- df_cleaned %>%
  group_by(Species_Group, Measurement_Type) %>%
  summarise(n_studies = n_distinct(Study_no), .groups = "drop") %>%
  pivot_wider(names_from = Measurement_Type, values_from = n_studies, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(Total))

write_csv(measurement_by_species, "measurement_type_by_species.csv")
cat("Saved: measurement_type_by_species.csv\n")
print(measurement_by_species)

# ---- TABLE 2: Measurement Type by Ecosystem Service ----
measurement_by_service <- df_cleaned %>%
  group_by(ES, Measurement_Type) %>%
  summarise(n_studies = n_distinct(Study_no), .groups = "drop") %>%
  pivot_wider(names_from = Measurement_Type, values_from = n_studies, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(Total))

write_csv(measurement_by_service, "measurement_type_by_service.csv")
cat("Saved: measurement_type_by_service.csv\n")
print(measurement_by_service)

# ---- TABLE 3: Measurement Type by Species AND Service (Long Format) ----
measurement_by_species_service <- df_cleaned %>%
  group_by(Species_Group, ES, Measurement_Type) %>%
  summarise(n_studies = n_distinct(Study_no), .groups = "drop") %>%
  arrange(Species_Group, desc(n_studies))

write_csv(measurement_by_species_service, "measurement_type_by_species_and_service_long.csv")
cat("Saved: measurement_type_by_species_and_service_long.csv\n")
print(measurement_by_species_service)

# ---- TABLE 4: Wide Format - Species x Service Matrix (with measurement types as subcategories) ----
measurement_matrix <- df_cleaned %>%
  group_by(Species_Group, ES, Measurement_Type) %>%
  summarise(n_studies = n_distinct(Study_no), .groups = "drop") %>%
  unite("ES_MeasType", ES, Measurement_Type, sep = " | ") %>%
  pivot_wider(names_from = Species_Group, values_from = n_studies, values_fill = 0)

write_csv(measurement_matrix, "measurement_type_species_service_matrix.csv")
cat("Saved: measurement_type_species_service_matrix.csv\n")
print(measurement_matrix)

# ---- TABLE 5: Summary Statistics ----
measurement_summary <- df_cleaned %>%
  summarise(
    Total_Studies = n_distinct(Study_no),
    Total_Records = n(),
    Unique_Measurement_Types = n_distinct(Measurement_Type),
    .groups = "drop"
  ) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Count")

write_csv(measurement_summary, "measurement_type_summary_stats.csv")
cat("Saved: measurement_type_summary_stats.csv\n")
print(measurement_summary)

# ---- TABLE 6: Top Measurement Types Overall ----
top_measurement_types <- df_cleaned %>%
  group_by(Measurement_Type) %>%
  summarise(
    n_studies = n_distinct(Study_no),
    percentage = round(n_distinct(Study_no) / n_distinct(df_cleaned$Study_no) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n_studies))

write_csv(top_measurement_types, "top_measurement_types_overall.csv")
cat("Saved: top_measurement_types_overall.csv\n")
print(top_measurement_types)

# ---- TABLE 7: Heatmap Data (for reference) ----
heatmap_data <- df_cleaned %>%
  group_by(Species_Group, Measurement_Type, ES) %>%
  summarise(count = n_distinct(Study_no), .groups = 'drop') %>%
  arrange(Species_Group, ES, Measurement_Type)

write_csv(heatmap_data, "ES_measurement_type_heatmap_data.csv")
cat("Saved: ES_measurement_type_heatmap_data.csv\n")

cat("\n=== SUMMARY ===\n")
cat("All tables have been saved to your working directory.\n")
cat("Files created:\n")
cat("  1. measurement_type_by_species.csv - Species comparison\n")
cat("  2. measurement_type_by_service.csv - Service comparison\n")
cat("  3. measurement_type_by_species_and_service_long.csv - Combined long format\n")
cat("  4. measurement_type_species_service_matrix.csv - Combined wide format\n")
cat("  5. measurement_type_summary_stats.csv - Overall statistics\n")
cat("  6. top_measurement_types_overall.csv - Ranked measurement types\n")
cat("  7. ES_measurement_type_heatmap_data.csv - Heatmap data\n")

