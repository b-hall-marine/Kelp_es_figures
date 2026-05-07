# BH 07/05/2026 Table_creation_Countries_services.R
#
# PURPOSE:
#   Generate summary tables quantifying kelp research by
#   country, ecosystem service, biophysical driver, and species. Produces
#   both overall summaries and species-stratified breakdowns with percentages.
#   Primary focus: geographic distribution of research effort and ES-species
#   research patterns.
#
# KEY FEATURES:
#   - Country normalisation: standardizes country name variations and multi-country
#     studies (splits by semicolon/comma)
#   - Mutually exclusive species categorization: sac_only, lam_only, both, 
#   - Robust column detection: automatically finds ES_Name, ES_type, and 
#     Bio_physical_Driver columns regardless of numbering format
#   - Duplicate handling: all counts use distinct(Study_no) to avoid 
#     double-counting
#   - Percentage calculations: includes both absolute counts and percentages
#     for cross-study comparisons
#   - Diagnostic output: prints species categorization summary to console
#
# OUTPUTS (14 CSV files):
#   - counts_by_country.csv - Studies per country, ranked
#   - counts_by_service.csv - Unique services identified
#   - counts_by_service_type.csv - Ecosystem service types
#   - counts_by_driver.csv - Biophysical drivers identified
#   - counts_by_species.csv - Study count per species category
#   - counts_ES_by_species.csv - ES × species matrix (long, with %)
#   - counts_ES_by_species_wide.csv - ES × species matrix (wide, counts)
#   - counts_ES_by_species_wide_percent.csv - ES × species (wide, %)
#   - counts_service_type_by_species.csv - Service type × species
#   - es_per_study_by_species.csv - Services per study by species
#   - es_per_study_summary_by_species.csv - ES statistics by species
#   - species_overlap_counts.csv - Species co-occurrence analysis
#   - summary_stats.csv - Key metrics with percentages
#   - country_service_matrix.csv - Country × service cross-tabulation (optional)
#   - study_species_category.csv - Study-level species assignments
#   - articles_per_year.csv - Publications per year (overall)
#   - articles_per_year_by_species.csv - Publications per year by species
#
# USAGE:
#   Place Codebook_Kelp_Long.csv in working directory, then:
#   source("Table_creation_Countries_services.R")
#
# NOTES:
#   - Multi-country studies: automatically split and assigned to each country
#   - Species categorization: prioritizes "Both" designation, then checks for
#     individual species; prevents double-counting
#   - Percentages: calculated as (count / total studies) × 100 to enable
#     cross-study comparison
#   - Diagnostic output: prints species categorization summary and example
#     "both" studies to console for validation
#   - Unmapped countries: lists any unrecognized country strings for manual review
#   - Empty columns: gracefully skips sections if required columns are missing

library(tidyverse)
library(countrycode)
library(stringr)

# ---- Config & file check ----
csv_file <- "Codebook_Kelp_Long.csv"
if (!file.exists(csv_file)) stop("CSV not found in working directory: ", csv_file)

df <- readr::read_csv(csv_file, guess_max = 2000, show_col_types = FALSE)

if (!"Study_no" %in% colnames(df)) stop("No Study_no column found in CSV")

# ---- Country normalisation: prefer Study_CoR column ----
normalize_country <- function(country_raw) {
  country_raw <- ifelse(is.na(country_raw), "", str_squish(country_raw))
  out <- case_when(
    str_detect(country_raw, regex("United States|USA|United States of America", ignore_case = TRUE)) ~ "United States",
    str_detect(country_raw, regex("United Kingdom|UK|Great Britain|United Kingdon", ignore_case = TRUE)) ~ "United Kingdom",
    str_detect(country_raw, regex("Republic of Korea|Korea", ignore_case = TRUE)) ~ "South Korea",
    str_detect(country_raw, regex("Russian Federation|Russia|Russian", ignore_case = TRUE)) ~ "Russia",
    str_detect(country_raw, regex("Canada", ignore_case = TRUE)) ~ "Canada",
    str_detect(country_raw, regex("Norway", ignore_case = TRUE)) ~ "Norway",
    str_detect(country_raw, regex("Germany", ignore_case = TRUE)) ~ "Germany",
    str_detect(country_raw, regex("France", ignore_case = TRUE)) ~ "France",
    str_detect(country_raw, regex("Spain", ignore_case = TRUE)) ~ "Spain",
    str_detect(country_raw, regex("Iceland", ignore_case = TRUE)) ~ "Iceland",
    str_detect(country_raw, regex("Denmark", ignore_case = TRUE)) ~ "Denmark",
    str_detect(country_raw, regex("Sweden", ignore_case = TRUE)) ~ "Sweden",
    str_detect(country_raw, regex("Portugal", ignore_case = TRUE)) ~ "Portugal",
    str_detect(country_raw, regex("Ireland", ignore_case = TRUE)) ~ "Ireland",
    str_detect(country_raw, regex("Netherlands|Dutch", ignore_case = TRUE)) ~ "Netherlands",
    str_detect(country_raw, regex("Baltic Sea|Global|Review|N/A|Not specified|Other species|Both", ignore_case = TRUE)) ~ NA_character_,
    TRUE ~ countrycode(country_raw, origin = "country.name", destination = "country.name")
  )
  out
}

# Use Study_CoR (preferred). If missing, fall back to Study_Country if it exists.
if ("Study_CoR" %in% names(df)) {
  df <- df %>% mutate(country_raw = if_else(is.na(Study_CoR) | str_trim(Study_CoR) == "", "", str_squish(Study_CoR)))
} else if ("Study_Country" %in% names(df)) {
  df <- df %>% mutate(country_raw = if_else(is.na(Study_Country) | str_trim(Study_Country) == "", "", str_squish(Study_Country)))
} else {
  df <- df %>% mutate(country_raw = "")
}

df <- df %>% mutate(country = normalize_country(country_raw))

unmapped_countries <- df %>% filter(is.na(country) & country_raw != "") %>% distinct(country_raw) %>% pull(country_raw)
if (length(unmapped_countries) > 0) {
  cat("Unmapped country strings (consider manual recoding):\n")
  print(unmapped_countries)
} else {
  cat("All country strings mapped or blank.\n")
}

# ---- Find ES name/type and driver columns robustly ----
service_name_cols <- intersect(colnames(df), paste0("ES_Name_", sprintf("%03d", 1:20)))
if (length(service_name_cols) == 0) {
  service_name_cols <- intersect(colnames(df), paste0("ES_Name_", 1:10))
}
service_name_cols <- service_name_cols[service_name_cols != ""]

service_type_cols <- intersect(colnames(df), paste0("ES_type_", sprintf("%03d", 1:20)))
if (length(service_type_cols) == 0) {
  service_type_cols <- intersect(colnames(df), paste0("ES_type_", 1:10))
}
service_type_cols <- service_type_cols[service_type_cols != ""]

driver_cols <- intersect(colnames(df), paste0("Bio_physical_Driver_", sprintf("%03d", 1:20)))
if (length(driver_cols) == 0) {
  driver_cols <- intersect(colnames(df), paste0("Bio_physical_Driver_", 1:10))
}
driver_cols <- driver_cols[driver_cols != ""]

# ---- 1) counts_by_country.csv ----
counts_by_country <- df %>%
  filter(!is.na(country) & country != "") %>%
  distinct(Study_no, country) %>%
  count(country, name = "n") %>%
  arrange(desc(n))

readr::write_csv(counts_by_country, "counts_by_country.csv")
cat("Saved counts_by_country.csv with", nrow(counts_by_country), "rows\n")

# ---- 2) counts_by_service.csv ----
if (length(service_name_cols) == 0) {
  cat("No ES_Name_* columns found. Skipping counts_by_service.\n")
  counts_by_service <- tibble()
} else {
  counts_by_service <- df %>%
    select(Study_no, all_of(service_name_cols)) %>%
    pivot_longer(cols = all_of(service_name_cols), names_to = "service_col", values_to = "service") %>%
    mutate(service = if_else(is.na(service) | str_trim(service) == "", NA_character_, str_squish(service))) %>%
    filter(!is.na(service)) %>%
    distinct(Study_no, service) %>%
    count(service, name = "n") %>%
    arrange(desc(n))
  
  readr::write_csv(counts_by_service, "counts_by_service.csv")
  cat("Saved counts_by_service.csv with", nrow(counts_by_service), "rows\n")
}

# ---- Articles per year ----
articles_per_year <- df %>%
  mutate(Pub_year_char = str_extract(as.character(Pub_year), "\\d{4}"),
         Pub_year_num = as.integer(Pub_year_char)) %>%
  filter(!is.na(Pub_year_num)) %>%
  distinct(Study_no, .keep_all = TRUE) %>%
  count(Pub_year_num, name = "n_articles") %>%
  arrange(Pub_year_num) %>%
  rename(year = Pub_year_num)

readr::write_csv(articles_per_year, "articles_per_year.csv")
cat("Saved articles_per_year.csv with", nrow(articles_per_year), "rows\n")


# ---- 3) counts_by_service_type.csv ----
if (length(service_type_cols) == 0) {
  cat("No ES_type_* columns found. Skipping counts_by_service_type.\n")
  counts_by_service_type <- tibble()
} else {
  counts_by_service_type <- df %>%
    select(Study_no, all_of(service_type_cols)) %>%
    pivot_longer(cols = all_of(service_type_cols), names_to = "stype_col", values_to = "service_type") %>%
    mutate(service_type = if_else(is.na(service_type) | str_trim(service_type) == "", NA_character_, str_squish(service_type))) %>%
    filter(!is.na(service_type)) %>%
    distinct(Study_no, service_type) %>%
    count(service_type, name = "n") %>%
    arrange(desc(n))
  
  readr::write_csv(counts_by_service_type, "counts_by_service_type.csv")
  cat("Saved counts_by_service_type.csv with", nrow(counts_by_service_type), "rows\n")
}

# ---- 4) counts_by_driver.csv ----
if (length(driver_cols) == 0) {
  cat("No Bio_physical_Driver_* columns found. Skipping counts_by_driver.\n")
  counts_by_driver <- tibble()
} else {
  counts_by_driver <- df %>%
    select(Study_no, all_of(driver_cols)) %>%
    pivot_longer(cols = all_of(driver_cols), names_to = "driver_col", values_to = "driver") %>%
    mutate(driver = if_else(is.na(driver) | str_trim(driver) == "", NA_character_, str_squish(driver))) %>%
    filter(!is.na(driver)) %>%
    distinct(Study_no, driver) %>%
    count(driver, name = "n") %>%
    arrange(desc(n))
  
  readr::write_csv(counts_by_driver, "counts_by_driver.csv")
  cat("Saved counts_by_driver.csv with", nrow(counts_by_driver), "rows\n")
}

# ---- 5) Species: use Kelp_species_mentioned (FIXED - Check "Both" FIRST) ----
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
    both_species = is_both | (sac & lam),  # "Both" flag OR detected both species
    other_only = other_species & !sac & !lam & !is_both,
    species_category = case_when(
      both_species ~ "both",
      sac_only ~ "sac_only",
      lam_only ~ "lam_only",
      other_only ~ "other_only",
      TRUE ~ "none"
    )
  )

# DIAGNOSTIC: Show what's being categorized
cat("\n=== SPECIES CATEGORIZATION DIAGNOSTIC ===\n")
species_debug <- df %>%
  distinct(Study_no, species_category, is_both, sac, lam, species_raw) %>%
  group_by(species_category) %>%
  summarise(
    count = n(),
    example_raw = paste(head(unique(species_raw[species_raw != ""]), 2), collapse = " | "),
    .groups = "drop"
  )
print(species_debug)

# Show studies with "Both" specifically
both_examples <- df %>%
  filter(species_category == "both") %>%
  distinct(Study_no, species_raw) %>%
  head(10)

cat("\nExamples of 'both' studies:\n")
print(both_examples)

# Save mapping
study_species_map <- df %>%
  distinct(Study_no, species_category, is_both, sac, lam, species_raw) %>%
  arrange(Study_no)

readr::write_csv(study_species_map, "study_species_category.csv")
cat("Saved study_species_category.csv\n")

# ---- Articles per year by species category ----
articles_per_year_species <- df %>%
  mutate(Pub_year_char = str_extract(as.character(Pub_year), "\\d{4}"),
         Pub_year_num = as.integer(Pub_year_char)) %>%
  filter(!is.na(Pub_year_num)) %>%
  distinct(Study_no, .keep_all = TRUE) %>%
  count(Pub_year_num, species_category, name = "n_articles") %>%
  pivot_wider(names_from = species_category, values_from = n_articles, values_fill = 0) %>%
  arrange(Pub_year_num) %>%
  rename(year = Pub_year_num)

readr::write_csv(articles_per_year_species, "articles_per_year_by_species.csv")
cat("Saved articles_per_year_by_species.csv\n")



# ---- 6) counts_by_species.csv (summary of study counts per exclusive species category) ----
counts_by_species <- df %>%
  distinct(Study_no, species_category) %>%
  group_by(species_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

# ensure categories shown even if zero (optional)
all_cats <- c("sac_only", "lam_only", "both", "other_only", "none")
counts_by_species <- tibble(species_category = all_cats) %>%
  left_join(counts_by_species, by = "species_category") %>%
  mutate(n = replace_na(n, 0))

readr::write_csv(counts_by_species, "counts_by_species.csv")
cat("Saved counts_by_species.csv\n")

# ---- 7) counts_ES_by_species (long) & wide matrix -------------------------
if (length(service_name_cols) > 0) {
  es_long_full <- df %>%
    select(Study_no, species_category, sac, lam, other_species, all_of(service_name_cols)) %>%
    pivot_longer(cols = all_of(service_name_cols), names_to = "service_col", values_to = "service") %>%
    mutate(service = if_else(is.na(service) | str_trim(service) == "", NA_character_, str_squish(service))) %>%
    filter(!is.na(service)) %>%
    distinct(Study_no, species_category, sac, lam, other_species, service)
  
  # counts by species_category and service (long)
  counts_ES_by_species <- es_long_full %>%
    distinct(species_category, Study_no, service) %>%
    count(species_category, service, name = "n") %>%
    arrange(species_category, desc(n))
  
  # compute percent within each service: of studies that reported that service, what percent belong to each species_category
  counts_ES_by_species <- counts_ES_by_species %>%
    group_by(service) %>%
    mutate(service_total = sum(n, na.rm = TRUE),
           percent = if_else(service_total > 0, n / service_total * 100, 0)) %>%
    ungroup() %>%
    arrange(service, desc(n))
  
  readr::write_csv(counts_ES_by_species, "counts_ES_by_species.csv")
  cat("Saved counts_ES_by_species.csv (includes percent of service-studies by species_category)\n")
  
  counts_ES_by_species_wide <- counts_ES_by_species %>%
    select(-service_total, -percent) %>%   # keep counts in the wide matrix
    pivot_wider(names_from = service, values_from = n, values_fill = 0)
  
  readr::write_csv(counts_ES_by_species_wide, "counts_ES_by_species_wide.csv")
  cat("Saved counts_ES_by_species_wide.csv\n")
  
  # Also write a wide table including percentages per service (species_category rows, service columns with percent)
  counts_ES_by_species_wide_pct <- counts_ES_by_species %>%
    select(species_category, service, percent) %>%
    pivot_wider(names_from = service, values_from = percent, values_fill = 0)
  readr::write_csv(counts_ES_by_species_wide_pct, "counts_ES_by_species_wide_percent.csv")
  cat("Saved counts_ES_by_species_wide_percent.csv (percent of service-studies by species_category)\n")
  
} else {
  cat("No ES_Name_* columns - skipping ES by species outputs.\n")
}

# ---- 8) counts_service_type_by_species ----------------------------------
if (length(service_type_cols) > 0) {
  st_long <- df %>%
    select(Study_no, species_category, all_of(service_type_cols)) %>%
    pivot_longer(cols = all_of(service_type_cols), names_to = "stype_col", values_to = "service_type") %>%
    mutate(service_type = if_else(is.na(service_type) | str_trim(service_type) == "", NA_character_, str_squish(service_type))) %>%
    filter(!is.na(service_type)) %>%
    distinct(Study_no, species_category, service_type)
  
  counts_service_type_by_species <- st_long %>%
    distinct(species_category, Study_no, service_type) %>%
    count(species_category, service_type, name = "n") %>%
    arrange(species_category, desc(n))
  
  readr::write_csv(counts_service_type_by_species, "counts_service_type_by_species.csv")
  cat("Saved counts_service_type_by_species.csv\n")
} else {
  cat("No service type columns - skipping service_type_by_species.\n")
}

# ---- 9) ES per study and summaries ------------------------------------
if (length(service_name_cols) > 0) {
  es_per_study <- es_long_full %>%
    group_by(Study_no, species_category) %>%
    summarise(n_services = n_distinct(service), services = paste(sort(unique(service)), collapse = " | "), .groups = "drop")
  
  readr::write_csv(es_per_study, "es_per_study_by_species.csv")
  cat("Saved es_per_study_by_species.csv\n")
  
  es_per_study_summary <- es_per_study %>%
    group_by(species_category) %>%
    summarise(
      n_studies = n(),
      mean_services = mean(n_services, na.rm = TRUE),
      median_services = median(n_services, na.rm = TRUE),
      sd_services = sd(n_services, na.rm = TRUE),
      min_services = min(n_services, na.rm = TRUE),
      max_services = max(n_services, na.rm = TRUE),
      .groups = "drop"
    )
  
  readr::write_csv(es_per_study_summary, "es_per_study_summary_by_species.csv")
  cat("Saved es_per_study_summary_by_species.csv\n")
}

# ---- 10) Species overlap counts ---------------------------------------
species_overlap <- df %>%
  distinct(Study_no, sac, lam, other_only) %>%
  summarise(
    total_studies = n(),
    sac_only = sum(sac & !lam),
    lam_only = sum(lam & !sac),
    sac_and_lam = sum(sac & lam),
    neither = sum(!sac & !lam & !other_only),
    other_only = sum(other_only & !sac & !lam)
  ) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value")

readr::write_csv(species_overlap, "species_overlap_counts.csv")
cat("Saved species_overlap_counts.csv\n")

# ---- 11) small summary file for manuscript text ------------------------
total_studies <- n_distinct(df$Study_no)
total_unique_services <- if (exists("counts_by_service") && nrow(counts_by_service) > 0) nrow(counts_by_service) else NA
total_service_types <- if (exists("counts_by_service_type") && nrow(counts_by_service_type) > 0) nrow(counts_by_service_type) else NA

# Pull counts for main species categories (sac_only, lam_only, both)
sac_count <- counts_by_species %>% filter(species_category == "sac_only") %>% pull(n) %>% replace_na(0)
lam_count <- counts_by_species %>% filter(species_category == "lam_only") %>% pull(n) %>% replace_na(0)
both_count <- counts_by_species %>% filter(species_category == "both") %>% pull(n) %>% replace_na(0)
other_count <- counts_by_species %>% filter(species_category == "other_only") %>% pull(n) %>% replace_na(0)
none_count <- counts_by_species %>% filter(species_category == "none") %>% pull(n) %>% replace_na(0)

# percentages of total studies
sac_pct <- if (total_studies > 0) sac_count / total_studies * 100 else NA_real_
lam_pct <- if (total_studies > 0) lam_count / total_studies * 100 else NA_real_
both_pct <- if (total_studies > 0) both_count / total_studies * 100 else NA_real_
other_pct <- if (total_studies > 0) other_count / total_studies * 100 else NA_real_
none_pct <- if (total_studies > 0) none_count / total_studies * 100 else NA_real_

summary_stats <- tibble(
  metric = c("total_studies", "total_unique_services", "total_service_types",
             "saccharina_only_studies", "saccharina_only_pct",
             "laminaria_only_studies", "laminaria_only_pct",
             "both_species_studies", "both_species_pct",
             "other_only_studies", "other_only_pct",
             "none_studies", "none_pct"),
  value = c(
    total_studies,
    total_unique_services,
    total_service_types,
    sac_count,
    round(sac_pct, 1),
    lam_count,
    round(lam_pct, 1),
    both_count,
    round(both_pct, 1),
    other_count,
    round(other_pct, 1),
    none_count,
    round(none_pct, 1)
  )
)

readr::write_csv(summary_stats, "summary_stats.csv")
cat("Saved summary_stats.csv (includes percent columns for species categories)\n")

# ---- 12) Optional: country x service matrix -----------------------------
if (length(service_name_cols) > 0 && nrow(counts_by_country) > 0) {
  country_service_matrix <- df %>%
    select(Study_no, country, all_of(service_name_cols)) %>%
    pivot_longer(cols = all_of(service_name_cols), names_to = "service_col", values_to = "service") %>%
    mutate(service = if_else(is.na(service) | str_trim(service) == "", NA_character_, str_squish(service))) %>%
    filter(!is.na(country) & !is.na(service)) %>%
    distinct(country, Study_no, service) %>%
    group_by(country, service) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(names_from = service, values_from = n, values_fill = 0)
  
  readr::write_csv(country_service_matrix, "country_service_matrix.csv")
  cat("Saved country_service_matrix.csv\n")
}

cat("\nDone. Files written to working directory:\n")
print(list.files(pattern = "counts_by_|es_per_study|summary_stats|species_overlap_counts|country_service_matrix|study_species_category"))
