# plot_studies_by_year.R
# Stacked bar chart of studies by publication year and ecosystem service (top N + Other)
# PLUS a cumulative line (unique papers cumulatively) with a secondary y-axis.
#
# Requirements:
# - Codebook_Kelp_Long.csv in the same folder
# - Packages: tidyverse, stringr, forcats, viridis, scales

library(tidyverse)
library(stringr)
library(forcats)
library(viridis)    # for colorblind-friendly discrete palette
library(scales)

# ---- Config ----
top_n <- 10          # number of top ES categories to keep; remainder -> "Other"
drop_year <- 2025    # explicitly drop this year
min_year <- 1900
max_year <- 2026     # safety upper bound

# ---- Read data ----
data <- read_csv("Codebook_Kelp_Long.csv", show_col_types = FALSE)

# ---- Robustly extract year from the data frame (first 4-digit match) ----
data_years <- data %>%
  mutate(Pub_year_char = str_extract(as.character(Pub_year), "\d{4}"),
         Pub_year_num  = as.integer(Pub_year_char)) %>%
  filter(!is.na(Pub_year_num) & Pub_year_num >= min_year & Pub_year_num <= max_year) %>%
  filter(Pub_year_num != drop_year)

message("Publication years retained: ", min(data_years$Pub_year_num), " to ", max(data_years$Pub_year_num),
        " (", n_distinct(data_years$Pub_year_num), " distinct years)")

# ---- Prepare cumulative unique-study counts per year
# Assume each row in the original CSV is a unique study (Study_no exists). Use distinct Study_no if present.
if ("Study_no" %in% names(data_years)) {
  studies_per_year <- data_years %>%
    distinct(Study_no, .keep_all = TRUE) %>%    # ensure each Study_no counted once per dataset
    count(Pub_year_num, name = "n_studies")
} else {
  # fallback: count rows per year (one row per study)
  studies_per_year <- data_years %>%
    count(Pub_year_num, name = "n_studies")
}

studies_per_year <- studies_per_year %>%
  arrange(Pub_year_num) %>%
  mutate(cumulative = cumsum(n_studies))

# ---- Pivot ES columns to long format and clean ES names ----
es_long <- data %>%
  select(Pub_year, starts_with("ES_Name"), Study_no) %>%
  pivot_longer(cols = starts_with("ES_Name"),
               names_to = "ES_type",
               values_to = "ES_name") %>%
  mutate(ES_name = str_squish(ES_name)) %>%
  filter(!is.na(ES_name), ES_name != "", !is.na(Pub_year)) %>%
  mutate(Pub_year_char = str_extract(as.character(Pub_year), "\d{4}"),
         Pub_year_num  = as.integer(Pub_year_char)) %>%
  filter(!is.na(Pub_year_num) & Pub_year_num >= min_year & Pub_year_num <= max_year) %>%
  filter(Pub_year_num != drop_year)

# ---- Count ES mentions per year (raw) ----
es_counts_raw <- es_long %>%
  count(Pub_year_num, ES_name, name = "count")

# ---- Identify top N ES categories by total mentions and lump others into "Other" ----
es_totals <- es_counts_raw %>%
  group_by(ES_name) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

top_names <- head(es_totals$ES_name, top_n)

es_counts <- es_counts_raw %>%
  mutate(ES_group = if_else(ES_name %in% top_names, ES_name, "Other")) %>%
  group_by(Pub_year_num, ES_group) %>%
  summarise(count = sum(count), .groups = "drop")

# ---- Order groups with largest totals first, and keep "Other" last if present ----
group_totals <- es_counts %>%
  group_by(ES_group) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

ordered_groups <- group_totals$ES_group %>% as.character()
if ("Other" %in% ordered_groups) {
  ordered_groups <- setdiff(ordered_groups, "Other")
  ordered_groups <- c(ordered_groups, "Other")
}

es_counts <- es_counts %>%
  mutate(ES_group = factor(ES_group, levels = ordered_groups))

# ---- Make Pub_year a factor for discrete x-axis ----
year_levels <- sort(unique(es_counts$Pub_year_num))
es_counts <- es_counts %>% mutate(Pub_year = factor(Pub_year_num, levels = year_levels))

# ensure studies_per_year has entries for all years (fill zero if missing)
studies_per_year <- tibble(Pub_year_num = year_levels) %>%
  left_join(studies_per_year, by = "Pub_year_num") %>%
  mutate(n_studies = replace_na(n_studies, 0)) %>%
  arrange(Pub_year_num) %>%
  mutate(cumulative = cumsum(n_studies),
         Pub_year = factor(Pub_year_num, levels = year_levels))

# ---- Palette: use viridis discrete colors for the groups (clear + colorblind friendly) ----
n_groups <- n_distinct(es_counts$ES_group)
pal <- viridis(n_groups, option = "turbo")   # options: "viridis","magma","plasma","cividis","turbo"
pal <- pal %>% as.character()

# ---- Compute scaling factors for secondary axis
max_yearly_studies <- max(studies_per_year$n_studies, na.rm = TRUE)
max_cumulative <- max(studies_per_year$cumulative, na.rm = TRUE)
if (max_cumulative == 0) max_cumulative <- 1
if (max_yearly_studies == 0) max_yearly_studies <- 1
scale_factor <- max_yearly_studies / max_cumulative
inv_scale_factor <- max_cumulative / max_yearly_studies
