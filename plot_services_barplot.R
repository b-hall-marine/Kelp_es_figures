# BH 07/05/2026 plot_services_barplot.R
# Stacked horizontal bar chart of studies per service, broken down by kelp species.
# Outputs:
#  - counts_service_species.csv
#  - services_by_species_stack.png
#
# Usage:
#  Put Codebook_Kelp_Long.csv (and optionally ES_Service_Type.csv) in the same folder and run:
#  source("plot_services_by_species.R")
#
# Required packages:
# install.packages(c("tidyverse","scales"))

library(tidyverse)
library(scales)

# ---- CONFIG ----
codebook_file <- "Codebook_Kelp_Long.csv"
mapping_files <- c("ES_Service_Type.csv", "ES_Service_Type.txt", "ES_Service_Type.tsv") # optional mapping file
out_counts_csv <- "counts_service_species.csv"
out_plot_png <- "services_by_species_stack.png"
top_n_services <- 30   # set to Inf to show all

# ---- Read codebook ----
if (!file.exists(codebook_file)) stop("Codebook CSV not found: ", codebook_file)
df <- readr::read_csv(codebook_file, guess_max = 2000, show_col_types = FALSE)


# ---- Find kelp species column (try several common names) ----
possible_species_cols <- c("Kelp_species_mentioned")
species_col <- intersect(possible_species_cols, colnames(df)) %>% first()


if (is.na(species_col) || is.null(species_col)) {
  # fallback: try any column that looks like it contains 'Kelp' in the name
  species_col <- colnames(df)[str_detect(colnames(df), regex("kelp", ignore_case = TRUE))] %>% first()
}
if (is.na(species_col) || is.null(species_col)) {
  stop("Could not find a kelp species column; please check column names. Tried: ", paste(possible_species_cols, collapse = ", "))
}
cat("Using species column:", species_col, "\n")

# ---- Optional: read service->category mapping for nicer labels (not required) ----
map_file <- NULL
for (f in mapping_files) if (file.exists(f)) { map_file <- f; break }
service_mapping <- NULL
if (!is.null(map_file)) {
  # try to read; allow CSV or tab-delimited .txt
  delim <- ifelse(tools::file_ext(map_file) == "csv", ",", "\t")
  service_mapping <- readr::read_delim(map_file, delim = delim, col_names = c("Service", "Category"),
                                       show_col_types = FALSE) %>%
    mutate(Service = str_squish(as.character(Service)))
  cat("Loaded service->category mapping from:", map_file, "\n")
} else {
  cat("No service->category mapping file found (ES_Service_Type.csv). Continuing without it.\n")
}

# ---- Detect ES name columns robustly ----
possible_patterns <- c(paste0("ES_Name_", sprintf("%03d", 1:10)),
                       paste0("ES_Name_", 1:10),
                       paste0("ES_name_", sprintf("%03d", 1:10)),
                       paste0("ES_name_", 1:10))
es_name_cols <- intersect(colnames(df), possible_patterns)
if (length(es_name_cols) == 0) {
  es_name_cols <- intersect(colnames(df), c("ES_Name_001","ES_Name_1","ES_Name_002","ES_Name_2"))
}
if (length(es_name_cols) == 0) stop("No ES_Name_* columns found. Please check column names.")

# ---- Create long table: Study_no x service x species ----
services_long <- df %>%
  select(Study_no, all_of(es_name_cols), all_of(species_col)) %>%
  pivot_longer(cols = -c(Study_no, all_of(species_col)), names_to = "service_col", values_to = "service_raw") %>%
  mutate(
    service = if_else(is.na(service_raw) | str_trim(service_raw) == "", NA_character_, str_squish(as.character(service_raw))),
    species = if_else(is.na(.data[[species_col]]) | str_trim(as.character(.data[[species_col]])) == "", NA_character_, str_squish(as.character(.data[[species_col]])))
  ) %>%
  filter(!is.na(service)) %>%
  # canonicalise the common long sediment phrase so it matches mapping if needed
  mutate(service = if_else(str_detect(service, regex("sediment.*regulat|sediment trapping", ignore_case = TRUE)),
                           "Regulation of sediment flows",
                           service))

# ---- Deduplicate per Study_no x service: keep species as recorded (if multiple species per study for same service keep all distinct) ----
study_service_species <- services_long %>%
  distinct(Study_no, service, species)

# If species is NA for some rows, mark "Unknown"
study_service_species <- study_service_species %>%
  mutate(species = if_else(is.na(species) | species == "", "Unknown", species))

# ---- Compute counts per service x species ----
counts <- study_service_species %>%
  group_by(service, species) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(service) %>%
  mutate(total_service = sum(n),
         pct = round(100 * n / total_service, 1)) %>%
  ungroup() %>%
  arrange(desc(total_service), service, desc(n))

# Save counts CSV
readr::write_csv(counts, out_counts_csv)
cat("Wrote counts to:", out_counts_csv, "\n")

# ---- Select top services to plot ----
service_totals <- counts %>% distinct(service, total_service) %>% arrange(desc(total_service))
if (is.finite(top_n_services)) {
  top_services <- service_totals %>% slice_max(total_service, n = top_n_services) %>% pull(service)
  plot_df <- counts %>% filter(service %in% top_services)
} else {
  plot_df <- counts
}

# Reorder services by total descending for plotting
service_order <- plot_df %>% distinct(service, total_service) %>% arrange(desc(total_service)) %>% pull(service)
plot_df <- plot_df %>% mutate(service = factor(service, levels = rev(service_order))) # reversed for coord_flip

# Shorten long species names if desired (optional)
plot_df <- plot_df %>% mutate(species_short = species)

# ---- HIDE IN-SEGMENT PCT LABELS BELOW A GIVEN SERVICE ----
# Find the rank of the named cutoff service ("Species Richness") in the descending totals list.
# We'll display in-segment percent labels only for services ranked above (i.e. with larger totals than) that cutoff.
cutoff_service_name <- "Species Richness"
# compute descending ordered vector of services (highest total first)
descending_services <- plot_df %>% distinct(service, total_service) %>% arrange(desc(total_service)) %>% pull(service)
# find index (NA-safe, case-insensitive)
cutoff_idx <- which(tolower(descending_services) == tolower(cutoff_service_name))

if (length(cutoff_idx) == 1) {
  # services to show pct labels for: those with rank <= cutoff_idx (i.e. above or equal)
  show_services <- descending_services[seq_len(cutoff_idx)]
  plot_df <- plot_df %>% mutate(show_pct = service %in% show_services)
  cat("Found cutoff service:", cutoff_service_name, "- showing percent labels for top", cutoff_idx, "services.\n")
} else {
  # fallback: if cutoff not found, show labels only for services with total_service >= threshold_count
  threshold_count <- 30
  plot_df <- plot_df %>% mutate(show_pct = total_service >= threshold_count)
  cat("Cutoff service not found. Using fallback threshold:", threshold_count, " to show percent labels.\n")
}


# ---- Plot: stacked horizontal bars with percent labels for each species segment ----
# label only segments where pct >= threshold
label_threshold <- 5.5


p <- ggplot(plot_df, aes(x = service, y = n, fill = species_short)) +
  geom_col(width = 0.75, color = "grey35") +
  coord_flip() +
  # percent labels inside segments where there is enough space and service is above cutoff
  geom_text(aes(label = ifelse(show_pct & pct >= label_threshold, paste0(pct, "%"), "")),
            position = position_stack(vjust = 0.5), colour = "black", size = 3) +
  # total count to the right of each bar
  geom_text(data = plot_df %>% distinct(service, total_service),
            aes(x = service, y = total_service, label = total_service),
            hjust = -1.1, inherit.aes = FALSE, size = 3.3) +
  theme_minimal(base_size = 13) +
  labs(title = "",
       x = NULL, y = "Number of papers", fill = "Kelp species") +
  theme(panel.grid.major.y = element_line(color = "gray92", size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.y = element_text(size = 10)) +
  expand_limits(y = max(plot_df$total_service) * 1.08)

# Choose a palette with enough distinct colours for species; use hue palette
num_species <- plot_df %>% distinct(species_short) %>% nrow()
palette_vals <- scales::hue_pal()(num_species)
names(palette_vals) <- plot_df %>% distinct(species_short) %>% arrange(species_short) %>% pull(species_short)

p <- p + scale_fill_manual(values = palette_vals, na.value = "grey70")

# Save plot
ggsave(out_plot_png, p, width = 14, height = 10, dpi = 800)
cat("Saved plot to:", out_plot_png, "\n")

# ---- Print a small table to console for verification ----
cat("\nTop rows of counts (service, species, n, pct):\n")
print(counts %>% arrange(desc(total_service), service, desc(n)) %>% slice_head(n = 50))


# Create a bar chart of the number of unique studies that mention each ecosystem service.
#
# Place this file in the same folder as Codebook_Kelp_Long.csv and run:
# source("plot_services_barplot.R")
#
# Required packages:
# install.packages(c("tidyverse", "countrycode", "viridis"))
#
# Outputs:
# - services_barplot.png  (horizontal bar chart)
# - counts_by_service.csv (counts used to make the plot)

library(tidyverse)
library(viridis)

# ---- Read CSV ----
csv_file <- "Codebook_Kelp_Long.csv"
if (!file.exists(csv_file)) stop("CSV not found in working directory: ", csv_file)
df <- readr::read_csv(csv_file, guess_max = 2000, show_col_types = FALSE)

# ---- Detect ES name columns (tolerant to ES_Name_001 or ES_Name_1 variants) ----
possible_patterns <- c(paste0("ES_Name_", sprintf("%03d", 1:10)),
                       paste0("ES_Name_", 1:10),
                       paste0("ES_name_", sprintf("%03d", 1:10)),
                       paste0("ES_name_", 1:10))

es_name_cols <- intersect(colnames(df), possible_patterns)

if (length(es_name_cols) == 0) {
  stop("No ES name columns found (looked for ES_Name_001..ES_Name_005). Please check column names.")
}

# ---- Normalise and canonicalise service strings ----
canonicalise_service <- function(s) {
  s2 <- ifelse(is.na(s) | str_trim(s) == "", NA_character_, str_squish(s))
  # tolerant replacement for the long sediment string (ignore case)
  s2 <- ifelse(!is.na(s2) & str_detect(s2, regex("sediment.*regulation.*flow|sediment trapping.*regulat", ignore_case = TRUE)),
               "Regulation of sediment flows",
               s2)
  s2
}


# ---- Tidy up services and count unique studies per service ----
counts_by_service <- df %>%
  # keep Study_no and all service name columns
  select(Study_no, all_of(es_name_cols)) %>%
  pivot_longer(cols = -Study_no, names_to = "service_col", values_to = "service_raw") %>%
  # normalize service text: NA/blanks -> NA, trim whitespace, unify case
  mutate(service = if_else(is.na(service_raw) | str_trim(service_raw) == "",
                           NA_character_,
                           str_squish(service_raw))) %>%
  filter(!is.na(service)) %>%
  # count each study once per service (distinct Study_no + service)
  distinct(Study_no, service) %>%
  count(service, name = "n") %>%
  arrange(desc(n)) %>%
  # for nicer axis labels: optionally title-case
  mutate(service_clean = service) %>%
  relocate(service_clean, .before = service)    # keep a clean name column

# Save counts for inspection
readr::write_csv(counts_by_service, "counts_by_service.csv")
cat("Wrote counts_by_service.csv (", nrow(counts_by_service), " services )\n", sep = "")

# ---- Plot horizontal bar chart with light horizontal guide lines ----
top_n_services <- top_n_services %>% mutate(service_clean = fct_reorder(service_clean, n))

p <- ggplot(top_n_services, aes(x = service_clean, y = n)) +
  geom_col(fill = viridis(1, option = "D"), width = 0.7) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(title = "",
       subtitle = "",
       x = "Individual Ecosystem Service", y = "Number of studies") +
  theme(
    # horizontal guide lines (light grey) to follow labels easily
    panel.grid.major.y = element_line(color = "gray90", size = 0.5),
    panel.grid.major.x = element_blank(),  # remove vertical gridlines
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  ) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) + # optional labels at end of bars
  expand_limits(y = max(top_n_services$n) * 1.08)     # add small space for labels

# Save plot
out_file <- "services_barplot_updated.png"
ggsave(out_file, p, width = 10, height = 8, dpi = 300)
cat("Saved bar chart to:", out_file, "\n")

# Print top rows to console
cat("\nTop services (console):\n")
print(head(counts_by_service, 50))




# ---- Step 2 Bar Chart split by services ---- #

library(tidyverse)
library(viridis)

# ---- Config ----
codebook_file <- "Codebook_Kelp_Long.csv"
mapping_files <- c("ES_Service_Type.csv")
top_n <- 30   # number of top services to show (set to Inf to show all)

# ---- Read files ----
if (!file.exists(codebook_file)) stop("Codebook CSV not found: ", codebook_file)
df <- readr::read_csv(codebook_file, guess_max = 2000, show_col_types = FALSE)

# read mapping (try multiple filenames)
map_file <- NULL
for (f in mapping_files) if (file.exists(f)) { map_file <- f; break }
if (is.null(map_file)) stop("Service->Category mapping file not found. Provide ES_Service_Type.csv or .txt")

mapping <- readr::read_delim(map_file, delim = ifelse(tools::file_ext(map_file) == "csv", ",", "\t"),
                             col_names = c("Service", "Category"), trim_ws = TRUE,
                             show_col_types = FALSE)

# Clean mapping: trim and lowercase for robust joins
mapping <- mapping %>%
  mutate(Service = str_squish(as.character(Service)),
         Category = str_squish(as.character(Category)),
         service_key = tolower(Service) %>% str_squish())

# ---- Detect ES name columns (robust) ----
possible_patterns <- c(paste0("ES_Name_", sprintf("%03d", 1:10)),
                       paste0("ES_Name_", 1:10),
                       paste0("ES_name_", sprintf("%03d", 1:10)),
                       paste0("ES_name_", 1:10))
es_name_cols <- intersect(colnames(df), possible_patterns)
if (length(es_name_cols) == 0) {
  # try common alternative names just in case
  es_name_cols <- intersect(colnames(df), c("ES_Name_001","ES_Name_002","ES_Name_003","ES_Name_004","ES_Name_005"))
}
if (length(es_name_cols) == 0) stop("No ES_Name_* columns found. Please check column names in the codebook.")

# ---- Pivot to long (one row per Study_no x service mention) ----
services_long <- df %>%
  select(Study_no, all_of(es_name_cols)) %>%
  pivot_longer(cols = -Study_no, names_to = "service_col", values_to = "service_raw") %>%
  mutate(service_clean = if_else(is.na(service_raw) | str_trim(service_raw) == "",
                                 NA_character_,
                                 str_squish(as.character(service_raw)))) %>%
  filter(!is.na(service_clean)) %>%
  # canonicalise a common long variant to match your mapping if needed
  mutate(service_clean = if_else(str_detect(service_clean, regex("sediment trapping.*regulat|sediment.*regulat", ignore_case = TRUE)),
                                 "Sediment trapping on reef / Regulation of sediment flows",
                                 service_clean))

# ---- Count unique studies per service (each study counted once per service) ----
counts <- services_long %>%
  distinct(Study_no, service_clean) %>%
  count(service_clean, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(service_key = tolower(service_clean) %>% str_squish())

# ---- Join to mapping (Category) ----
counts_mapped <- counts %>%
  left_join(mapping %>% select(service_key, Category), by = "service_key") %>%
  mutate(Category = if_else(is.na(Category), "Unknown", Category))

# save CSV for inspection
readr::write_csv(counts_mapped %>% select(service = service_clean, n, Category), "counts_by_service_with_category.csv")
cat("Wrote counts_by_service_with_category.csv\n")

# ---- Select top N to plot (or all) ----
if (is.finite(top_n)) {
  plot_df <- counts_mapped %>% slice_max(n, n = top_n)
} else {
  plot_df <- counts_mapped
}

# reorder factor descending
plot_df <- plot_df %>% mutate(service = fct_reorder(service_clean, n))

# ---- Colour mapping: adjust to taste ----
category_palette <- c(
  "Provisioning" = "royalblue",  # blue
  "Regulating"  = "#F28E4B",   # orange
  "Supporting"  = "#56BFA6",   # green
  "Cultural"    = "khaki",   # purple
  "Unknown"     = "grey70"
)

# ensure categories present in palette (add any new categories to palette)
cats_present <- unique(plot_df$Category)
missing_cats <- setdiff(cats_present, names(category_palette))
if (length(missing_cats) > 0) {
  for (mc in missing_cats) category_palette[mc] <- "grey60"
}

# ---- Plot 3 ----
p <- ggplot(plot_df, aes(x = service, y = n, fill = Category)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = category_palette, name = "MEA Category") +
  theme_minimal(base_size = 13) +
  labs(title = "",
       x = NULL, y = "Number of Papers") +
  theme(
    panel.grid.major.y = element_line(color = "gray92", size = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    legend.position = "right"
  ) +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  expand_limits(y = max(plot_df$n) * 1.08)

# Save
out_png <- "services_barplot_by_type.png"
ggsave(out_png, p, width = 10, height = 8, dpi = 300)
cat("Saved:", out_png, "\n")


# Quick plot showing number of (unique) papers published each year.

# Outputs:
# - publications_per_year.png    (annual counts: bars + line)
# - publications_per_year_cum.png (cumulative count)

# Required packages:
# install.packages(c("tidyverse"))

library(tidyverse)

# ---- Read and clean year ----
df <- readr::read_csv(csv_file, guess_max = 2000, show_col_types = FALSE)

# Choose which column to use for year; prefer Pub_year, fallback to Study_year
year_col <- if ("Pub_year" %in% names(df)) "Pub_year" else if ("Study_year" %in% names(df)) "Study_year" else stop("No Pub_year or Study_year column found")

# Extract numeric year (handles cases where year field might be non-numeric or a range)
extract_year <- function(x) {
  # convert to character, extract first 4-digit group if present
  x_chr <- as.character(x)
  y <- str_extract(x_chr, "\\d{4}")
  as.integer(y)
}

df <- df %>%
  mutate(pub_year_raw = !!sym(year_col),
         pub_year = extract_year(pub_year_raw))

# Filter to plausible years (e.g. 1900 to current year + 1)
current_year <- as.integer(format(Sys.Date(), "%Y"))
df <- df %>%
  filter(!is.na(pub_year) & pub_year >= 1900 & pub_year <= (current_year + 1))

# ---- Count unique studies per year ----
# Use Study_no as unique identifier; if missing, use Title as fallback
if (!"Study_no" %in% names(df) || all(is.na(df$Study_no))) {
  df <- df %>% mutate(Study_no = row_number())
}

annual_counts <- df %>%
  distinct(Study_no, pub_year) %>%   # ensure each study counted only once in a year
  count(pub_year, name = "n") %>%
  arrange(pub_year)

# Print a quick table of counts
cat("Publication counts by year (first/last 10):\n")
print(head(annual_counts, 10))
print(tail(annual_counts, 10))

# ---- Plot: annual counts (bars + line) ----
p <- ggplot(annual_counts, aes(x = pub_year, y = n)) +
  geom_col(fill = "tan4", alpha = 0.85, width = 0.8) +
  geom_line(aes(x = pub_year, y = n), colour = "#2C7BB6", size = 0.5) +
  geom_point(colour = "#2C7BB6", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "",
       x = "Publication year", y = "Number of studies") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme(panel.grid.minor = element_blank())

out_png <- "publications_per_year.png"
ggsave(out_png, p, width = 10, height = 5, dpi = 300)
cat("Saved:", out_png, "\n")

# ---- Optional: cumulative curve ----
annual_counts <- annual_counts %>%
  arrange(pub_year) %>%
  mutate(cumulative = cumsum(n))

p_cum <- ggplot(annual_counts, aes(x = pub_year, y = cumulative)) +
  geom_area(fill = "#BDD7EA", alpha = 0.7) +
  geom_line(color = "#3182bd", size = 0.5) +
  theme_minimal(base_size = 14) +
  labs(title = "Cumulative number of papers over time",
       x = "Publication year", y = "Cumulative number of papers") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
  theme(panel.grid.minor = element_blank())

out_png2 <- "publications_per_year_cum.png"
ggsave(out_png2, p_cum, width = 10, height = 5, dpi = 300)
cat("Saved:", out_png2, "\n")

# ---- Quick extras (print top years) ----


