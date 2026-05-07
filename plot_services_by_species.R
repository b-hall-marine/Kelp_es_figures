# BH 07/05/2026 plot_services_by_species.R
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

