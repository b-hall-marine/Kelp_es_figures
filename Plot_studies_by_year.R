# BHALL 07/05/2026  plot_studies_by_year.R (OPTION 2: Multiple ES per article, fully clarified)
#
# Creates a barchart and .csv of the articles studying kelp ES throughout time
#
# Usage:
#  - Place Codebook_Kelp_Long.csv in the same folder
#  - Install required packages if needed:
#      install.packages(c("tidyverse","sf","rnaturalearth","rnaturalearthdata","countrycode","rcartocolor","ggspatial","cowplot"))
#  - Run:
#      source("plot_world_map.R")
#
# Output:figures/studies_by_year_stacked_topN_other_cumulative.svg
#  - 
library(tidyverse)
library(stringr)
library(forcats)
library(viridis)
library(scales)

# ---- Config ----
top_n <- 10
drop_year <- 2025
min_year <- 1900
max_year <- 2026

# ---- Read data ----
data <- read_csv("Codebook_Kelp_Long.csv", show_col_types = FALSE)

# ---- Extract year ----
data_clean <- data %>%
  mutate(Pub_year_char = str_extract(as.character(Pub_year), "\\d{4}"),
         Pub_year_num = as.integer(Pub_year_char)) %>%
  filter(!is.na(Pub_year_num) & Pub_year_num >= min_year & Pub_year_num <= max_year) %>%
  filter(Pub_year_num != drop_year)

message("Years retained: ", min(data_clean$Pub_year_num), " to ", max(data_clean$Pub_year_num))

# ---- Calculate statistics about multi-ES articles ----
es_per_study <- data_clean %>%
  select(Study_no, starts_with("ES_Name")) %>%
  distinct() %>%
  pivot_longer(cols = starts_with("ES_Name"),
               names_to = "ES_type",
               values_to = "ES_name") %>%
  mutate(ES_name = str_squish(ES_name)) %>%
  filter(!is.na(ES_name), ES_name != "") %>%
  group_by(Study_no) %>%
  summarise(n_es = n(), .groups = "drop")

mean_es <- mean(es_per_study$n_es)
max_es <- max(es_per_study$n_es)
pct_multi <- 100 * sum(es_per_study$n_es > 1) / n_distinct(es_per_study$Study_no)

message(sprintf("Average ES per article: %.2f", mean_es))
message(sprintf("Maximum ES per article: %d", max_es))
message(sprintf("Articles addressing multiple ES: %.1f%%", pct_multi))

# ---- Cumulative unique studies per year ----
studies_per_year <- data_clean %>%
  distinct(Study_no, .keep_all = TRUE) %>%
  count(Pub_year_num, name = "n_studies") %>%
  arrange(Pub_year_num) %>%
  mutate(cumulative = cumsum(n_studies))

# ---- ALL ES per study (keep all ES mentions) ----
es_long <- data_clean %>%
  select(Study_no, Pub_year_num, starts_with("ES_Name")) %>%
  pivot_longer(cols = starts_with("ES_Name"),
               names_to = "ES_type",
               values_to = "ES_name") %>%
  mutate(ES_name = str_squish(ES_name)) %>%
  filter(!is.na(ES_name), ES_name != "")

# ---- Count ES mentions per year ----
es_counts_raw <- es_long %>%
  count(Pub_year_num, ES_name, name = "count")

# ---- Top N + Other ----
es_totals <- es_counts_raw %>%
  group_by(ES_name) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

top_names <- head(es_totals$ES_name, top_n)

es_counts <- es_counts_raw %>%
  mutate(ES_group = if_else(ES_name %in% top_names, ES_name, "Other")) %>%
  group_by(Pub_year_num, ES_group) %>%
  summarise(count = sum(count), .groups = "drop")

# ---- Order groups ----
group_totals <- es_counts %>%
  group_by(ES_group) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

ordered_groups <- as.character(group_totals$ES_group)
if ("Other" %in% ordered_groups) {
  ordered_groups <- setdiff(ordered_groups, "Other")
  ordered_groups <- c(ordered_groups, "Other")
}

es_counts <- es_counts %>%
  mutate(ES_group = factor(ES_group, levels = ordered_groups))

# ---- Year levels & fill missing years ----
year_levels <- sort(unique(c(es_counts$Pub_year_num, studies_per_year$Pub_year_num)))
es_counts <- es_counts %>% mutate(Pub_year = factor(Pub_year_num, levels = year_levels))

studies_per_year <- tibble(Pub_year_num = year_levels) %>%
  left_join(studies_per_year, by = "Pub_year_num") %>%
  mutate(n_studies = replace_na(n_studies, 0)) %>%
  arrange(Pub_year_num) %>%
  mutate(cumulative = cumsum(n_studies),
         Pub_year = factor(Pub_year_num, levels = year_levels))

# ---- Colors ----
n_groups <- n_distinct(es_counts$ES_group)
pal <- viridis(n_groups, option = "turbo") %>% as.character()

# ---- Scaling ----
max_yearly_es <- max(es_counts$count, na.rm = TRUE)
max_cumulative <- max(studies_per_year$cumulative, na.rm = TRUE)
if (max_cumulative == 0) max_cumulative <- 1
if (max_yearly_es == 0) max_yearly_es <- 1

scale_factor <- max_yearly_es / max_cumulative
inv_scale_factor <- max_cumulative / max_yearly_es

# ---- Caption with statistics ----
# caption_text <- sprintf(
# "Each article can address multiple ecosystem services. Average: %.1f ES per article (max: %d). %d%% of articles address >1 ES.",
#  mean_es, max_es, round(pct_multi)
#  )

# ---- Plot ----
p <- ggplot() +
  geom_col(data = es_counts, aes(x = Pub_year, y = count, fill = ES_group), 
           width = 0.8, colour = NA) +
  geom_line(data = studies_per_year, aes(x = Pub_year, y = cumulative * scale_factor, group = 1),
            colour = "black", size = 0.9, linetype = "solid") +
  geom_point(data = studies_per_year, aes(x = Pub_year, y = cumulative * scale_factor),
             colour = "black", size = 0.0, fill = "white", stroke = 1) +
  scale_fill_manual(values = pal, name = "Ecosystem Service") +
  scale_y_continuous(
    name = "Number of ES mentions per year",
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * inv_scale_factor, 
                        name = "Cumulative unique articles")
  ) +
  labs(
    x = "Publication Year",
    # title = "Ecosystem Services in Kelp Literature",
    # subtitle = "Stacked bars show ES mentions (articles can address multiple services); Line shows cumulative unique articles.",
    # caption = caption_text
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    legend.position = c(0.45, 0.75),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 9),
    # plot.title = element_text(face = "bold", size = 13),
    # plot.subtitle = element_text(size = 10, face = "italic"),
    # plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 10))
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = FALSE))

# ---- Save ----
if (!dir.exists("figures")) dir.create("figures")
ggsave("figures/studies_by_year_stacked_topN_other_cumulative.svg", p, width = 12, height = 6)
ggsave("figures/studies_by_year_stacked_topN_other_cumulative.png", p, width = 12, height = 6, dpi = 1000)

message("Saved: figures/studies_by_year_stacked_topN_other_cumulative.{svg,png}")
