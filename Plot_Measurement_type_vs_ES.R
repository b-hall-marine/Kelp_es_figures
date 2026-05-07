#  BHALL 07/05/2026 ES_vs_Measurement_Type_Heatmap.R
#
# PURPOSE:
#   Generate a species-stratified heatmap comparing Ecosystem Services (ES) 
#   against ES Measurement Types.
#
# KEY FEATURES:
#   - Data reshaped from wide to long format
#   - Standardised measurement type categories (e.g., "Biomass or Harvest",
#     "Chemical studies or product", "Water Quality")
#   - Three-panel faceted plot (one per species group)
#   - Zebra shading for improved readability
#   - Heatmap tiles sized by sqrt(count) for visual emphasis on differences
#   - Color gradient from yellow (low) to purple (high)
#
# OUTPUTS:
#   - ES_vs_Measurement_Type_heatmap.png
#   - ES_vs_Measurement_Type_heatmap.pdf
#   - ES_vs_Measurement_Type_summary.csv
#
# USAGE:
#   Place Codebook_Kelp_Long.csv in working directory, then:
#   source("ES_vs_Measurement_Type_Heatmap.R")
#
# NOTES:
#   - Tile counts represent the number of distinct studies (Study_no)
#   - Empty cells indicate no studies for that ES-Measurement combination
#   - X-axis ordered by total measurement type frequency (descending)
#   - Y-axis ordered by total ES frequency (ascending)
# ============================================================================

library(tidyverse)
library(stringr)

# 1. Load Data
df <- read_csv("Codebook_Kelp_Long.csv")

# 2. Reshape and Clean
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

# 3. Spelling Corrections & Consolidation
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

# 4. Aggregate
plot_data <- df_cleaned %>%
  group_by(Species_Group, Measurement_Type, ES) %>%
  summarise(count = n_distinct(Study_no), .groups = 'drop')

# Order ES (Y-axis) and Measurement_Type (X-axis)
es_order <- plot_data %>% 
  group_by(ES) %>% 
  summarise(total = sum(count)) %>% 
  arrange(total) %>% 
  pull(ES)

measurement_order <- plot_data %>% 
  group_by(Measurement_Type) %>% 
  summarise(total = sum(count)) %>% 
  arrange(desc(total)) %>% 
  pull(Measurement_Type)

plot_data$ES <- factor(plot_data$ES, levels = es_order)
plot_data$Measurement_Type <- factor(plot_data$Measurement_Type, levels = measurement_order)

# Zebra Shading Data
shading_data <- tibble(ES = levels(plot_data$ES)) %>%
  mutate(y_idx = row_number()) %>%
  filter(y_idx %% 2 == 0) %>%
  mutate(ymin = y_idx - 0.5, ymax = y_idx + 0.5)

# 5. Final Plot
ggplot() +
  # 1. Background Zebra Stripes
  geom_rect(data = shading_data, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "grey96", alpha = 0.7, inherit.aes = FALSE) +
  # 2. Heatmap Tiles
  geom_tile(data = plot_data, aes(x = Measurement_Type, y = ES, fill = count), color = "white", size = 0.5) +
  # 3. Numeric labels
  geom_text(data = plot_data, aes(x = Measurement_Type, y = ES, label = count), size = 2.8) +
  # 4. Color Scale
  scale_fill_gradientn(colours = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#5E4FA2"),
                       name = "Papers (n)", trans = "sqrt") +
  # 5. Facet by Species
  facet_grid(. ~ Species_Group, scales = "free_x", space = "free_x") +
  theme_minimal() +
  labs(x = "ES Measurement Type", 
       y = "Ecosystem Service") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9),
    strip.background = element_rect(fill = "grey90", color = "white"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90", size = 0.2), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5)
  )

# Optional: Save the plot
ggsave("ES_vs_Measurement_Type_heatmap.png", width = 14, height = 10, dpi = 300)
ggsave("ES_vs_Measurement_Type_heatmap.pdf", width = 14, height = 10)

cat("Plot saved as ES_vs_Measurement_Type_heatmap.png and .pdf\n")

# Create a summary table version
summary_table <- df_cleaned %>%
  group_by(ES, Measurement_Type) %>%
  summarise(
    n_studies = n_distinct(Study_no),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Measurement_Type, values_from = n_studies, values_fill = 0) %>%
  arrange(ES)

write_csv(summary_table, "ES_vs_Measurement_Type_summary.csv")
