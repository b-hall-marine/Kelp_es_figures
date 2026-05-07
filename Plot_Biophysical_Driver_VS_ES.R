# BH 07/05/2026 Plot_Biophysical_Drivers_VS_ES.R
#
# PURPOSE:
#   Generate a species heatmap comparing Biophysical Drivers 
#   against Ecosystem Services (ES). This visualization identifies which
#   environmental drivers are most commonly studied in relation to each
#   ecosystem service across three kelp species groupings (Saccharina latissima,
#   Laminaria hyperborea, and Both).
#
# KEY FEATURES:
#   - Data reshaped from wide to long format
#   - Standardized biophysical driver categories
#   - Three-panel faceted plot (one per species group)
#   - Zebra shading for improved readability
#   - Heatmap tiles sized by sqrt(count) for visual emphasis on differences
#   - Color gradient from yellow (low) to purple (high)
#   - Vertical gridlines to aid cross-panel comparison
#
# OUTPUTS:
#   - Interactive heatmap visualization displayed in R
#   - Can be saved using ggsave() for PNG/PDF export
#
# USAGE:
#   Place Codebook_Kelp_Long.csv in working directory, then:
#   source("Plot_Biophysical_Drivers_VS_ES.R")
#
# NOTES:
#   - Tile counts represent the number of distinct studies (Study_no)
#   - Empty cells indicate no studies examining that driver-ES combination
#   - X-axis (drivers) automatically ordered by panel; Y-axis (ES) ordered
#     by total frequency (ascending) across all species
#   - Useful for identifying research gaps and research concentration areas
#   - Supports identification of species-specific driver-ES research patterns
# ============================================================================


library(tidyverse)
library(stringr)

# 1. Load Data
df <- read_csv("Codebook_Kelp_Long.csv")

# 2. Reshape and Clean
df_long <- df %>%
  pivot_longer(cols = starts_with("ES_Name"), values_to = "ES", names_to = "ES_col") %>%
  pivot_longer(cols = starts_with("Bio_physical_Driver"), values_to = "Driver", names_to = "Driver_col") %>%
  filter(!is.na(ES), !is.na(Driver), ES != "", Driver != "") %>%
  mutate(
    ES = str_trim(ES) %>% str_squish(),
    Driver = str_trim(Driver) %>% str_squish(),
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
    Driver = if_else(str_detect(Driver, "(?i)Nutrient"), "Nutrient availability", Driver)
  )

# 4. Aggregate
plot_data <- df_cleaned %>%
  group_by(Species_Group, Driver, ES) %>%
  summarise(count = n_distinct(Study_no), .groups = 'drop')

# Order ES (Y-axis) and Drivers (X-axis)
es_order <- plot_data %>% group_by(ES) %>% summarise(total = sum(count)) %>% arrange(total) %>% pull(ES)
plot_data$ES <- factor(plot_data$ES, levels = es_order)

# Zebra Shading Data
shading_data <- tibble(ES = levels(plot_data$ES)) %>%
  mutate(y_idx = row_number()) %>%
  filter(y_idx %% 2 == 0) %>%
  mutate(ymin = y_idx - 0.5, ymax = y_idx + 0.5)

# 5. Final Plot with Vertical Lines
ggplot() +
  # 1. Background Zebra Stripes (Horizontal detailing)
  geom_rect(data = shading_data, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
            fill = "grey90", alpha = 0.7, inherit.aes = FALSE) +
  # 2. Heatmap Tiles (White borders create the "cells")
  geom_tile(data = plot_data, aes(x = Driver, y = ES, fill = count), color = "white", size = 0.5) +
  # 3. Numeric labels
  geom_text(data = plot_data, aes(x = Driver, y = ES, label = count), size = 2.8) +
  # 4. Color Scale
  scale_fill_gradientn(colours = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#5E4FA2"),
                        name = "Papers (n)", trans = "sqrt") +
  # 5. Facet by Species
  facet_grid(. ~ Species_Group, scales = "free_x", space = "free_x") +
  theme_minimal() +
  labs(title =,
       x = "Biophysical Driver", y = "Ecosystem Service") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9),
    strip.background = element_rect(fill = "grey90", color = "white"),
    strip.text = element_text(face = "bold"),
    # ADDING THE VERTICAL DETAILING HERE:
    panel.grid.major.x = element_line(color = "grey90", size = 0.2), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    # Ensure the panel border is visible to separate species clearly
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5)
  )

