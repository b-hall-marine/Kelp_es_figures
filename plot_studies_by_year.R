# Load required libraries
library(tidyverse)
library(readr) 

# Load your dataset
data <- read_csv("Codebook_Kelp_Long.csv")

# Reshape ES columns into long format
es_long <- data %>%
  select(Pub_year, starts_with("ES_Name")) %>%
  pivot_longer(cols = starts_with("ES_Name"),
               names_to = "ES_type",
               values_to = "ES_name") %>%
  filter(!is.na(ES_name), !is.na(Pub_year))

# Count number of ES mentions per year
es_counts <- es_long %>%
  group_by(Pub_year, ES_name) %>%
  summarise(count = n(), .groups = "drop")

# Plot stacked bar chart
ggplot(es_counts, aes(x = factor(Pub_year), y = count, fill = ES_name)) +
  geom_bar(stat = "identity") +
  labs(title = "",
       x = "Publication Year",
       y = "Number of studies",
       fill = "Ecosystem Service") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
