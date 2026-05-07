# BHALL 07/05/2026 plot_world_map.R
#
#
# Creates a polished world choropleth with Europe inset
#
# Usage:
#  - Place Codebook_Kelp_Long.csv in the same folder
#  - Install required packages if needed:
#      install.packages(c("tidyverse","sf","rnaturalearth","rnaturalearthdata","countrycode","rcartocolor","ggspatial","cowplot"))
#  - Run:
#      source("plot_world_map.R")
#
# Output:
#  - kelp_studies_worldmap_with_eu_inset.png

install.packages("rcartocolor")

library(tidyverse)
library(rcartocolor)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(rcartocolor) # Carto palettes
library(ggspatial)
library(cowplot)     # for ggdraw() / draw_plot()

# ---- Config ----
csv_file <- "Codebook_Kelp_Long.csv"
out_file  <- "kelp_studies_worldmap_with_eu_inset.png"
img_width <- 12   # inches
img_height <- 6   # inches
dpi       <- 300

# ---- Read data ----
if (!file.exists(csv_file)) stop("CSV not found in working directory: ", csv_file)
df <- readr::read_csv(csv_file, guess_max = 2000, show_col_types = FALSE)

if (!"Study_no" %in% names(df)) stop("CSV must have a Study_no column")
# prefer Study_CoR; fall back to Study_Country if Study_CoR missing or blank
country_source_col <- if ("Study_CoR" %in% names(df)) "Study_CoR" else if ("Study_Country" %in% names(df)) "Study_Country" else NULL
if (is.null(country_source_col)) stop("CSV must include Study_CoR or Study_Country column")

# ---- Normalize raw country strings ----
df <- df %>%
  mutate(country_raw = coalesce(as.character(.data[[country_source_col]]), "") %>% str_squish())

# After reading CSV and before country normalization:
df <- df %>%
  separate_rows(country_raw, sep = ";|,") %>%  
  mutate(country_raw = str_squish(country_raw))

normalize_country <- function(x) {
  x <- ifelse(is.na(x), "", str_squish(as.character(x)))
  case_when(
    x == "" ~ NA_character_,
    str_detect(x, regex("\\bUnited States\\b|\\bUSA\\b|United States of America|U\\.S\\.|U\\.S\\.A\\.", ignore_case = TRUE)) ~ "United States",
    str_detect(x, regex("\\bUnited Kingdom\\b|\\bUK\\b|Great Britain|United Kingdon", ignore_case = TRUE)) ~ "United Kingdom",
    str_detect(x, regex("\\bRepublic of Korea\\b|\\bKorea\\b", ignore_case = TRUE)) ~ "South Korea",
    str_detect(x, regex("\\bRussia\\b|Russian Federation|Russian", ignore_case = TRUE)) ~ "Russia",
    str_detect(x, regex("\\bBoth\\b|\\bGlobal\\b|\\bRegional\\b|\\bN/A\\b|\\bNot specified\\b|\\bBaltic Sea\\b", ignore_case = TRUE)) ~ NA_character_,
    TRUE ~ countrycode(x, origin = "country.name", destination = "country.name", warn = FALSE)
  )
}

df <- df %>% mutate(country = normalize_country(country_raw))

# print unmapped raw strings so you can correct the mapping if needed
unmapped <- df %>% filter(is.na(country) & country_raw != "") %>% distinct(country_raw) %>% pull(country_raw)
if (length(unmapped) > 0) {
  message("Unmapped country strings (check / extend normalize_country()):")
  print(unmapped)
} else {
  message("All country strings mapped (or blank).")
}

# ---- Count unique studies (Study_no) per mapped country ----
counts <- df %>%
  filter(!is.na(country)) %>%
  distinct(Study_no, country) %>%  # one study counted once per country
  count(country, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c", warn = FALSE))

message("Top countries by unique Study_no:")
print(head(counts, 20))

# ---- Load world shapes and join counts ----
world <- ne_countries(scale = "medium", returnclass = "sf") %>% select(iso_a3, name_long, geometry)

# FIX: Correct known ISO code issues in Natural Earth data
world <- world %>%
  mutate(iso_a3 = case_when(
    name_long == "Norway" ~ "NOR",
    name_long == "France" ~ "FRA",
    name_long == "Somaliland" ~ "SOM",
    TRUE ~ iso_a3
  ))

world_counts <- world %>%
  left_join(counts, by = c("iso_a3" = "iso3")) %>%
  mutate(n = replace_na(n, 0))

# ---- Define native range of Saccharina latissima and Laminaria hyperborea ----
# Species occur in temperate and polar North Atlantic/North Pacific waters
native_range_countries <- c(
  # North Atlantic
  "Norway", "Iceland", "United Kingdom", "Ireland", "France", "Spain", 
  "Portugal", "Germany", "Denmark", "Sweden", "Canada", "United States",
  # Arctic/Subarctic
  "Russia", "Greenland", "Faroe Islands",
  #North Pacific
  "Japan","Republic of Korea", 
  # Additional temperate coasts where they occur
  "Netherlands", "Belgium"
)

# Create a column indicating if country is in native range
world_counts <- world_counts %>%
  mutate(in_native_range = name_long %in% native_range_countries)

#### Plotting section ####

# projection for display
robinson <- sf::st_crs("+proj=robin")

# graticule settings (lon/lat in degrees)
lon_breaks <- seq(-180, 180, by = 60)
lat_breaks <- seq(-60, 80, by = 30)

# helper to create meridian/parallels as sf lines in lon/lat then transform to target CRS
make_meridian <- function(lon, lat_range = c(-90, 90), crs_out = robinson) {
  coords <- matrix(c(rep(lon, 2), lat_range), ncol = 2)
  s <- st_sfc(st_linestring(coords), crs = 4326)
  st_transform(s, crs_out)
}
make_parallel <- function(lat, lon_range = c(-180, 180), crs_out = robinson) {
  coords <- matrix(c(lon_range, rep(lat, 2)), ncol = 2)
  s <- st_sfc(st_linestring(coords), crs = 4326)
  st_transform(s, crs_out)
}

# build graticule sf objects
meridians <- do.call(c, lapply(lon_breaks, make_meridian))
parallels <- do.call(c, lapply(lat_breaks, make_parallel))

meridians_sf <- st_sf(geometry = meridians)
parallels_sf  <- st_sf(geometry = parallels)

# build label points (in lon/lat), then transform to robinson for plotting
lon_label_lat <- min(lat_breaks) + 4
lon_label_points <- tibble(lon = lon_breaks, lat = lon_label_lat,
                           label = paste0(abs(lon_breaks), "°", ifelse(lon_breaks > 0, "E", ifelse(lon_breaks < 0, "W", "")))) %>%
  mutate(label = if_else(lon == 0, "0°", label))
lon_label_sf <- st_as_sf(lon_label_points, coords = c("lon", "lat"), crs = 4326) %>% st_transform(robinson)

lat_label_lon <- min(lon_breaks) + 4
lat_label_points <- tibble(lat = lat_breaks, lon = lat_label_lon,
                           label = paste0(abs(lat_breaks), "°", ifelse(lat_breaks > 0, "N", ifelse(lat_breaks < 0, "S", "")))) %>%
  mutate(label = if_else(lat == 0, "0°", label))
lat_label_sf <- st_as_sf(lat_label_points, coords = c("lon", "lat"), crs = 4326) %>% st_transform(robinson)

# ---- colours / appearance ----
ocean_fill <- "white"   # muted blue for ocean
land_grey  <- "#efefef"   # neutral land base for countries with no data
country_border <- "#9a9a9a" # thin border for all countries
inset_border_col <- "#6b6b6b"
world_outline <- "#4a4a4a" # NEW: dark grey for world contour


# Load world outline (simplified coastline)
world_outline_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_union() %>%  # merge all countries into single geometry
  st_transform(robinson)


# Monochromatic colour gradient (light to dark blue)
colour_low <- "#e8f4f8"   # very light blue
colour_high <- "#08519c"  # dark blue


# 1. Create the bounding box (the "globe" outline)
# This creates a rectangle from -180 to 180 longitude and -90 to 90 latitude
globe_outline <- st_bbox(c(xmin = -180, xmax = 180, ymax = 90, ymin = -90), 
                         crs = st_crs(4326)) %>% 
  st_as_sfc() %>% 
  st_segmentize(units::set_units(1, "degree")) # Ensures the lines curve smoothly


# Main map (base):
p_base <- ggplot() +

  
  # Countries OUTSIDE native range - darker grey
  geom_sf(data = filter(world_counts, !in_native_range), 
          fill = "grey80", colour = country_border, size = 0.12) +
  # Countries INSIDE native range but with no studies - light grey
  geom_sf(data = filter(world_counts, in_native_range & n == 0), 
          fill = land_grey, colour = country_border, size = 0.12) +
  # Countries with studies - coloured gradient
  geom_sf(data = filter(world_counts, n > 0), 
          aes(fill = n), colour = country_border, size = 0.16) +

  # graticules (in projected coordinates)
  geom_sf(data = meridians_sf, colour = "grey80", size = 0.25) +
  geom_sf(data = parallels_sf,  colour = "grey80", size = 0.25) +
  # labels for graticule
  geom_sf(data = lon_label_sf, aes(geometry = geometry), inherit.aes = FALSE, alpha = 0) +
  geom_sf_text(data = lon_label_sf, aes(label = label, geometry = geometry), stat = "sf_coordinates",
               size = 3.0, color = "grey20", nudge_y = -0.02) +
  geom_sf_text(data = lat_label_sf, aes(label = label, geometry = geometry), stat = "sf_coordinates",
               size = 3.0, color = "grey20", hjust = 0) +
  # Monochromatic continuous scale
  scale_fill_gradient(low = colour_low, high = colour_high, na.value = land_grey,
                      name = "Number of studies", breaks = scales::pretty_breaks(n = 6)) +
  coord_sf(crs = robinson) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = ocean_fill, colour = NA),
    plot.background  = element_rect(fill = ocean_fill, colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.key = element_rect(fill = "white", colour = NA)
  ) +
  labs(x = NULL, y = NULL)


# Europe inset: lon/lat bbox transformed to plotting CRS (Robinson) as before
eu_xlim <- c(-12, 40)
eu_ylim <- c(34, 72)

eu_bbox_ll <- st_as_sfc(st_bbox(c(xmin = eu_xlim[1], xmax = eu_xlim[2], ymin = eu_ylim[1], ymax = eu_ylim[2]), crs = st_crs(4326)))
eu_bbox_proj <- st_transform(eu_bbox_ll, robinson)
eu_bbox_nums <- st_bbox(eu_bbox_proj)
pad_frac <- 0.02
xpad <- (eu_bbox_nums["xmax"] - eu_bbox_nums["xmin"]) * pad_frac
ypad <- (eu_bbox_nums["ymax"] - eu_bbox_nums["ymin"]) * pad_frac
xlim_proj <- c(eu_bbox_nums["xmin"] - xpad, eu_bbox_nums["xmax"] + xpad)
ylim_proj <- c(eu_bbox_nums["ymin"] - ypad, eu_bbox_nums["ymax"] + ypad)

# Europe inset (same changes):
p_eu <- ggplot() +
  
  # Outside native range - darker grey
  geom_sf(data = filter(world_counts, !in_native_range), 
          fill = "#d4d4d4", colour = country_border, size = 0.10) +
  # Inside native range but no studies - light grey
  geom_sf(data = filter(world_counts, in_native_range & n == 0), 
          fill = land_grey, colour = country_border, size = 0.10) +
  # With studies - coloured 
  geom_sf(data = filter(world_counts, n > 0), aes(fill = n), colour = country_border, size = 0.12) +
  geom_sf(data = meridians_sf, colour = "grey80", size = 0.14) +
  geom_sf(data = parallels_sf,  colour = "grey80", size = 0.14) +
  scale_fill_gradient(low = colour_low, high = colour_high, na.value = land_grey, guide = "none") +
  coord_sf(crs = robinson, xlim = xlim_proj, ylim = ylim_proj, expand = FALSE) +
  theme_void() +
  # Draw the border on top by adding it as a separate layer AFTER the maps
  annotate("rect", 
           xmin = xlim_proj[1], xmax = xlim_proj[2], 
           ymin = ylim_proj[1], ymax = ylim_proj[2],
           fill = NA, colour = inset_border_col, size = 1.2) +
  theme(
    panel.background = element_rect(fill = "white", colour = inset_border_col, size = 0.8),
    plot.margin = unit(rep(4, 4), "pt")
  )

# Compose final figure with cowplot: place inset over the main map
final_plot <- ggdraw() +
  draw_plot(p_base, 0, 0, 1, 1) +
  draw_plot(p_eu, x = 0.06, y = 0.30, width = 0.30, height = 0.30)  # adjust inset position/size

# Save
ggsave(out_file, final_plot, width = img_width, height = img_height, dpi = dpi)
message("Saved: ", out_file)

