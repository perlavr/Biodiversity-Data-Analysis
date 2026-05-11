#############################################
# SCRIPT 03: EXTRACT ELEVATION DATA
#
# Objective:
# Add elevation values to Vespa occurrence records.
#
# Ecological rationale:
# Elevation influences local climate, vegetation structure,
# nesting opportunities, and potential range limits.
# Lower elevations are generally warmer and may be more suitable
# for Vespa velutina establishment, while Vespa crabro may tolerate
# a broader elevation range.
#############################################

# =========================
# 1) PACKAGES
# =========================

library(dplyr)
library(readr)
library(sf)
library(elevatr)
library(raster)
library(ggplot2)
library(rnaturalearth)

sf_use_s2(FALSE)

# =========================
# 2) LOAD CLIMATE DATASET
# =========================

# Load the occurrence dataset enriched with climate variables
# produced by the previous script, 02_climate.r.
species_climate_df <- readr::read_csv(
  "data/processed/occurrences_with_climate.csv",
  show_col_types = FALSE
)

# =========================
# 3) DEFINE EUROPE STUDY AREA
# =========================

# Load world country boundaries for mapping and spatial reference.
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define the same European bounding box used in the occurrence cleaning step.
# Keeping the same spatial extent ensures consistency across all scripts.
europe_bbox <- sf::st_bbox(
  c(xmin = -10, xmax = 30, ymin = 35, ymax = 70),
  crs = sf::st_crs(4326)
)

# Convert the bounding box into an sf polygon.
# elevatr::get_elev_raster() requires a spatial object as input.
europe_poly <- sf::st_sf(
  id = 1,
  geometry = sf::st_as_sfc(europe_bbox)
)

# =========================
# 4) DOWNLOAD ELEVATION RASTER
# =========================

# Download an elevation raster for the European study area.
#
# z controls the spatial resolution:
# - higher z = finer resolution but slower download and processing
# - z = 5 is used here because Europe is a large study area
#   and the goal is regional-scale environmental characterisation.
elevation_europe <- elevatr::get_elev_raster(
  locations = europe_poly,
  z = 5,
  clip = "locations"
)

# =========================
# 5) CONVERT OCCURRENCES TO SPATIAL POINTS
# =========================

# Convert occurrence coordinates to an sf point object.
# Coordinates are in WGS84 longitude/latitude (EPSG:4326).
points_sf <- sf::st_as_sf(
  species_climate_df,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# =========================
# 6) EXTRACT ELEVATION VALUES
# =========================

# Extract elevation values at each occurrence point.
# raster::extract() returns the raster cell value located under each point.
elevation_values <- raster::extract(
  elevation_europe,
  as(points_sf, "Spatial")
)

# Add extracted elevation values to the occurrence table.
species_climate_elev_df <- species_climate_df %>%
  mutate(
    elevation_m = as.numeric(elevation_values)
  )

# =========================
# 7) VALIDATION CHECKS
# =========================

print(summary(species_climate_elev_df$elevation_m))
print(table(species_climate_elev_df$species))

# =========================
# 8) VALIDATION PLOT
# =========================

# This plot compares elevation distributions between species.
# It is used as a diagnostic figure and as a first ecological comparison.
plot_elevation <- species_climate_elev_df %>%
  dplyr::filter(!is.na(elevation_m)) %>%
  ggplot(aes(x = elevation_m, fill = species)) +
  geom_density(alpha = 0.4, adjust = 1.5) +
  theme_classic() +
  labs(
    title = "Elevation distribution at Vespa occurrence points",
    subtitle = "Elevation extracted from raster data",
    x = "Elevation (m)",
    y = "Density",
    fill = "Species"
  )

print(plot_elevation)

# =========================
# 9) EXPORT
# =========================

# Export the dataset enriched with climate and elevation variables.
readr::write_csv(
  species_climate_elev_df,
  "data/processed/occurrences_with_climate_elevation.csv"
)

# Export the validation figure.
ggsave(
  filename = "inst/figures/elevation_distribution.png",
  plot = plot_elevation,
  width = 8,
  height = 5,
  dpi = 300
)
