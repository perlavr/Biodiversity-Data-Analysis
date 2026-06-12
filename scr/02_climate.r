#############################################
# SCRIPT 02: EXTRACT CLIMATE DATA
#
# Objective:
# Add mean annual temperature and precipitation
# to Vespa occurrence records.
#
# Climate variables are extracted because temperature
# and precipitation influence insect survival, activity,
# reproduction, and invasion potential.
#############################################

# =========================
# 1) PACKAGES
# =========================

# install.packages("terra")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("geodata")

library(terra)
library(dplyr)
library(ggplot2)
library(readr)
library(geodata)

# =========================
# 2) STARTING DATASET
# =========================

# Load the cleaned occurrence dataset produced by Script 01.
# This dataset contains harmonised GBIF and iNaturalist records.
cur_data <- readr::read_csv(
  "data/processed/clean_occurrences.csv",
  show_col_types = FALSE
)

# Set a seed to make the random sampling reproducible.
set.seed(123)

# A subset of occurrence records is used because climate extraction
# can be computationally expensive.
# Records are sampled within each species to avoid one species dominating
# the climate dataset. This keeps the comparison between Vespa velutina
# and Vespa crabro more balanced.
species_df <- cur_data %>%

  # Keep only records with coordinates.
  dplyr::filter(!is.na(longitude), !is.na(latitude)) %>%

  # Sample records independently for each species.
  dplyr::group_by(species) %>%

  # Randomly shuffle records within each species.
  dplyr::slice_sample(prop = 1) %>% 

  # Keep a maximum of 2500 records per species.
  dplyr::slice_head(n = 2500) %>%  
  
  dplyr::ungroup() %>%
  
  # Create a unique identifier for each occurrence.
  # This ID is used to join extracted climate values back to the table.
  dplyr::mutate(occurrence_id = dplyr::row_number()) %>%

  # Keep only variables needed for climate extraction and later analyses.
  dplyr::select(
    occurrence_id,
    species,
    countryCode,
    location,
    source,
    longitude,
    latitude
  )

summary(species_df)
table(species_df$species)
table(species_df$source)

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

# =========================
# 3) CREATE A SPATIAL OBJECT
# =========================

# Climate extraction requires coordinates
# terra::vect() converts the occurrence table into a spatial vector object.
pts_v <- terra::vect(
  species_df,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Extract coordinates back from the spatial object and keep occurrence IDs.
coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  dplyr::rename(
    longitude = x,
    latitude = y
  ) %>%
  dplyr::mutate(
    occurrence_id = species_df$occurrence_id
  )

# Remove duplicated coordinates before climate extraction.
coords_unique <- coords_df %>%
  dplyr::distinct(longitude, latitude, .keep_all = TRUE)

# =========================
# 4) EXTRACT CLIMATE DATA
# =========================

# WorldClim bioclimatic variables are used as climate descriptors.
# bio1 = annual mean temperature, in °C * 10
# bio12 = annual precipitation, in mm

clim <- geodata::worldclim_global(
  var = "bio",
  res = 10,
  path = "data/raw"
)

clim_vars <- clim[[c("wc2.1_10m_bio_1", "wc2.1_10m_bio_12")]]

names(clim_vars) <- c("bio1_temp", "bio12_prec")

clim_extract <- terra::extract(
  clim_vars,
  pts_v
)

species_climate_df <- species_df %>%
  dplyr::bind_cols(clim_extract) %>%
  dplyr::mutate(
    tas_mean_c = bio1_temp / 10,
    prec_mean_annual = bio12_prec
  ) %>%
  dplyr::select(
    -ID,
    -bio1_temp,
    -bio12_prec
  )

# =========================
# 5) CHECK RESULT
# =========================

# Compare dimensions before and after adding climate variables.
print(dim(species_df))
print(dim(species_climate_df))

# Check final column names.
print(names(species_climate_df))

# Summarise extracted climate variables to detect missing or unrealistic values.
print(summary(species_climate_df$tas_mean_c))
print(summary(species_climate_df$prec_mean_annual))

# =========================
# 6) VALIDATION PLOTS
# =========================

# Validation plot for temperature.
# This plot checks whether both species have plausible temperature values
# and allows a first ecological comparison of thermal conditions.
plot_temp <- species_climate_df %>%
  dplyr::filter(!is.na(tas_mean_c)) %>%
  ggplot(aes(x = tas_mean_c, fill = species)) +
  geom_density(alpha = 0.4) +
  theme_classic() +
  labs(
    title = "Mean annual temperature at Vespa occurrence points",
    subtitle = "WorldClim bioclimatic data",
    x = "Mean annual temperature (°C)",
    y = "Density",
    fill = "Species"
  )

print(plot_temp)

# Validation plot for precipitation.
# This plot checks whether precipitation values are plausible
# and whether species occur under different moisture conditions.
plot_prec <- species_climate_df %>%
  dplyr::filter(!is.na(prec_mean_annual)) %>%
  ggplot(aes(x = prec_mean_annual, fill = species)) +
  geom_density(alpha = 0.4) +
  theme_classic() +
  labs(
    title = "Mean annual precipitation at Vespa occurrence points",
    subtitle = "WorldClim bioclimatic data",
    x = "Mean annual precipitation",
    y = "Density",
    fill = "Species"
  )

print(plot_prec)

# =========================
# 7) EXPORT
# =========================

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)

# Export the occurrence dataset enriched with climate variables.
readr::write_csv(
  species_climate_df,
  "data/processed/occurrences_with_climate.csv"
)

# Export validation figures.
ggsave(
  filename = "inst/figures/temp_distribution.png",
  plot = plot_temp,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "inst/figures/prec_distribution.png",
  plot = plot_prec,
  width = 8,
  height = 5,
  dpi = 300
)
