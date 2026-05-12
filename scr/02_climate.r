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

library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)
library(readr)

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

# =========================
# 3) CREATE A SPATIAL OBJECT
# =========================

# CHELSA extraction requires coordinates.
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
# 4) EXTRACT MONTHLY TEMPERATURE
# =========================

# CHELSA variable:
# tas = near-surface air temperature.
#
# Data are extracted for 2020 because this year matches the recent
# occurrence period and is consistent with the MODIS NDVI extraction.
tas_r <- getChelsa(
  var       = "tas",
  coords    = coords_unique %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2021-01-01"),
  dataset   = "chelsa-monthly"
)

# Remove the time column and convert monthly values to a matrix.
tas_mat <- tas_r %>%
  dplyr::select(-time) %>%
  as.matrix()

# Calculate the annual mean temperature for each point.
# CHELSA temperature is returned in Kelvin, so it is converted to Celsius.
tas_mean_c <- colMeans(tas_mat, na.rm = TRUE) - 273.15


# Store temperature values in a table linked by occurrence ID.
tas_df_unique <- data.frame(
  occurrence_id = coords_unique$occurrence_id,
  tas_mean_c = tas_mean_c
)

# =========================
# 5) EXTRACT MONTHLY PRECIPITATION
# =========================

# CHELSA variable:
# pr = precipitation.
#
# Precipitation is included because humidity and water availability
# influence habitat suitability, vegetation, and prey availability.
prec_r <- getChelsa(
  var       = "pr",
  coords    = coords_unique %>% dplyr::select(longitude, latitude),
  startdate = as.Date("2020-01-01"),
  enddate   = as.Date("2021-01-01"),
  dataset   = "chelsa-monthly"
)

# Remove the time column and convert monthly values to a matrix.
prec_mat <- prec_r %>%
  dplyr::select(-time) %>%
  as.matrix()

# Calculate mean precipitation across the 12 monthly values.
prec_mean <- colMeans(prec_mat, na.rm = TRUE)

# Store precipitation values in a table linked by occurrence ID.
prec_df_unique <- data.frame(
  occurrence_id = coords_unique$occurrence_id,
  prec_mean_annual = as.numeric(prec_mean)
)

# =========================
# 6) JOIN CLIMATE VARIABLES
# =========================

# Join the extracted temperature and precipitation variables
# back to the occurrence dataset using occurrence_id.
species_climate_df <- species_df %>%
  dplyr::left_join(tas_df_unique, by = "occurrence_id") %>%
  dplyr::left_join(prec_df_unique, by = "occurrence_id")

# =========================
# 7) CHECK RESULT
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
# 8) VALIDATION PLOTS
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
    subtitle = "CHELSA monthly climate data, 2020",
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
    subtitle = "CHELSA monthly climate data, 2020",
    x = "Mean annual precipitation",
    y = "Density",
    fill = "Species"
  )

print(plot_prec)

# =========================
# 9) EXPORT
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
