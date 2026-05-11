################################################################################
# SCRIPT 05: ADD MODIS NDVI DATA TO VESPA OCCURRENCE POINTS
#
# Objective:
# Add satellite-derived NDVI values to Vespa occurrence records using
# MODIS data extracted with NASA AppEEARS.
#
# Ecological rationale:
# NDVI is a proxy for vegetation productivity. Vegetation structure may
# influence prey availability, nesting resources, habitat suitability,
# and landscape conditions relevant to Vespa species.
################################################################################

# =========================
# 1) PACKAGES
# =========================

library(readr)
library(dplyr)
library(ggplot2)

# =========================
# 2) LOAD ENVIRONMENTAL DATASET
# =========================

# Load the dataset enriched with climate, elevation and ecosystem variables.
matrix_full_eco <- readr::read_csv(
  "data/processed/occurrences_with_climate_elevation_ecosystem.csv",
  show_col_types = FALSE
)

# =========================
# 3) CREATE BALANCED POINT FILE FOR APPEEARS
# =========================

# This file must be uploaded manually to AppEEARS.
# The id column is a new AppEEARS point identifier.
# occurrence_id is kept to join NDVI values back to the original dataset.
set.seed(123)

points_app <- matrix_full_eco %>%
  dplyr::filter(!is.na(longitude), !is.na(latitude)) %>%
  dplyr::group_by(species) %>%
  dplyr::slice_sample(prop = 1) %>%
  dplyr::slice_head(n = 500) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(appeears_id = dplyr::row_number()) %>%
  dplyr::select(
    id = appeears_id,
    occurrence_id,
    latitude,
    longitude
  )

dir.create("data/modis", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(
  points_app,
  "data/modis/points_appEEARS_1000.csv"
)

print(table(
  matrix_full_eco$species[
    match(points_app$occurrence_id, matrix_full_eco$occurrence_id)
  ]
))

# =========================
# 4) LOAD APPEEARS MODIS NDVI RESULTS
# =========================

# Product used in AppEEARS:
# MOD13A1.061
# Layer:
# 500m 16 days NDVI
#
# This file must be downloaded manually from AppEEARS after uploading
# data/modis/points_appEEARS_1000.csv.

ndvi_raw <- readr::read_csv(
  "data/modis/Europe-MOD13A1-061-results.csv",
  show_col_types = FALSE
)

# =========================
# 5) CLEAN NDVI VALUES
# =========================


# AppEEARS returns multiple NDVI values per point because MODIS provides
# 16-day composites over the selected time period.
# We calculate the mean NDVI per uploaded point ID.
#
# Values are already scaled between 0 and 1 in this AppEEARS output,
# so no MODIS scale factor is applied.

ndvi_clean <- ndvi_raw %>%
  dplyr::mutate(
    NDVI = as.numeric(MOD13A1_061__500m_16_days_NDVI)
  ) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    NDVI = mean(NDVI, na.rm = TRUE),
    .groups = "drop"
  )

# =========================
# 6) JOIN NDVI BACK TO OCCURRENCES
# =========================

points_ndvi <- points_app %>%
  dplyr::left_join(
    ndvi_clean,
    by = c("id" = "ID")
  )

matrix_full_eco_ndvi <- matrix_full_eco %>%
  dplyr::left_join(
    points_ndvi %>%
      dplyr::select(occurrence_id, NDVI),
    by = "occurrence_id"
  ) %>%
  dplyr::filter(!is.na(NDVI), NDVI > 0, NDVI < 1)

# =========================
# 7) VALIDATION CHECKS
# =========================

print(summary(matrix_full_eco_ndvi$NDVI))
print(table(matrix_full_eco_ndvi$species))

# =========================
# 8) VALIDATION PLOT
# =========================

plot_ndvi <- matrix_full_eco_ndvi %>%
  ggplot(aes(x = NDVI, fill = species, color = species)) +
  geom_density(alpha = 0.3, linewidth = 1.1, adjust = 1.5) +
  theme_classic() +
  labs(
    title = "NDVI distribution at Vespa occurrence points",
    subtitle = "MODIS MOD13A1 NDVI, AppEEARS point extraction",
    x = "Mean NDVI",
    y = "Density",
    fill = "Species",
    color = "Species"
  )

print(plot_ndvi)

# =========================
# 9) EXPORT
# =========================

readr::write_csv(
  matrix_full_eco_ndvi,
  "data/processed/final_environmental_dataset_with_ndvi.csv"
)

ggsave(
  filename = "inst/figures/ndvi_distribution.png",
  plot = plot_ndvi,
  width = 8,
  height = 5,
  dpi = 300
)
