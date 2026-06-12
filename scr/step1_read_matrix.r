#############################################
# STEP 1: READ FINAL ENVIRONMENTAL MATRIX
#
# Ecological question:
# Does the invasive hornet Vespa velutina occupy
# different environmental conditions compared to
# the native Vespa crabro across Europe?
#
# Objective:
# Load the final environmental matrix produced
# during the intermediate project and prepare it
# for the final analyses.
#############################################

library(dplyr)
library(readr)
library(ggplot2)

matrix_full <- readr::read_csv(
  "data/matrix_full.csv",
  show_col_types = FALSE
)

print(names(matrix_full))
print(dim(matrix_full))
print(table(matrix_full$species))

env_vars <- c(
  "tas_mean_c",
  "prec_mean_annual",
  "elevation_m",
  "NDVI"
)

matrix_analysis <- matrix_full %>%
  dplyr::filter(
    !is.na(species),
    !is.na(tas_mean_c),
    !is.na(prec_mean_annual),
    !is.na(elevation_m),
    !is.na(NDVI)
  ) %>%
  dplyr::mutate(
    species = as.factor(species)
  )

print(summary(matrix_analysis[, env_vars]))
print(table(matrix_analysis$species))

# Interpretation:
# This cleaned matrix contains the occurrence records for both Vespa species
# with complete environmental information. The following scripts use these
# variables to test whether the invasive Vespa velutina and the native
# Vespa crabro differ in environmental niche conditions across Europe.