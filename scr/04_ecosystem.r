#############################################
# SCRIPT 04: ADD ECOSYSTEM DATA
#
# Objective:
# Add ecosystem categories to Vespa occurrence records
# using the WorldEcosystem raster and its metadata table.
#
# Ecological rationale:
# Ecosystem and land-cover categories describe habitat structure.
# They can influence nesting opportunities, prey availability,
# vegetation cover, and landscape suitability for Vespa species.
#############################################

# =========================
# 1) PACKAGES
# =========================

library(dplyr)
library(readr)
library(raster)
library(sf)
library(rnaturalearth)
library(ggplot2)

sf_use_s2(FALSE)

# =========================
# 2) LOAD OCCURRENCE DATA
# =========================

# Load the dataset enriched with climate and elevation variables.
species_env_df <- readr::read_csv(
  "data/processed/occurrences_with_climate_elevation.csv",
  show_col_types = FALSE
)
# =========================
# 3) LOAD ECOSYSTEM RASTER
# =========================

# The WorldEcosystem raster contains categorical ecosystem classes.
# Each raster cell stores a numeric code that must be linked to metadata.
ecosystem_raster <- raster::raster(
  "data/WorldEcosystem.tif"
)

# Print raster information to check extent, resolution, and coordinate system.
print(ecosystem_raster)

# =========================
# 4) DEFINE EUROPE STUDY AREA
# =========================

# Define the same European bounding box used in previous scripts.
# This keeps all environmental extractions spatially consistent.
europe_bbox <- sf::st_bbox(
  c(xmin = -10, xmax = 30, ymin = 35, ymax = 70),
  crs = sf::st_crs(4326)
)

# Convert bounding box into an sf polygon.
europe_poly <- sf::st_sf(
  id = 1,
  geometry = sf::st_as_sfc(europe_bbox)
)

# Convert sf object to Spatial object because raster functions
# operate with Spatial classes.
europe_sp <- as(europe_poly, "Spatial")

# =========================
# 5) CROP ECOSYSTEM RASTER TO EUROPE
# =========================

# crop() reduces the raster to the rectangular extent of Europe.
ecosystem_europe <- raster::crop(
  ecosystem_raster,
  raster::extent(europe_sp)
)

# mask() keeps only pixels inside the European study polygon.
ecosystem_europe <- raster::mask(
  ecosystem_europe,
  europe_sp
)

# =========================
# 6) CONVERT OCCURRENCES TO SPATIAL POINTS
# =========================

# Convert occurrence coordinates to SpatialPoints.
# The coordinate reference system is WGS84 longitude/latitude.
spatial_points <- sp::SpatialPoints(
  coords = species_env_df[, c("longitude", "latitude")],
  proj4string = sp::CRS("+proj=longlat +datum=WGS84")
)


# =========================
# 7) EXTRACT ECOSYSTEM VALUES
# =========================

# Extract the ecosystem raster code at each occurrence location.
eco_values <- raster::extract(
  ecosystem_europe,
  spatial_points
)

# Add ecosystem numeric codes to the occurrence dataset.
species_env_eco_df <- species_env_df %>%
  dplyr::mutate(
    ecosystem_code = as.integer(eco_values)
  )

# =========================
# 8) LOAD ECOSYSTEM METADATA
# =========================

# The metadata table links ecosystem numeric codes to descriptive classes.
metadata_eco <- readr::read_tsv(
  "data/WorldEcosystem.metadata.tsv",
  show_col_types = FALSE
)

# Inspect metadata structure to verify the column names.
print(names(metadata_eco))
print(head(metadata_eco))

# =========================
# 9) JOIN ECOSYSTEM METADATA
# =========================

# Join descriptive ecosystem information to occurrence records.
# ecosystem_code is the extracted raster value.
# Value is the corresponding code in the metadata table.
species_env_eco_df <- species_env_eco_df %>%
  dplyr::left_join(
    metadata_eco,
    by = c("ecosystem_code" = "Value")
  )

# =========================
# 10) VALIDATION CHECKS
# =========================

# Check species representation after ecosystem extraction.
print(table(species_env_eco_df$species))

# Check extracted ecosystem codes, including missing values.
print(table(species_env_eco_df$ecosystem_code, useNA = "ifany"))

# If the metadata contains a climate-region variable,
# check its distribution as an additional validation step.
if ("Climate_Re" %in% names(species_env_eco_df)) {
  print(table(species_env_eco_df$Climate_Re, useNA = "ifany"))
}

# =========================
# 11) VALIDATION PLOT
# =========================

# Create a validation plot if the Climate_Re metadata column exists.
# This plot shows how both species are distributed across ecosystem
# climate categories.
if ("Climate_Re" %in% names(species_env_eco_df)) {
  
  plot_ecosystem <- ggplot(
    species_env_eco_df,
    aes(x = Climate_Re, fill = species)
  ) +
    geom_bar(position = "dodge") +
    theme_classic() +
    labs(
      title = "Vespa occurrences by ecosystem climate category",
      subtitle = "WorldEcosystem raster categories",
      x = "Climate category",
      y = "Number of occurrences",
      fill = "Species"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(plot_ecosystem)
}

# Ecological interpretation:
#
# Most occurrence records for both Vespa species are associated
# with "cold temperature moist" ecosystem categories.
# This likely reflects the dominance of temperate and humid
# environments across western and central Europe, where most
# occurrence records were collected.
#
# A smaller number of occurrences are associated with
# "warm temperature moist" ecosystems, suggesting that both species
# can also occur in warmer southern European regions.
#
# Vespa velutina appears particularly common in humid temperate regions,
# which is consistent with its current invasion distribution in Europe.

# =========================
# 12) EXPORT
# =========================

# Export the occurrence dataset enriched with ecosystem variables.
readr::write_csv(
  species_env_eco_df,
  "data/processed/occurrences_with_climate_elevation_ecosystem.csv"
)

# Export ecosystem validation figure only if it was created.
if (exists("plot_ecosystem")) {
  ggsave(
    filename = "inst/figures/ecosystem_categories.png",
    plot = plot_ecosystem,
    width = 9,
    height = 5,
    dpi = 300
  )
}
