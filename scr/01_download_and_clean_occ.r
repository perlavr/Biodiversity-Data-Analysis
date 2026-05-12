#############################################
# SCRIPT 01: DOWNLOAD AND CLEAN OCCURRENCE DATA
#
# Objective
# Build a clean multi-species occurrence dataset
# across Europe using GBIF and iNaturalist.
#
# Ecological question:
# Does the invasive hornet Vespa velutina occupy
# different environmental conditions compared to
# the native Vespa crabro across Europe?
#
# This dataset will later be enriched with climate,
# elevation, ecosystem, and satellite-derived vegetation variables.
#
# Species:
# - Vespa velutina
# - Vespa crabro
#############################################

# =========================
# 1) PACKAGES
# =========================

library(rgbif)
library(rnaturalearth)
library(ggplot2)
library(rinat)
library(dplyr)
library(sf)
library(conflicted)
library(readr)
library(tidyr)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::bind_rows)

sf_use_s2(FALSE)

# =========================
# 2) USER PARAMETERS
# =========================

# Comparison between:
# - an invasive hornet species (Vespa velutina)
# - a native European hornet species (Vespa crabro)
#
# Both species are well documented in citizen
# science databases and are ecologically relevant
# for invasion ecology analyses.
species_list <- c(
  "Vespa velutina",
  "Vespa crabro"
)

# Maximum number of records downloaded per species and per database.
nblimit <- 5000

# Only recent observations are retained to better match
# contemporary environmental conditions and understand the
# recent invasion.
date_start <- as.Date("2020-01-01")
date_end   <- as.Date("2026-04-22")

# European study area
xmin <- -10
xmax <- 30
ymin <- 35
ymax <- 70

# =========================
# 3) EUROPE FILTER FUNCTIONS
# =========================

# These functions restrict occurrence records to the
# European study area.
filter_europe_gbif <- function(df) {
  df %>%
    dplyr::filter(
      .data$decimalLongitude > xmin,
      .data$decimalLongitude < xmax,
      .data$decimalLatitude > ymin,
      .data$decimalLatitude < ymax
    )
}

filter_europe_inat <- function(df) {
  df %>%
    dplyr::filter(
      .data$longitude > xmin,
      .data$longitude < xmax,
      .data$latitude > ymin,
      .data$latitude < ymax
    )
}

# =========================
# 4) DOWNLOAD GBIF DATA
# =========================

gbif_list <- list()

for (sp in species_list) {
  # Download occurrence records with geographic coordinates from GBIF.
  gbif_raw <- occ_data(
    scientificName = sp,
    hasCoordinate = TRUE,
    limit = nblimit
  )
  gbif_occ <- gbif_raw$data
  # Remove records without coordinates and retain only European observations.
  gbif_occ <- gbif_occ %>%
    dplyr::filter(
      !is.na(decimalLongitude),
      !is.na(decimalLatitude)
    ) %>%
    filter_europe_gbif()
  gbif_list[[sp]] <- data.frame(
    species      = sp,
    longitude    = gbif_occ$decimalLongitude,
    latitude     = gbif_occ$decimalLatitude,
    date_obs     = as.Date(gbif_occ$eventDate),
    countryCode  = gbif_occ$countryCode,
    location     = NA_character_,
    source       = "gbif"
  )
}

# Cleaning and harmonisation of GBIF records.
# The objective is to obtain a consistent dataset structure
# before merging with iNaturalist observations.
gbif_final <- dplyr::bind_rows(gbif_list)
print(gbif_final)

# =========================
# 5) CLEAN GBIF DATA
# =========================

data_gbif <- gbif_final %>%
  
  dplyr::filter(
    !is.na(longitude),
    !is.na(latitude),
    !is.na(date_obs)
  ) %>%
  
  dplyr::filter(
    date_obs >= date_start,
    date_obs <= date_end
  ) %>%
  
  dplyr::mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude),
    source    = "gbif"
  ) %>%
  
  # Remove duplicated occurrence records.
  # Duplicates are defined as identical species observations
  # occurring at the same coordinates and date.
  dplyr::distinct(
    species,
    longitude,
    latitude,
    date_obs,
    .keep_all = TRUE
  ) %>%
  
  dplyr::select(
    species,
    longitude,
    latitude,
    date_obs,
    countryCode,
    location,
    source
  )

print(data_gbif)

# =========================
# 6) DOWNLOAD INATURALIST DATA
# =========================

inat_list <- list()

for (sp in species_list) {
  
  inat_raw <- tryCatch(
    get_inat_obs(
      taxon_name = sp,
      maxresults = nblimit
    ),
    error = function(e) NULL
  )
  
  if (is.null(inat_raw) || nrow(inat_raw) == 0) {
    warning(paste("No iNaturalist data for", sp))
    next
  }
  
  inat_raw <- inat_raw %>%
    dplyr::filter(
      !is.na(longitude),
      !is.na(latitude)
    ) %>%
    filter_europe_inat()
  
  if (nrow(inat_raw) == 0) {
    warning(paste("No European records for", sp))
    next
  }
  
  inat_list[[sp]] <- data.frame(
    species      = sp,
    longitude    = inat_raw$longitude,
    latitude     = inat_raw$latitude,
    date_obs     = as.Date(inat_raw$observed_on),
    countryCode  = NA_character_,
    location     = inat_raw$place_guess,
    source       = "inat"
  )
}

# IMPORTANT: create inat_final after the loop
if (length(inat_list) == 0) {
  inat_final <- data.frame(
    species = character(),
    longitude = numeric(),
    latitude = numeric(),
    date_obs = as.Date(character()),
    countryCode = character(),
    location = character(),
    source = character()
  )
} else {
  inat_final <- dplyr::bind_rows(inat_list)
}

# Now inat_final exists
print(inat_final)

# =========================
# 7) CLEAN INATURALIST DATA
# =========================

if (nrow(inat_final) == 0) {
  
  warning("No iNaturalist data retained")
  
  data_inat <- data.frame(
  species = character(),
  longitude = numeric(),
  latitude = numeric(),
  date_obs = as.Date(character()),
  countryCode = character(),
  location = character(),
  source = character()
  ) 
  
} else {
  
  data_inat <- inat_final %>%
    
    dplyr::filter(
      !is.na(longitude),
      !is.na(latitude),
      !is.na(date_obs)
    ) %>%
    
    # Retain only observations within the selected temporal window.
    dplyr::filter(
      date_obs >= date_start,
      date_obs <= date_end
    ) %>%
    
    dplyr::mutate(
      longitude = as.numeric(longitude),
      latitude  = as.numeric(latitude),
      source    = "inat"
    ) %>%
    
    dplyr::distinct(
      species,
      longitude,
      latitude,
      date_obs,
      .keep_all = TRUE
    ) %>%
    
    dplyr::select(
      species,
      longitude,
      latitude,
      date_obs,
      countryCode,
      location,
      source
    )
}

print(data_inat)

# =========================
# 8) MERGE GBIF + INAT
# =========================

# Merge GBIF and iNaturalist datasets into a single table.
matrix_full <- dplyr::bind_rows(
  data_gbif,
  data_inat
)

print(names(matrix_full))

# =========================
# 9) FINAL EUROPE FILTER
# =========================

# Convert the occurrence dataset into a spatial object
# to perform spatial filtering operations.
matrix_full_sf <- st_as_sf(
  matrix_full,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

europe_bbox <- st_bbox(c(
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax
), crs = st_crs(4326))

europe_poly <- st_as_sfc(europe_bbox)

inside <- st_within(
  matrix_full_sf,
  europe_poly,
  sparse = FALSE
)[,1]

# Final cleaning step:
# - retain only occurrences located within the European study area
# - simplify coordinates for visualisation
# - remove duplicated records
cur_data <- matrix_full[inside, ]

cur_data <- cur_data %>%
  dplyr::mutate(
    longitude = round(.data$longitude, 2),
    latitude  = round(.data$latitude, 2)
  ) %>%
  dplyr::distinct(
    .data$species,
    .data$longitude,
    .data$latitude,
    .data$date_obs,
    .keep_all = TRUE
  )

print(matrix_full)
print(cur_data)

# =========================
# 10) VALIDATION CHECKS
# =========================

print(nrow(cur_data))

print(table(cur_data$species))

print(table(cur_data$source))

print(summary(cur_data$date_obs))

# =========================
# 11) VALIDATION MAP
# =========================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

europe_crop <- st_crop(
  world,
  europe_bbox
)

plot_occurrences <- ggplot(data = europe_crop) +
  
  geom_sf(
    fill = "grey95",
    color = "black"
  ) +
  
  geom_point(
    data = cur_data,
    aes(
      x = longitude,
      y = latitude,
      fill = source
    ),
    shape = 21,
    size = 1,
    color = "white",
    alpha = 0.7
  ) +
  
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax)
  ) +
  
  facet_wrap(~ species) +
  
  scale_fill_manual(values = c(
    "gbif" = "darkgreen",
    "inat" = "red"
  )) +
  
  theme_classic() +
  
  labs(
    title = "European occurrence dataset",
    subtitle = "GBIF and iNaturalist records",
    x = "Longitude",
    y = "Latitude",
    fill = "Source"
  )

print(plot_occurrences)

# =========================
# 12) EXPORT CLEAN DATASET
# =========================

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "inst/figures",
  showWarnings = FALSE,
  recursive = TRUE
)

readr::write_csv(
  cur_data,
  "data/processed/clean_occurrences.csv"
)

ggsave(
  filename = "inst/figures/map_occurrences.png",
  plot = plot_occurrences,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 13) TEMPORAL DISTRIBUTION OF OCCURRENCES
# =========================

cur_data %>%
  dplyr::mutate(year = as.numeric(format(date_obs, "%Y"))) %>%
  dplyr::count(year, species) %>%
  tidyr::pivot_wider(
    names_from = species,
    values_from = n,
    values_fill = 0
  )

# =========================
# 14) VISUALISATION OF RECENT OCCURRENCES (2025)
# =========================

plot_2025 <- cur_data %>%
  
  dplyr::mutate(
    year = as.numeric(format(date_obs, "%Y"))
  ) %>%
  
  dplyr::filter(year == 2025)

# Simplify coordinates to reduce point overlap in the validation map.
plot_2025 <- plot_2025 %>%
  
  # Coordinates are rounded to two decimals to reduce repeated records
  # from the same local area. This corresponds to approximately 1 km
  # and is used as a spatial deduplication step.
  dplyr::mutate(
    longitude = round(longitude, 2),
    latitude  = round(latitude, 2)
  ) %>%
  
  dplyr::distinct(
    species,
    longitude,
    latitude,
    .keep_all = TRUE
  )

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

plot_2025_map <- ggplot(data = world) +
  
  geom_sf(
    fill = "grey95",
    color = "grey40"
  ) +
  
  geom_point(
    data = plot_2025,
    aes(
      x = longitude,
      y = latitude,
      fill = species
    ),
    shape = 21,
    color = "white",
    size = 1.5,
    alpha = 0.7
  ) +
  
  coord_sf(
    xlim = c(-10, 30),
    ylim = c(35, 70)
  ) +
  
  scale_fill_manual(values = c(
    "Vespa velutina" = "darkgreen",
    "Vespa crabro" = "red"
  )) +
  
  theme_classic() +
  
  labs(
    title = "Vespa occurrences in Europe (2025)",
    subtitle = "Filtered GBIF + iNaturalist records",
    x = "Longitude",
    y = "Latitude",
    fill = "Species"
  )

print(plot_2025_map)

ggsave(
  filename = "inst/figures/map_occurrences_2025.png",
  plot = plot_2025_map,
  width = 10,
  height = 6,
  dpi = 300
)
