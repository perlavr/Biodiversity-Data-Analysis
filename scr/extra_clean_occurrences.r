#############################################
# SCRIPT 02: CLEAN GBIF OCCURRENCE DATA
#
# Objective:
# Import offline GBIF datasets for Vespa velutina and Vespa crabro,
# clean occurrence records, and create a harmonised dataset for Europe.
#############################################

library(dplyr)
library(readr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::distinct)

# =========================
# 1) PARAMETERS
# =========================

date_start <- as.Date("2020-01-01")
date_end   <- as.Date("2026-04-22")

# Europe bounding box
xmin <- -10
xmax <- 30
ymin <- 35
ymax <- 70

# =========================
# 2) IMPORT RAW GBIF FILES
# =========================

# GBIF downloads are often tab-separated, even when named .csv.
crabro_raw <- read_delim(
  "data/crabro.csv",
  delim = "\t",
  col_types = cols(.default = col_character())
)

velutina_raw <- read_delim(
  "data/velutina.csv",
  delim = "\t",
  col_types = cols(.default = col_character())
)

# =========================
# 3) STANDARDISE DATA
# =========================

crabro <- crabro_raw %>%
  mutate(target_species = "Vespa crabro")

velutina <- velutina_raw %>%
  mutate(target_species = "Vespa velutina")

gbif_raw <- bind_rows(crabro, velutina)

# =========================
# 4) CLEAN OCCURRENCE DATA
# =========================

gbif_clean <- gbif_raw %>%
  mutate(
    longitude = as.numeric(decimalLongitude),
    latitude  = as.numeric(decimalLatitude),
    date_obs  = as.Date(sub("/.*", "", eventDate)),
    year_obs  = as.numeric(format(date_obs, "%Y")),
    uncertainty = as.numeric(coordinateUncertaintyInMeters),
    source    = "GBIF"
  ) %>%
  
  filter(
    !is.na(longitude),
    !is.na(latitude),
    !is.na(date_obs),
    
    longitude >= -180, longitude <= 180,
    latitude >= -90, latitude <= 90
  ) %>%
  
  filter(
    longitude >= xmin,
    longitude <= xmax,
    latitude >= ymin,
    latitude <= ymax
  ) %>%
  
  filter(
    date_obs >= date_start,
    date_obs <= date_end
  ) %>%
  
  filter(
    occurrenceStatus == "PRESENT" | is.na(occurrenceStatus)
  ) %>%
  
  filter(
    is.na(uncertainty) | uncertainty <= 10000
  ) %>%
  
  select(
    species = target_species,
    scientificName,
    countryCode,
    longitude,
    latitude,
    date_obs,
    year_obs,
    basisOfRecord,
    uncertainty,
    source
  ) %>%
  distinct(species, longitude, latitude, date_obs, .keep_all = TRUE)

# =========================
# 5) VALIDATION CHECKS
# =========================

print(nrow(gbif_clean))
print(table(gbif_clean$species))
print(summary(gbif_clean$date_obs))
print(table(gbif_clean$countryCode, useNA = "ifany"))

# =========================
# 6) VALIDATION MAP
# =========================

world <- ne_countries(scale = "medium", returnclass = "sf")

# Add a filtered dataset

gbif_recent <- gbif_clean %>%
  dplyr::filter(year_obs %in% c(2025, 2026))

plot_occurrences <- ggplot(data = world) +
  geom_sf(fill = "grey95", color = "grey40") +
  
  geom_point(
    data = gbif_recent,   # ← ici
    aes(x = longitude, y = latitude, color = species),
    size = 0.8,
    alpha = 0.6
  ) +
  
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  
  facet_wrap(~ species) +
  
  theme_classic() +
  
  labs(
    title = "GBIF occurrences (2025–2026)",
    subtitle = "Vespa velutina vs Vespa crabro",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )

print(plot_occurrences)

# =========================
# 7) EXPORT CLEAN DATA
# =========================

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)

write_csv(
  gbif_clean,
  "data/processed/clean_occurrences_gbif.csv"
)

ggsave(
  filename = "inst/figures/extra_map_occurrences.png",
  plot = plot_occurrences,
  width = 10,
  height = 6,
  dpi = 300
)
