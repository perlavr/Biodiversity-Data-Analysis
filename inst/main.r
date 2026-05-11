################################################################################
# MAIN SCRIPT
#
# Objective:
# Run the full biodiversity data processing pipeline.
################################################################################

source("scr/01_download_and_clean_occ.r")
source("scr/02_climate.R")
source("scr/03_elevation.R")
source("scr/04_ecosystem.R")
source("scr/05_ndvi.R")
