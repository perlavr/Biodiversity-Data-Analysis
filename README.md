# Biodiversity Data Analysis Project

## Objective
Compare environmental conditions associated with the invasive hornet
Vespa velutina and the native Vespa crabro across Europe.

## Data sources
- GBIF
- iNaturalist
- CHELSA climate data
- Elevation raster data
- WorldEcosystem raster
- MODIS NDVI (NASA AppEEARS)

## Pipeline
1. Download and clean occurrence records
2. Extract climate variables
3. Extract elevation
4. Extract ecosystem categories
5. Extract NDVI values

## Final dataset
data/processed/final_environmental_dataset_with_ndvi.csv