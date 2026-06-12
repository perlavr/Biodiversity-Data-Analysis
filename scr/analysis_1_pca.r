#############################################
# ANALYSIS 1: PCA OF ENVIRONMENTAL NICHE
#
# Objective:
# Use Principal Component Analysis to summarize
# the main environmental gradients occupied by
# Vespa velutina and Vespa crabro.
#
# Ecological purpose:
# PCA helps determine whether the invasive species
# Vespa velutina occupies a different environmental
# niche from the native Vespa crabro.
#############################################
install.packages("ggrepel")

library(dplyr)
library(ggplot2)
library(readr)
library(ggrepel)

# =========================
# 1) PREPARE PCA DATA
# =========================

# We keep the species identity, coordinates and selected environmental variables.
# The environmental variables were defined in step1_read_matrix.R as env_vars.
# Records with missing environmental values are removed because PCA cannot handle NA values.
pca_data <- matrix_analysis %>%
  dplyr::select(
    species,
    longitude,
    latitude,
    all_of(env_vars)
  ) %>%
  dplyr::filter(
    dplyr::if_all(
      all_of(env_vars),
      ~ !is.na(.)
    )
  )

# Only numerical environmental variables are used to calculate the PCA.
# Species and coordinates are kept separately for plotting and interpretation.
pca_env <- pca_data %>%
  dplyr::select(all_of(env_vars))

# =========================
# 2) RUN PCA
# =========================

# PCA is run on centered and scaled variables.
# Scaling is important because the variables have different units:
# temperature in °C, precipitation in mm, elevation in m, and NDVI without unit.
pca_res <- prcomp(
  pca_env,
  center = TRUE,
  scale. = TRUE
)

# Extract PCA scores for each occurrence record.
# These scores represent the position of each observation in the reduced
# environmental space defined by the principal components.
pca_scores <- as.data.frame(pca_res$x) %>%
  dplyr::bind_cols(
    pca_data %>%
      dplyr::select(species, longitude, latitude)
  )

# Calculate the percentage of variance explained by each PCA axis.
# These values are added to the axis labels of the PCA plot.
pca_var <- summary(pca_res)$importance[2, ] * 100

# Print PCA summaries to inspect the contribution of each environmental variable.
# The rotation table indicates which variables contribute most to each PCA axis.
print(summary(pca_res))
print(pca_res$rotation)

# =========================
# 3) PCA PLOT
# =========================

# Calculate the centroid of each species in PCA space.
# The centroid represents the average environmental position of each species.
species_centers <- pca_scores %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE),
    .groups = "drop"
  )

# The PCA plot shows whether the two Vespa species occupy similar or different
# environmental spaces.
# Points are occurrence records, ellipses summarize the main environmental
# space occupied by each species, and large black-outlined points are centroids.
plot_pca <- ggplot(
  pca_scores,
  aes(x = PC1, y = PC2, color = species, fill = species)
) +
  stat_ellipse(
    geom = "polygon",
    alpha = 0.16,
    color = NA
  ) +
  geom_point(
    alpha = 0.45,
    size = 1.7
  ) +
  geom_point(
    data = species_centers,
    aes(x = PC1, y = PC2, fill = species),
    shape = 21,
    size = 5,
    color = "black",
    stroke = 1
  ) +
  geom_text_repel(
  data = species_centers,
  aes(
    x = PC1,
    y = PC2,
    label = species
  ),
  color = "black",
  fontface = "bold",
  size = 5,
  show.legend = FALSE,
  box.padding = 1,
  point.padding = 1
) +
  scale_color_manual(values = species_cols) +
  scale_fill_manual(values = species_cols) +
  theme_vespa() +
  labs(
    title = "Environmental niche space of Vespa species",
    subtitle = "PCA based on temperature, precipitation, elevation and NDVI",
    x = paste0("PC1 (", round(pca_var[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_var[2], 1), "%)"),
    color = "Species",
    fill = "Species"
  )

print(plot_pca)

print(pca_res$rotation)


# =========================
# 4) EXPORT
# =========================

# Create output folders if they do not already exist.
dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Export the PCA figure for the final report and summary panel.
ggsave(
  filename = "inst/figures/final_pca_environmental_niche.png",
  plot = plot_pca,
  width = 8,
  height = 6,
  dpi = 300
)

# Export PCA scores so they can be reused in later scripts,
# for example for interactive plots.
readr::write_csv(
  pca_scores,
  "data/processed/pca_scores.csv"
)

# Interpretation:
# The PCA shows strong overlap between the environmental niches of
# Vespa velutina and Vespa crabro. Most occurrence points are concentrated
# close to the center of the PCA space, around PC1 = 0 and PC2 = 0,
# indicating that many records occur under relatively similar environmental
# conditions.
#
# Vespa crabro is slightly shifted upward along PC2 relative to
# Vespa velutina. The PCA loadings indicate that PC2 is primarily associated
# with NDVI, suggesting that Vespa crabro tends to occur in areas with
# slightly higher vegetation productivity.
#
# However, the large overlap between the species ellipses indicates that
# environmental differences remain relatively weak at the European scale.
# Overall, both species occupy broadly similar environmental conditions,
# with only a modest differentiation along the vegetation gradient.
