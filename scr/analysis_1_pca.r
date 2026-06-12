#############################################
# ANALYSIS 1: PCA OF ENVIRONMENTAL NICHE
#
# Objective:
# Use Principal Component Analysis to summarize
# the main environmental gradients occupied by
# Vespa velutina and Vespa crabro.
#############################################

library(dplyr)
library(ggplot2)
library(readr)

# =========================
# 1) PREPARE PCA DATA
# =========================

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

pca_env <- pca_data %>%
  dplyr::select(all_of(env_vars))

# =========================
# 2) RUN PCA
# =========================

pca_res <- prcomp(
  pca_env,
  center = TRUE,
  scale. = TRUE
)

pca_scores <- as.data.frame(pca_res$x) %>%
  dplyr::bind_cols(
    pca_data %>%
      dplyr::select(species, longitude, latitude)
  )

pca_var <- summary(pca_res)$importance[2, ] * 100

print(summary(pca_res))
print(pca_res$rotation)

# =========================
# 3) PCA PLOT
# =========================

species_centers <- pca_scores %>%
  dplyr::group_by(species) %>%
  dplyr::summarise(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE),
    .groups = "drop"
  )

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

# =========================
# 4) EXPORT
# =========================

dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

ggsave(
  filename = "inst/figures/final_pca_environmental_niche.png",
  plot = plot_pca,
  width = 8,
  height = 6,
  dpi = 300
)

readr::write_csv(
  pca_scores,
  "data/processed/pca_scores.csv"
)

# Interpretation:
# The PCA summarizes the main environmental gradients occupied by both species.
# If the two species form separated clouds or have weakly overlapping ellipses,
# this suggests that Vespa velutina and Vespa crabro occupy different
# environmental conditions. If the ellipses largely overlap, their realized
# environmental niches are more similar across the sampled European records.