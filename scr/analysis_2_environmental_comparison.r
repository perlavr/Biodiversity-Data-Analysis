#############################################
# ANALYSIS 2: ENVIRONMENTAL COMPARISON
#
# Objective:
# Compare key environmental variables between
# Vespa velutina and Vespa crabro.
#############################################

library(dplyr)
library(ggplot2)

# =========================
# 1) TEMPERATURE
# =========================

plot_temp_box <- ggplot(
  matrix_analysis,
  aes(
    x = species,
    y = tas_mean_c,
    fill = species
  )
) +
  geom_boxplot(alpha = 0.7) +
  theme_classic() +
  labs(
    title = "Temperature comparison",
    x = "Species",
    y = "Mean annual temperature (°C)"
  )

print(plot_temp_box)

# =========================
# 2) PRECIPITATION
# =========================

plot_prec_box <- ggplot(
  matrix_analysis,
  aes(
    x = species,
    y = prec_mean_annual,
    fill = species
  )
) +
  geom_boxplot(alpha = 0.7) +
  theme_classic() +
  labs(
    title = "Precipitation comparison",
    x = "Species",
    y = "Annual precipitation"
  )

print(plot_prec_box)

# =========================
# 3) ELEVATION
# =========================

plot_elev_box <- ggplot(
  matrix_analysis,
  aes(
    x = species,
    y = elevation_m,
    fill = species
  )
) +
  geom_boxplot(alpha = 0.7) +
  theme_classic() +
  labs(
    title = "Elevation comparison",
    x = "Species",
    y = "Elevation (m)"
  )

print(plot_elev_box)

# =========================
# 4) NDVI
# =========================

plot_ndvi_box <- ggplot(
  matrix_analysis,
  aes(
    x = species,
    y = NDVI,
    fill = species
  )
) +
  geom_boxplot(alpha = 0.7) +
  theme_classic() +
  labs(
    title = "Vegetation productivity comparison",
    x = "Species",
    y = "NDVI"
  )

print(plot_ndvi_box)

# =========================
# 5) CORRELATION HEATMAP
# =========================

corr_data <- matrix_analysis %>%
  dplyr::select(
    tas_mean_c,
    prec_mean_annual,
    elevation_m,
    NDVI
  )

corr_matrix <- cor(
  corr_data,
  use = "complete.obs"
)

corr_df <- as.data.frame(as.table(corr_matrix))

plot_corr <- ggplot(
  corr_df,
  aes(
    Var1,
    Var2,
    fill = Freq
  )
) +
  geom_tile() +
  geom_text(
    aes(label = round(Freq, 2)),
    color = "white"
  ) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  theme_classic() +
  labs(
    title = "Correlation matrix of environmental variables",
    x = "",
    y = "",
    fill = "r"
  )

print(plot_corr)

# =========================
# 6) SIMPLE STATISTICAL TESTS
# =========================

temp_test <- t.test(
  tas_mean_c ~ species,
  data = matrix_analysis
)

prec_test <- t.test(
  prec_mean_annual ~ species,
  data = matrix_analysis
)

elev_test <- t.test(
  elevation_m ~ species,
  data = matrix_analysis
)

ndvi_test <- t.test(
  NDVI ~ species,
  data = matrix_analysis
)

print(temp_test)
print(prec_test)
print(elev_test)
print(ndvi_test)

# =========================
# 7) EXPORT
# =========================

ggsave(
  "inst/figures/boxplot_temperature.png",
  plot_temp_box,
  width = 6,
  height = 5,
  dpi = 300
)

ggsave(
  "inst/figures/boxplot_precipitation.png",
  plot_prec_box,
  width = 6,
  height = 5,
  dpi = 300
)

ggsave(
  "inst/figures/boxplot_elevation.png",
  plot_elev_box,
  width = 6,
  height = 5,
  dpi = 300
)

ggsave(
  "inst/figures/boxplot_ndvi.png",
  plot_ndvi_box,
  width = 6,
  height = 5,
  dpi = 300
)

ggsave(
  "inst/figures/correlation_heatmap.png",
  plot_corr,
  width = 7,
  height = 6,
  dpi = 300
)

# Interpretation:
# These comparisons allow identification of environmental variables
# that differ significantly between the invasive and native hornet.
# Significant differences suggest environmental niche divergence.

plot_climate_space <- ggplot(
  matrix_analysis,
  aes(
    x = tas_mean_c,
    y = prec_mean_annual,
    color = species,
    fill = species
  )
) +
  geom_point(alpha = 0.35, size = 1.5) +
  stat_ellipse(
    geom = "polygon",
    alpha = 0.15,
    color = NA
  ) +
  theme_classic() +
  scale_color_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  scale_fill_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  labs(
    title = "Climatic niche of Vespa species",
    subtitle = "Temperature and precipitation conditions at occurrence points",
    x = "Mean annual temperature (°C)",
    y = "Annual precipitation",
    color = "Species",
    fill = "Species"
  )

print(plot_climate_space)

ggsave(
  "inst/figures/climatic_niche_temperature_precipitation.png",
  plot_climate_space,
  width = 8,
  height = 6,
  dpi = 300
)

plot_env_violin <- matrix_analysis %>%
  dplyr::select(species, tas_mean_c, prec_mean_annual, elevation_m, NDVI) %>%
  tidyr::pivot_longer(
    cols = c(tas_mean_c, prec_mean_annual, elevation_m, NDVI),
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    variable = dplyr::recode(
      variable,
      tas_mean_c = "Temperature",
      prec_mean_annual = "Precipitation",
      elevation_m = "Elevation",
      NDVI = "NDVI"
    )
  ) %>%
  ggplot(aes(x = species, y = value, fill = species)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.9) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_classic() +
  scale_fill_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  labs(
    title = "Environmental conditions occupied by each species",
    subtitle = "Violin plots show the full distribution of each environmental variable",
    x = "",
    y = "Environmental value",
    fill = "Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(plot_env_violin)

ggsave(
  "inst/figures/environmental_violin_comparison.png",
  plot_env_violin,
  width = 10,
  height = 7,
  dpi = 300
)
