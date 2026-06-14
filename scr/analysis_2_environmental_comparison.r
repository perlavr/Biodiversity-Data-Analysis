#############################################
# ANALYSIS 2: ENVIRONMENTAL COMPARISON
#
# Objective:
# Compare the main environmental variables between
# the invasive Vespa velutina and the native Vespa crabro.
#
# Ecological purpose:
# This analysis tests whether the two species differ in
# temperature, precipitation, elevation and vegetation productivity.
#############################################

library(dplyr)
library(ggplot2)
library(tidyr)

# =========================
# 1) PREPARE DATA FOR COMPARISON
# =========================

# Convert the environmental variables to a long format.
# This makes it possible to compare several variables in a single figure.
env_long <- matrix_analysis %>%
  dplyr::select(
    species,
    tas_mean_c,
    prec_mean_annual,
    elevation_m,
    NDVI
  ) %>%
  tidyr::pivot_longer(
    cols = c(tas_mean_c, prec_mean_annual, elevation_m, NDVI),
    names_to = "variable",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    variable = dplyr::recode(
      variable,
      tas_mean_c = "Temperature (°C)",
      prec_mean_annual = "Precipitation",
      elevation_m = "Elevation (m)",
      NDVI = "NDVI"
    )
  )

# =========================
# 2) VIOLIN PLOTS
# =========================

# Violin plots show the full distribution of each environmental variable.
# Boxplots are added inside the violins to show medians and interquartile ranges.
# This figure helps identify which environmental variables differ most between species.
plot_env_violin <- ggplot(
  env_long,
  aes(
    x = species,
    y = value,
    fill = species
  )
) +
  geom_violin(
    alpha = 0.55,
    trim = TRUE,
    color = NA
  ) +
  geom_boxplot(
    width = 0.16,
    outlier.size = 0.4,
    alpha = 0.9,
    color = "grey20"
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  scale_fill_manual(values = species_cols) +
  theme_vespa() +
  labs(
    title = "Environmental conditions occupied by each Vespa species",
    subtitle = "Distributions of temperature, precipitation, elevation and NDVI",
    x = "",
    y = "Environmental value",
    fill = "Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "none"
  )

print(plot_env_violin)

# =========================
# 3) CLIMATIC NICHE SPACE
# =========================

# This scatter plot compares the two main climate variables:
# mean annual temperature and annual precipitation.
# Ellipses summarize the climatic space occupied by each species.
plot_climate_space <- ggplot(
  matrix_analysis,
  aes(
    x = tas_mean_c,
    y = prec_mean_annual,
    color = species,
    fill = species
  )
) +
  stat_ellipse(
    geom = "polygon",
    alpha = 0.15,
    color = NA
  ) +
  geom_point(
    alpha = 0.38,
    size = 1.5
  ) +
  scale_color_manual(values = species_cols) +
  scale_fill_manual(values = species_cols) +
  theme_vespa() +
  labs(
    title = "Climatic niche space of Vespa species",
    subtitle = "Temperature and precipitation conditions at occurrence points",
    x = "Mean annual temperature (°C)",
    y = "Annual precipitation",
    color = "Species",
    fill = "Species"
  )

print(plot_climate_space)

# =========================
# 4) CORRELATION HEATMAP
# =========================

# The correlation matrix checks whether environmental variables are related.
# This is useful because strongly correlated variables may describe similar
# environmental gradients.
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

corr_df <- as.data.frame(as.table(corr_matrix)) %>%
  dplyr::mutate(
    Var1 = dplyr::recode(
      Var1,
      tas_mean_c = "Temperature",
      prec_mean_annual = "Precipitation",
      elevation_m = "Elevation",
      NDVI = "NDVI"
    ),
    Var2 = dplyr::recode(
      Var2,
      tas_mean_c = "Temperature",
      prec_mean_annual = "Precipitation",
      elevation_m = "Elevation",
      NDVI = "NDVI"
    )
  )

plot_corr <- ggplot(
  corr_df,
  aes(
    x = Var1,
    y = Var2,
    fill = Freq
  )
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = round(Freq, 2)),
    color = "black",
    size = 4
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_vespa() +
  labs(
    title = "Correlation between environmental variables",
    subtitle = "Pearson correlation coefficients",
    x = "",
    y = "",
    fill = "r"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "right"
  )

print(plot_corr)

# =========================
# 5) SIMPLE STATISTICAL TESTS
# =========================

# Welch t-tests compare the mean environmental values between the two species.
# These tests are exploratory and are used to support the visual comparisons.
temp_test <- t.test(tas_mean_c ~ species, data = matrix_analysis)
prec_test <- t.test(prec_mean_annual ~ species, data = matrix_analysis)
elev_test <- t.test(elevation_m ~ species, data = matrix_analysis)
ndvi_test <- t.test(NDVI ~ species, data = matrix_analysis)

print(temp_test)
print(prec_test)
print(elev_test)
print(ndvi_test)

# Store p-values in a small results table.
test_results <- data.frame(
  variable = c("Temperature", "Precipitation", "Elevation", "NDVI"),
  p_value = c(
    temp_test$p.value,
    prec_test$p.value,
    elev_test$p.value,
    ndvi_test$p.value
  )
)

print(test_results)

# =========================
# 6) EXPORT
# =========================

dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

ggsave(
  "inst/figures/environmental_violin_comparison.png",
  plot_env_violin,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "inst/figures/climatic_niche_temperature_precipitation.png",
  plot_climate_space,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "inst/figures/correlation_heatmap.png",
  plot_corr,
  width = 7,
  height = 6,
  dpi = 300
)

write.csv(
  test_results,
  "data/processed/environmental_t_tests.csv",
  row.names = FALSE
)

# Interpretation:
# The violin plots show a strong overlap between the environmental conditions
# occupied by Vespa velutina and Vespa crabro. For temperature, precipitation,
# elevation and NDVI, the median values and interquartile ranges are broadly
# similar between species.
#
# Vespa velutina generally shows a slightly wider distribution around the
# central part of the violin plots, indicating that many records occur under
# similar environmental conditions. However, this pattern does not correspond
# to a clear shift in the environmental values occupied by the species.
#
# Overall, the distributions suggest that both species occupy largely similar
# environmental conditions across Europe, with only limited evidence of strong
# environmental niche differentiation based on the variables considered here.
#
# In the correlation heatmap, the strongest relationship is observed between
# elevation and precipitation (r ≈ 0.28), indicating a weak positive association.
# Higher elevation sites tend to receive slightly more precipitation, which is
# consistent with orographic effects commonly observed in European mountain regions.
#
# Overall, the low correlation values suggest that each environmental variable
# contributes different ecological information and can therefore be retained
# for subsequent analyses.
#
# The climatic niche plot shows a strong overlap between Vespa velutina and
# Vespa crabro in the temperature–precipitation space.
#
# The occurrence records of both species occupy very similar combinations of
# mean annual temperature and precipitation, and the confidence ellipses largely
# overlap.
#
# This suggests that the invasive Vespa velutina currently occurs under climatic
# conditions that are broadly similar to those occupied by the native Vespa crabro
# across Europe.
#
# Therefore, temperature and precipitation alone do not appear to strongly
# differentiate the environmental niches of the two species in the present dataset.