#############################################
# ANALYSIS 3: RANDOM FOREST
#
# Objective:
# Identify the environmental variables that best
# discriminate Vespa velutina from Vespa crabro.
#############################################

# install.packages("randomForest")

library(dplyr)
library(randomForest)
library(ggplot2)

# =========================
# 1) PREPARE DATA
# =========================

rf_data <- matrix_analysis %>%
  dplyr::select(
    species,
    tas_mean_c,
    prec_mean_annual,
    elevation_m,
    NDVI
  ) %>%
  na.omit()

rf_data$species <- as.factor(rf_data$species)

print(table(rf_data$species))
print(dim(rf_data))

# =========================
# 2) FIT RANDOM FOREST
# =========================

set.seed(123)

rf_model <- randomForest(
  species ~ .,
  data = rf_data,
  ntree = 500,
  importance = TRUE
)

print(rf_model)

# =========================
# 3) VARIABLE IMPORTANCE
# =========================

importance_df <- data.frame(
  Variable = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, "MeanDecreaseGini"]
) %>%
  dplyr::mutate(
    Variable = dplyr::recode(
      Variable,
      tas_mean_c = "Temperature",
      prec_mean_annual = "Precipitation",
      elevation_m = "Elevation",
      NDVI = "NDVI"
    )
  ) %>%
  dplyr::arrange(desc(Importance))

print(importance_df)

# =========================
# 4) IMPORTANCE PLOT
# =========================

plot_rf <- ggplot(
  importance_df,
  aes(
    x = reorder(Variable, Importance),
    y = Importance
  )
) +
  geom_col(fill = "#2166AC", width = 0.7) +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Variables discriminating the two Vespa species",
    subtitle = "Higher values indicate stronger contribution to species classification",
    x = "",
    y = "Random Forest importance"
  )

print(plot_rf)

# =========================
# 5) MODEL PERFORMANCE
# =========================

conf_mat <- rf_model$confusion

print(conf_mat)

accuracy <- sum(diag(conf_mat)) /
  sum(conf_mat)

print(accuracy)

# =========================
# 6) EXPORT RESULTS
# =========================

dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

write.csv(
  importance_df,
  "data/processed/random_forest_importance.csv",
  row.names = FALSE
)

ggsave(
  filename = "inst/figures/random_forest_importance.png",
  plot = plot_rf,
  width = 8,
  height = 5,
  dpi = 300
)

# Interpretation:
#
# Temperature was the most important variable for distinguishing
# Vespa velutina from Vespa crabro, followed by precipitation,
# elevation and NDVI.
#
# This suggests that climatic conditions contribute more strongly
# to species differentiation than vegetation productivity alone.
#
# The Random Forest model achieved an overall classification
# accuracy of approximately 76%, indicating that environmental
# variables contain useful information for discriminating between
# the two species.
#
# Although the PCA and climatic niche analyses showed substantial
# overlap between the environmental niches of Vespa velutina and
# Vespa crabro, the Random Forest results suggest that subtle
# multivariate differences exist when several environmental
# variables are considered simultaneously.
#
# Overall, the two species occupy broadly similar environments
# across Europe, but temperature appears to be the strongest
# environmental factor contributing to their differentiation.