#############################################
# ANALYSIS 3: RANDOM FOREST
#
# Objective:
# Identify the environmental variables that best
# discriminate Vespa velutina from Vespa crabro.
#############################################

install.packages("randomForest")

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

importance_df <- importance_df %>%
  arrange(desc(Importance))

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
# Variables with higher Mean Decrease Gini values contribute more
# strongly to species discrimination.
#
# If temperature and precipitation dominate the ranking,
# climatic conditions may be the main drivers of niche differences.
#
# If NDVI or elevation are highly ranked, habitat structure
# and topography may play an important role in species distribution.