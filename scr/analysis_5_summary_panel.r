#############################################
# ANALYSIS 5: FINAL SUMMARY PANEL
#
# Objective:
# Combine the main results into one final figure:
# A) spatial distribution map
# B) Random Forest variable importance
# C) environmental violin plots
# D) PCA environmental niche space
#############################################

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyr)
library(cowplot)
library(patchwork)

# =========================
# A) MAP
# =========================

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

graphA <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey96",
    color = "grey75",
    linewidth = 0.2
  ) +
  geom_point(
    data = matrix_analysis,
    aes(x = longitude, y = latitude, color = species),
    alpha = 0.55,
    size = 1.3
  ) +
  coord_sf(
    xlim = c(-10, 30),
    ylim = c(35, 70),
    expand = FALSE
  ) +
  scale_color_manual(values = species_cols) +
  theme_void(base_size = 11) +
  labs(
    title = "B.",
    color = "Species"
  ) +
  theme(
    plot.title = element_text(face = "plain", size = 9, hjust = 0),
    legend.position = "bottom",
    aspect.ratio = 1
  )

# =========================
# B) RANDOM FOREST IMPORTANCE
# =========================

graphB <- plot_rf +
  labs(
    title = "D.",
    subtitle = NULL
  ) +
  theme(
    plot.title = element_text(face = "plain", size = 9, hjust = 0)
  )

# =========================
# C) ENVIRONMENTAL VIOLIN PLOTS
# =========================

graphC <- plot_env_violin +
  labs(
    title = "A.",
    subtitle = NULL
  ) +
  theme(
    plot.title = element_text(face = "plain", size = 9, hjust = 0),
    legend.position = "none"
  )

# =========================
# D) PCA NICHE SPACE
# =========================

graphD <- plot_pca +
  coord_fixed() +
  labs(
    title = "C.",
    subtitle = NULL
  ) +
  theme(
    plot.title = element_text(face = "plain", size = 9, hjust = 0),
    aspect.ratio = 1,
    legend.position = "none"
  )

# =========================
# FINAL PANEL
# =========================

# Extract ONLY the legend from the spatial map
legend_species <- cowplot::get_legend(
  graphA +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
)

# Remove legends from all plots inside the panel
graphA_clean <- graphA +
  theme(legend.position = "none")

graphB_clean <- graphB +
  theme(legend.position = "none")

graphC_clean <- graphC +
  theme(legend.position = "none")

graphD_clean <- graphD +
  theme(legend.position = "none")

# Build the figure without any internal legends
main_panel <- (
  graphC_clean
) / (
  graphA_clean | graphD_clean
) / (
  graphB_clean
) +
  plot_layout(
    heights = c(1.5, 1.8, 0.7)
  ) +
  plot_annotation(
    title = "Environmental niche comparison of Vespa velutina and Vespa crabro"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0.5)
  )

# Add the extracted map legend at the very bottom
figure_finale <- cowplot::plot_grid(
  main_panel,
  legend_species,
  ncol = 1,
  rel_heights = c(1, 0.06)
)

print(figure_finale)

ggsave(
  filename = "inst/figures/final_summary_panel_A4.png",
  plot = figure_finale,
  width = 8.27,
  height = 11.69,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Interpretation:
# The final panel summarizes the main results of the project. The map shows
# that Vespa velutina is mainly concentrated in western Europe, whereas
# Vespa crabro is more broadly distributed across the study area.
#
# The PCA and violin plots show strong overlap between the environmental
# conditions occupied by both species. This suggests that Vespa velutina
# currently occupies environments broadly similar to those of the native
# Vespa crabro.
#
# However, the Random Forest analysis shows that the two species can still
# be discriminated using environmental variables, with temperature being the
# most important predictor. This indicates that niche differences are subtle
# and mainly detectable when several variables are considered together.