#############################################
# ANALYSIS 4: ADVANCED FIGURES
#
# Objective:
# Create complementary figures to explore the
# spatial and environmental distribution of
# Vespa velutina and Vespa crabro.
#
# This script produces:
# 1) A static map of European occurrence records
# 2) An interactive PCA figure
#############################################

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(sf)
library(rnaturalearth)

# =========================
# 1) MAP OF OCCURRENCES
# =========================

# Load country boundaries for the background map.
world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# The map shows where the occurrence records used in the analyses are located.
# This helps determine whether environmental differences may also reflect
# geographic differences in the distribution of the two species.
plot_final_map <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey96",
    color = "grey75",
    linewidth = 0.2
  ) +
  geom_point(
    data = matrix_analysis,
    aes(
      x = longitude,
      y = latitude,
      color = species
    ),
    alpha = 0.55,
    size = 1.4
  ) +
  coord_sf(
    xlim = c(-10, 30),
    ylim = c(35, 70),
    expand = FALSE
  ) +
  scale_color_manual(values = species_cols) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "Spatial distribution of Vespa occurrences in Europe",
    subtitle = "Records used for environmental niche comparison",
    color = "Species"
  )

print(plot_final_map)

dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)

ggsave(
  "inst/figures/final_species_distribution_map.png",
  plot_final_map,
  width = 9,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Interpretation:
# The map shows that both species are represented across the European study
# area, but their spatial distributions are not identical. This spatial pattern
# is important to consider because environmental differences between species
# may partly reflect geographic differences in where records are available.
#
# Vespa velutina is expected to be more concentrated in regions where the
# invasion is established, while Vespa crabro has a broader native European
# distribution. Therefore, the spatial map provides ecological context for the
# PCA, environmental comparisons and Random Forest results.

# =========================
# 2) INTERACTIVE PCA
# =========================

# The interactive PCA uses the PCA scores generated in analysis_1_pca.R.
# It allows individual records to be inspected by species and coordinates.
plot_pca_interactive <- ggplot(
  pca_scores,
  aes(
    x = PC1,
    y = PC2,
    color = species,
    text = paste(
      "Species:", species,
      "<br>Longitude:", round(longitude, 2),
      "<br>Latitude:", round(latitude, 2)
    )
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  scale_color_manual(values = species_cols) +
  theme_vespa() +
  labs(
    title = "Interactive PCA of Vespa environmental niche",
    x = paste0("PC1 (", round(pca_var[1], 1), "%)"),
    y = paste0("PC2 (", round(pca_var[2], 1), "%)"),
    color = "Species"
  )

plot_pca_interactive_html <- plotly::ggplotly(
  plot_pca_interactive,
  tooltip = "text"
)

htmlwidgets::saveWidget(
  plot_pca_interactive_html,
  "inst/figures/interactive_pca.html",
  selfcontained = FALSE
)

# Interpretation:
# The map shows clear differences in the geographic distribution of the two
# species across Europe.
#
# Vespa crabro is widely distributed throughout the study area, including
# both western and eastern Europe, which is consistent with its status as a
# native European hornet.
#
# In contrast, Vespa velutina is mainly concentrated in western Europe and is
# not represented in eastern regions. This pattern is consistent with
# the ongoing expansion of the invasive species, which has not yet colonized
# all parts of Europe to the same extent.
#
# These spatial differences are important when interpreting environmental
# analyses because part of the observed environmental variation may reflect
# differences in geographic distribution rather than strict ecological
# preferences.