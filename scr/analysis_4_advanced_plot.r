#############################################
# ANALYSIS 5: ADVANCED INTERACTIVE FIGURE
#
# Objective:
# Create an interactive PCA figure using plotly.
#############################################

install.packages("plotly")

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

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
  geom_point(alpha = 0.7, size = 2) +
  theme_classic() +
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

dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)

htmlwidgets::saveWidget(
  plot_pca_interactive_html,
  "inst/figures/interactive_pca.html",
  selfcontained = FALSE
)

# Interpretation:
# The interactive PCA allows detailed exploration of individual occurrence
# records. Points can be inspected to identify whether specific regions or
# species are associated with particular environmental gradients.

library(sf)
library(rnaturalearth)
library(ggplot2)

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

plot_final_map <- ggplot() +
  geom_sf(
    data = world,
    fill = "grey95",
    color = "grey70",
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
    size = 1.5
  ) +
  coord_sf(
    xlim = c(-10, 30),
    ylim = c(35, 70)
  ) +
  theme_classic() +
  scale_color_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  labs(
    title = "Spatial distribution of Vespa occurrences",
    subtitle = "Records used in the environmental analyses",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )

print(plot_final_map)

ggsave(
  "inst/figures/final_species_distribution_map.png",
  plot_final_map,
  width = 9,
  height = 7,
  dpi = 300
)