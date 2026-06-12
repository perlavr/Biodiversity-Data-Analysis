#############################################
# ANALYSIS 5: FINAL SUMMARY PANEL
#############################################

install.packages(c("fmsb", "cowplot"))
install.packages("gridGraphics")

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyr)
library(fmsb)
library(cowplot)
library(gridGraphics)

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
    size = 1.3
  ) +
  coord_sf(
    xlim = c(-10, 30),
    ylim = c(35, 70),
    expand = FALSE
  ) +
  scale_color_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  theme_void() +
  labs(
    title = "A. Spatial distribution of Vespa occurrences",
    color = "Species"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

# =========================
# B) RADAR CHART
# =========================

profiles <- matrix_analysis %>%
  group_by(species) %>%
  summarise(
    Temperature = mean(tas_mean_c, na.rm = TRUE),
    Precipitation = mean(prec_mean_annual, na.rm = TRUE),
    Elevation = mean(elevation_m, na.rm = TRUE),
    NDVI = mean(NDVI, na.rm = TRUE),
    .groups = "drop"
  )

profiles_mat <- as.data.frame(profiles[, -1])
rownames(profiles_mat) <- profiles$species

profiles_norm <- as.data.frame(
  lapply(profiles_mat, function(x) {
    if (max(x) == min(x)) {
      rep(0.5, length(x))
    } else {
      (x - min(x)) / (max(x) - min(x))
    }
  })
)

rownames(profiles_norm) <- rownames(profiles_mat)

radar_data <- rbind(
  rep(1, 4),
  rep(0, 4),
  profiles_norm
)

cols_c <- c("#1B7837", "#C51B7D")
cols_f <- adjustcolor(cols_c, alpha.f = 0.25)

graphB_grob <- cowplot::as_grob(~ {
  par(mar = c(1, 1, 3, 1))
  fmsb::radarchart(
    radar_data,
    axistype = 1,
    pcol = cols_c,
    pfcol = cols_f,
    plwd = 2,
    cglcol = "grey85",
    cglty = 1,
    vlcex = 0.8
  )
  title("B. Mean environmental profile", cex.main = 0.9, font.main = 2)
  legend(
    "topright",
    legend = rownames(profiles_norm),
    col = cols_c,
    lwd = 2,
    bty = "n",
    cex = 0.7
  )
})

# =========================
# C) ENVIRONMENTAL VIOLIN PLOTS
# =========================

graphC <- matrix_analysis %>%
  select(species, tas_mean_c, prec_mean_annual, elevation_m, NDVI) %>%
  pivot_longer(
    cols = c(tas_mean_c, prec_mean_annual, elevation_m, NDVI),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      tas_mean_c = "Temperature",
      prec_mean_annual = "Precipitation",
      elevation_m = "Elevation",
      NDVI = "NDVI"
    )
  ) %>%
  ggplot(aes(x = species, y = value, fill = species)) +
  geom_violin(alpha = 0.6, trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.size = 0.4, alpha = 0.9) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  theme_classic(base_size = 10) +
  labs(
    title = "C. Environmental conditions by species",
    x = "",
    y = "Value"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# =========================
# D) CLIMATE SPACE
# =========================

graphD <- ggplot(
  matrix_analysis,
  aes(
    x = tas_mean_c,
    y = prec_mean_annual,
    color = species,
    fill = species
  )
) +
  geom_point(alpha = 0.35, size = 1.4) +
  stat_ellipse(
    geom = "polygon",
    alpha = 0.15,
    color = NA
  ) +
  scale_color_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  scale_fill_manual(values = c(
    "Vespa velutina" = "#1B7837",
    "Vespa crabro" = "#C51B7D"
  )) +
  theme_classic(base_size = 10) +
  labs(
    title = "D. Climatic niche space",
    x = "Mean annual temperature (°C)",
    y = "Annual precipitation",
    color = "Species",
    fill = "Species"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

# =========================
# FINAL PANEL
# =========================

figure_finale <- cowplot::ggdraw() +
  cowplot::draw_plot(graphA,      x = 0.00, y = 0.60, width = 1.00, height = 0.40) +
  cowplot::draw_plot(graphB_grob, x = 0.00, y = 0.28, width = 0.42, height = 0.32) +
  cowplot::draw_plot(graphC,      x = 0.42, y = 0.28, width = 0.58, height = 0.32) +
  cowplot::draw_plot(graphD,      x = 0.00, y = 0.00, width = 1.00, height = 0.28)

print(figure_finale)

dir.create("inst/figures", showWarnings = FALSE, recursive = TRUE)

ggsave(
  "inst/figures/final_summary_panel.png",
  figure_finale,
  width = 13,
  height = 16,
  dpi = 300,
  bg = "white"
)

# Interpretation:
# The final panel compares the spatial and environmental niches of the two
# Vespa species. The map shows their European distribution, the radar chart
# summarizes their mean environmental profiles, the violin plots compare the
# full distribution of each environmental variable, and the climate-space plot
# shows whether the two species occupy overlapping or distinct temperature and
# precipitation conditions.