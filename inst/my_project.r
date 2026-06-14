#############################################
# FINAL PROJECT
# Applied Biological Data Analysis — June 2026
#
# Ecological question:
# Does the invasive hornet Vespa velutina occupy
# different environmental conditions compared to
# the native Vespa crabro across Europe?
#
# Objective:
# Run all final analyses starting from the final
# environmental matrix: data/matrix_full.csv
#############################################

# 1) Load and prepare the final environmental matrix
source("scr/step1_read_matrix.R")

# 2) PCA environmental niche analysis
source("scr/analysis_1_pca.R")

# 3) Environmental comparison between species
source("scr/analysis_2_environmental_comparison.R")

# 4) Random Forest discriminating variable analysis
source("scr/analysis_3_random_forest.R")

# 5) Advanced plot: spatial map and interactive PCA
source("scr/analysis_4_advanced_plot.R")

# 6) Final summary panel
source("scr/analysis_5_summary_panel.R")