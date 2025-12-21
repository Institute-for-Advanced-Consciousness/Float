# ============================================
# 05_radar_figs.R
# Produces radar figures for 11-ASC subscales
# ============================================

source("R/00_setup.R")
source("R/utils/radar_functions.R")

# Cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete

# ============================================
# 1. Simple radar (non-dose-response)
# ============================================

# Prepare data
psilo_ket_points <- get_psilo_ket_points()
ASC_code_summary <- prepare_asc_summary(only_complete_data)
radar_data_norm <- prepare_radar_data(ASC_code_summary, psilo_ket_points)

# Create and save "simple" plot
radar_plot <- create_simple_radar(radar_data_norm)
# Create output/figures directory if it doesn't exist
if(!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)}
ggsave("output/figures/simple_radar_plot.png", radar_plot, width = 10, height = 8, dpi = 300)

# ============================================
# 2. Dose-response radar
# ============================================

dose_file <- "data/phenom_psilo_float.csv"

dose_response_points <- read.csv(dose_file, check.names = FALSE)
dose_radar_reordered <- prepare_dose_radar_data(dose_response_points)
dose_radar_plot <- create_dose_radar(dose_radar_reordered)
  
ggsave("output/figures/dose_response_radar_plot.png", dose_radar_plot, width = 10, height = 8, dpi = 300)
cat("\nRadar figures complete. Saved to output/figures/\n")
