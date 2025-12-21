# ============================================
# 05_OB_barplot.R
# Produces OB ANOVA barplot figure
# ============================================

source("R/00_setup.R")
source("R/utils/radar_functions.R")    # For group_labels, fived_labels
source("R/utils/barplot_functions.R")

# Cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete

# ============================================
# 1. Prep data for plotting
# ============================================

OB_summary <- prepare_OB_summary(only_complete_data)

# ============================================
# 2. Games-howell on p-values
# ============================================

cat("Running Games-Howell post-hoc test for Oceanic Boundlessness...\n")
gh_results <- run_games_howell_OB(only_complete_data)

# ============================================
# 3. Create, save plot
# ============================================

OB_plot <- create_OB_barplot(OB_summary, gh_results)
print(OB_plot)

ggsave("output/figures/OB_barplot.png", OB_plot, width = 8, height = 6, dpi = 300)

cat("\nOB barplot complete! Saved to output/figures/OB_barplot.png\n")