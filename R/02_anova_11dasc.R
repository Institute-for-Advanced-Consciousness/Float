# ============================================
# 02_anova_11dasc.R
# Between-group differences: 11-ASC Questionnaire
# ============================================

source("R/00_setup.R")
source("R/utils/anova_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete
subscales_11d <- dat$subscales_11d

# ============================================
# Run ANOVA for each 11D-ASC subscale
# ============================================


results_11d <- purrr::map2_dfr(
  subscales_11d, 
  names(subscales_11d), 
  ~run_anova_analysis(only_complete_data, .x, .y)
)

# ============================================
# Create and save table
# ============================================

table_11dasc <- create_anova_table(
  results_11d, 
  title = "Between-Group Differences: 11-ASC Questionnaire",
  include_test = FALSE
)


# Save table
gt::gtsave(table_11dasc, "output/tables/table_11dasc.png")

cat("11-ASC ANOVA complete. Table saved to output/tables/table_11dasc.png\n")
