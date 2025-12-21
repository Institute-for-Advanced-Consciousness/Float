# ============================================
# 02_anova_clinical.R
# Between-group differences: Clinical outcome variables
# ============================================

source("R/00_setup.R")
source("R/utils/anova_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete
subscales_clinical <- dat$subscales_clinical

# ============================================
# Run ANOVA for each clinical outcome
# ============================================


results_clinical <- purrr::map2_dfr(
  subscales_clinical, 
  names(subscales_clinical), 
  ~run_anova_analysis(only_complete_data, .x, .y)
)

# ============================================
# Create and save table
# ============================================

table_clinical <- create_anova_table(
  results_clinical, 
  title = "Between-Group Differences: Mediation Outcome Variables",
  include_test = TRUE
)


# Save table
gt::gtsave(table_clinical, "output/tables/table_clinical.png")

cat("Clinical Outcomes ANOVA complete. Table saved to output/tables/table_clinical.png\n")
