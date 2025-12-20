# ============================================
# 02_anova_panas.R
# Between-group differences: PANAS subscales
# ============================================

source("R/00_setup.R")
source("R/utils/anova_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete
subscales_panas <- dat$subscales_panas

# ============================================
# Run ANOVA for each PANAS subscale
# ============================================

results_panas <- purrr::map2_dfr(
  subscales_panas, 
  names(subscales_panas), 
  ~run_anova_analysis(only_complete_data, .x, .y)
)

# ============================================
# Create and save table
# ============================================

table_panas <- create_anova_table(
  results_panas, 
  title = "Between-Group Differences: PANAS",
  include_test = TRUE
)

print(table_panas)

# Save table
gt::gtsave(table_panas, "output/tables/table_panas.png")

cat("PANAS ANOVA complete. Table saved to output/tables/table_panas.png\n")
