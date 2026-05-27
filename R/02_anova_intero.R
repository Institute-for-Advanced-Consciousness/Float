# ============================================
# 02_anova_intero.R
# Between-group differences: Interoception measures
# ============================================

source("R/00_setup.R")
source("R/utils/anova_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
raw_data <- dat$raw_data
only_complete_data <- dat$only_complete
subscales_intero <- dat$subscales_intero

# ============================================
# Run ANOVA for each interoception subscale
# ============================================


results_intero <- purrr::map2_dfr(
  subscales_intero, 
  names(subscales_intero), 
  ~run_anova_analysis(only_complete_data, .x, .y)
)

# ============================================
# Create and save table
# ============================================

table_intero <- create_anova_table(
  results_intero, 
  title = "Between-Group Differences: Interoception",
  include_test = TRUE
)


# Save table
gt::gtsave(table_intero, "output/tables/table_intero.docx")

cat("Interoception ANOVA complete. Table saved to output/tables/table_intero.docx\n")
