# ============================================
# 02_anova_5dasc.R
# Between-group differences: 5D-ASC Questionnaire
# ============================================

source("R/00_setup.R")
source("R/utils/anova_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete
subscales_5d <- dat$subscales_5d

# ============================================
# Run ANOVA for each 5D-ASC subscale
# ============================================

results_5d <- purrr::map2_dfr(
  subscales_5d, 
  names(subscales_5d), 
  ~run_anova_analysis(only_complete_data, .x, .y)
)

# ============================================
# Create and save table
# ============================================

table_5dasc <- create_anova_table(
  results_5d, 
  title = "Between-Group Differences: 5D-ASC Questionnaire",
  include_test = TRUE
)


# Save table
#gt::gtsave(table_5dasc, "output/tables/table_5dasc.png")
gt::gtsave(table_5dasc, "output/tables/table_5dasc.docx")

cat("5D-ASC ANOVA complete. Table saved to output/tables/table_5dasc.docx\n")
