# ============================================
# 03_mediation_phenom.R
# Post-hoc mediation: OB -> Positive Side Effects
# ============================================

source("R/00_setup.R")
source("R/utils/mediation_functions.R")
source("R/utils/table_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete
covariates <- dat$covariates

# ============================================
# Define phenomenology follow-up parameters
# ============================================

phenom_mediators <- c("alt_postfloat_6_dasc_5d_oceanic_boundlessness_score")

# Individual positive side effect outcomes (sec 30-44)
phenom_outcomes <- paste0("average_sec_", 30:44)

# ============================================
# Run phenomenology follow-up mediation models
# ============================================

post_hoc_phenom <- list()
counter <- 1


for (med in phenom_mediators) {
  for (out in phenom_outcomes) {
    model_name <- paste("Model", counter, ":", basename(med), "->", out)
    cat("Running:", model_name, "\n")
    
    result <- tryCatch(
      run_simple_mediation(
        data = only_complete_data,
        mediator_var = med,
        outcome_var = out,
        covariates = covariates,
        model_name = model_name
      ),
      error = function(e) {
        message("Error in ", model_name, ": ", e$message)
        return(NA)
      }
    )
    
    # Handle failed models
    if (is.atomic(result) || is.null(result)) {
      result <- data.frame(Model = model_name, Indirect_p = NA_real_)
    }
    
    post_hoc_phenom[[counter]] <- result
    counter <- counter + 1
  }
}

# ============================================
# Create summary and add Bonferroni correction
# ============================================

post_hoc_phenom_summary <- create_simple_summary(post_hoc_phenom)

# Bonferroni correction (more conservative for exploratory analysis)
post_hoc_phenom_summary$FDR_p <- p.adjust(post_hoc_phenom_summary$Indirect_p, method = "bonferroni")

# ============================================
# Create and save table
# ============================================

post_hoc_phenom_table <- create_phenom_followup_table(post_hoc_phenom_summary)

gt::gtsave(post_hoc_phenom_table, "output/tables/post_hoc_phenom_table.png")

# Save summary for reference
saveRDS(post_hoc_phenom_summary, "output/post_hoc_phenom_summary.rds")

cat("\nPhenomenology follow-up mediation complete, table saved to output/tables/post_hoc_phenom_table.png\n")
