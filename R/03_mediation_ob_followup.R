# ============================================
# 03_mediation_ob_followup.R
# Post-hoc mediation: Oceanic Boundlessness subscales
# ============================================

source("R/00_setup.R")
source("R/utils/mediation_functions.R")
source("R/utils/table_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete
covariates <- dat$covariates

# ============================================
# Define OB follow-up mediators and outcomes
# ============================================

# 11D-ASC subscales that comprise Oceanic Boundlessness
ob_mediators <- c(
  "alt_postfloat_6_dasc_11d_unity_score",
  "alt_postfloat_6_dasc_11d_spiritual_score",
  "alt_postfloat_6_dasc_11d_bliss_score",
  "alt_postfloat_6_dasc_11d_disembodiment_score",
  "alt_postfloat_6_dasc_11d_insight_score"
)

# Outcome from original significant finding
ob_outcomes <- c("diff_panasx_posaffect_score")

# ============================================
# Run OB follow-up mediation models
# ============================================

post_hoc_OB <- list()
counter <- 1

cat("Running Oceanic Boundlessness Follow-Up Mediation Models...\n")

for(med in ob_mediators) {
  for(out in ob_outcomes) {
    
    model_name <- paste("Model", counter, ":", basename(med), "->", out)
    cat("Running:", model_name, "\n")
    
    tryCatch({
      result <- run_simple_mediation(
        data = only_complete_data, 
        mediator_var = med,
        outcome_var = out,
        covariates = covariates,
        model_name = model_name
      )
      
      post_hoc_OB[[counter]] <- result
      counter <- counter + 1
      
    }, error = function(e) {
      cat("Error with", model_name, ":", e$message, "\n")
    })
  }
}

# ============================================
# Create summary and add FDR correction
# ============================================

post_hoc_OB_summary <- create_simple_summary(post_hoc_OB)

# Create mediator labels for FDR grouping
post_hoc_OB_summary$mediator <- case_when(
  grepl("unity", post_hoc_OB_summary$Model) ~ "Unity",
  grepl("spiritual", post_hoc_OB_summary$Model) ~ "Spiritual",
  grepl("bliss", post_hoc_OB_summary$Model) ~ "Bliss",
  grepl("disembodiment", post_hoc_OB_summary$Model) ~ "Disembodiment",
  grepl("insight", post_hoc_OB_summary$Model) ~ "Insight"
)

# FDR correction across all OB models
post_hoc_OB_summary$FDR_p <- p.adjust(post_hoc_OB_summary$Indirect_p, method = "BH")

# ============================================
# Create and save table
# ============================================

post_hoc_table <- create_ob_followup_table(post_hoc_OB_summary)
print(post_hoc_table)

gt::gtsave(post_hoc_table, "output/tables/post_hoc_ob_table.png")

# Save summary for reference
saveRDS(post_hoc_OB_summary, "output/post_hoc_ob_summary.rds")

cat("\nOB follow-up mediation complete, table saved to output/tables/post_hoc_ob_table.png\n")
