# ============================================
# 03_mediation_simple.R
# Simple mediation models: All mediators x outcomes
# ============================================

source("R/00_setup.R")
source("R/utils/mediation_functions.R")
source("R/utils/table_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
raw_data <- dat$raw_data
only_complete_data <- dat$only_complete
mediators <- dat$mediators
outcomes <- dat$outcomes
covariates <- dat$covariates

# ============================================
# Run all simple mediation models
# ============================================

simple_results <- list()
counter <- 1

cat("Running Simple Mediation Models...\n")
cat("This may take a while due to bootstrapping (5000 samples per model)\n\n")

for(i in seq_along(mediators)) {
  for(j in seq_along(outcomes)) {
    
    mediator <- mediators[i]
    outcome <- outcomes[j]
    model_name <- paste("Simple:", mediator, "->", outcome)
    
    # Select appropriate dataset based on mediator
    # ASC mediators use only_complete_data, interoception uses raw_data
    this_data <- if (startsWith(mediator, "alt")) {
      only_complete_data
    } else {
      raw_data
    }
    
    tryCatch({
      cat("Running:", model_name, "\n")
      
      result <- run_simple_mediation(
        data = this_data,
        mediator_var = mediator,
        outcome_var = outcome,
        covariates = covariates,
        model_name = model_name
      )
      
      simple_results[[counter]] <- result
      counter <- counter + 1
      
    }, error = function(e) {
      cat("Error with", model_name, ":", e$message, "\n")
    })
  }
}

# ============================================
# Create summary and add FDR correction
# ============================================

simple_summary <- create_simple_summary(simple_results)
simple_summary <- add_mediation_fdr(simple_summary)

# ============================================
# Create and save table
# ============================================

mediation_table <- create_simple_mediation_table(simple_summary)
print(mediation_table)

gt::gtsave(mediation_table, "output/tables/simple_mediation_table.png")

# Also save the summary data frame for reference
saveRDS(simple_summary, "output/simple_mediation_summary.rds")

cat("\nSimple mediation complete, able saved to output/tables/simple_mediation_table.png\n")
