# ============================================
# run_all.R
# Master script to run entire pipeline
# ============================================

cat("Float Analysis Pipeline\n")
start_time <- Sys.time()

# ============================================
# 1. Clean the data
# ============================================
cat("Step 1: Data Cleaning\n")
source("R/01_data_cleaning.R")

# ============================================
# 2. ANOVA Analyses
# ============================================
cat("Step 2: ANOVA Analyses\n")
cat("\n[2a] 11D-ASC ANOVA\n")
source("R/02_anova_11dasc.R")
cat("\n[2b] 5D-ASC ANOVA\n")
source("R/02_anova_5dasc.R")
cat("\n[2c] Interoception ANOVA\n")
source("R/02_anova_intero.R")
cat("\n[2d] Clinical Outcomes ANOVA\n")
source("R/02_anova_clinical.R")
cat("\n[2e] PANAS ANOVA\n")
source("R/02_anova_panas.R")
cat("\n")

# ============================================
# 3. Mediation Analyses
# ============================================
cat("Step 3: Mediation Analyses\n")
cat("NOTE: Mediation analyses use bootstrapping and may take 30-60+ minutes\n\n")

cat("\n[3a] Simple Mediation Models\n")
source("R/03_mediation_simple.R")
cat("\n[3b] OB Follow-Up Mediation\n")
source("R/03_mediation_ob_followup.R")
cat("\n[3c] Phenomenology Follow-Up Mediation\n")
source("R/03_mediation_phenom.R")
cat("\n")

# ============================================
# 4. Pre-Post Comparisons
# ============================================
cat("Step 4: Pre-Post Comparisons\n")
cat("----------------------------------------\n")
source("R/04_ttests_prepost.R")
cat("\n")

# ============================================
# 5. Radar Figures
# ============================================
cat("Step 5: Radar Figures\n")
source("R/05_radar_figs.R")
cat("\n")

# ============================================
# 6. OB Barplot
# ============================================
cat("Step 6: OB Barplot Figure\n")
source("R/05_OB_barplot.R")
cat("\n")

# ============================================
# 7. Measure Administration Table
# ============================================
cat("Step 7: Measure Administration Table\n")
source("R/05_measure_admin_table.R")
cat("\n")


# ============================================
# Summary
# ============================================
end_time <- Sys.time()
duration <- difftime(end_time, start_time, units = "mins")

cat("Pipeline Complete.\n")
cat("Total time:", round(duration, 1), "minutes\n\n")

cat("Output files created in output/\n")
