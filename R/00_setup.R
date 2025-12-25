# ============================================
# 00_setup.R
# Load/install packages, set options
# ============================================

packages <- c(
  "tidyverse",
  "lavaan",
  "gt",
  "effectsize",
  "car",
  "purrr",
  "broom",
  "ggradar",
  "ggdist",
  "ggsignif",
  "ggpubr",
  "stringr",
  "gtsummary",
  "readxl"
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed], repos = "https://cloud.r-project.org/")
}

# Load all packages
lapply(packages, library, character.only = TRUE)

# Set options
set.seed(420)           # Reproducibility in bootstrapping

cat("All packages loaded successfully.\n")
