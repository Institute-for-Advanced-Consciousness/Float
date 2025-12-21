# ============================================
# 00_setup.R
# Load packages, set options
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
  "stringr"
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)

# Set options
set.seed(420)           # Reproducibility in bootstrapping

cat("All packages loaded successfully.\n")
