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
  "remotes",
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

# Update fs to fix version conflicts
install.packages("fs", repos = "https://cloud.r-project.org/")

# Install ggradar from GitHub if needed
if (!require("ggradar", quietly = TRUE)) {
  remotes::install_github("ricardo-bion/ggradar")
}

# Load all packages
lapply(packages, library, character.only = TRUE)
library(ggradar)

# Set options
set.seed(420)           # Reproducibility in bootstrapping

cat("All packages loaded successfully.\n")
