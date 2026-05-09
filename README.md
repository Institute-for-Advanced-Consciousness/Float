## Background

Analysis code for a randomized controlled trial examining how floatation-REST induces altered states of consciousness and enhances interoceptive awareness, and how these changes mediate therapeutic improvements.

## Repository Structure

```
floating/
├── run_all.R                 # Master script — runs full pipeline
├── data/                     # Data files (not included; see data/README.md)
├── output/                   # Generated tables and figures
└── R/
    ├── 00_setup.R            # Package loading
    ├── 01_data_cleaning.R    # Data processing and variable definitions
    ├── 02_anova_.R          # Between-group ANOVAs (5 scripts)
    ├── 03_mediation_.R      # Mediation analyses (3 scripts)
    ├── 04_ttests_prepost.R   # Pre-post comparisons
    ├── 05_.R                # Figures (radar, barplot), other tables
    └── utils/                # Helper functions
```

## Setup

This project uses `renv` for reproducible environments. After cloning:

**RStudio:**
```r
renv::restore()  # Installs exact package versions from renv.lock
```

**VS Code or Terminal:**
```bash
R --slave -e "renv::restore()"
```

This ensures you have the same package versions as the original analysis.

## How to Run

### 1. Add Data Files

Add data files to the `data/` folder (see `data/README.md` for details).

### 2. Select parameters

ANOVAs and figures take < 1 min each. Mediation scripts take 1-2 hours due to 5,000 bootstrapped samples (can decrease number of bootstrap samples in `utils/mediation_functions.R`).

### 3. Run Analysis

```r
setwd("/path/to/floating")
source("run_all.R")
```

## Data Access

Data files not included in this repository. Contact corresponding author (SKhalsa@mednet.ucla.edu) for access.

## Updates

[TBD]

## Citation
[Manuscript in preparation]
