setwd("~/Documents/theo_from_dropbox/IACS/Float/floating")

# Run data cleaning first
source("R/01_data_cleaning.R")

source("R/utils/mediation_functions.R")

# Fewer bootstrap samples
test1 <- run_simple_mediation(
  data = dat$only_complete,
  mediator_var = "alt_postfloat_6_dasc_5d_oceanic_boundlessness_score",
  outcome_var = "diff_panasx_posaffect_score",
  covariates = "age",
  bootstrap = 50
)

# More bootstrap samples
test2 <- run_simple_mediation(
  data = dat$only_complete,
  mediator_var = "alt_postfloat_6_dasc_5d_oceanic_boundlessness_score",
  outcome_var = "diff_panasx_posaffect_score",
  covariates = "age",
  bootstrap = 5000
)

# Compare indirect effect confidence intervals
c(test1$indirect_ci_lower, test1$indirect_ci_upper)
c(test2$indirect_ci_lower, test2$indirect_ci_upper)
c(test1$indirect_se, test2$indirect_se)

#source("run_all.R")
