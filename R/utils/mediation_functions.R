# ============================================
# utils/mediation_functions.R
# Shared functions for running mediation analyses
# ============================================

# Suppress R CMD check notes about dplyr/tidyverse NSE
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Model", "mediator", "outcome", "family",
    "Indirect_p", "FDR_p"
  ))
}

#' Run simple mediation analysis using lavaan
#'
#' @param data Data frame containing all variables
#' @param mediator_var Character string of mediator variable name
#' @param outcome_var Character string of outcome variable name
#' @param predictor_var Character string of predictor variable (default: "binary_group_num")
#' @param covariates Character vector of covariate names (can be NULL)
#' @param bootstrap Number of bootstrap samples (default: 5000)
#' @param model_name Optional name for the model
#' @return List containing all path coefficients and model statistics
run_simple_mediation <- function(data, mediator_var, outcome_var, 
                                 predictor_var = "pooled_group_num", 
                                 covariates = NULL, 
                                 bootstrap = 5000, model_name = NULL) {

  # Add covariates to model
  covs_string <- ""
  if(!is.null(covariates) && length(covariates) > 0) {
    covs_string <- paste(" +", paste(covariates, collapse = " + "))
  }
  
  
  model_string <- paste0('
  # Mediator model (a path)
  ', mediator_var, ' ~ a*', predictor_var, covs_string, '

  # Outcome model (b and c\' paths)
  ', outcome_var, ' ~ b*', mediator_var, ' + cp*', predictor_var, covs_string, '

  # Indirect effect
  indirect := a * b

  # Total effect
  total := cp + indirect

  # Proportion mediated
  prop_mediated := indirect / total
')
  
  # Fit with bootstrapped SEs
  
  fit <- lavaan::sem(
    model_string, 
    data = data,
    se = "bootstrap",
    bootstrap = bootstrap
  )
  
  # Get parameter estimates with standardized coefficients
  params <- lavaan::parameterEstimates(fit, boot.ci.type = "bca.simple", standardized = TRUE)
  fit_indices <- lavaan::fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr", "aic", "bic"))
  r2 <- lavaan::inspect(fit, "r2")
  
  # Extract specific path coefficients
  a_path <- params[params$label == "a", ]
  b_path <- params[params$label == "b", ]
  cp_path <- params[params$label == "cp", ]
  indirect <- params[params$label == "indirect", ]
  total <- params[params$label == "total", ]
  prop_med <- params[params$label == "prop_mediated", ]
  
  return(list(
    model_name = ifelse(is.null(model_name), paste("Simple:", mediator_var, "->", outcome_var), model_name),
    fit = fit,
    
    # A-path coefficients (IV -> Mediator)
    a_path_b = ifelse(nrow(a_path) > 0, a_path$est, NA),
    a_path_b_std = ifelse(nrow(a_path) > 0, a_path$std.all, NA),
    a_path_se = ifelse(nrow(a_path) > 0, a_path$se, NA),
    a_path_p = ifelse(nrow(a_path) > 0, a_path$pvalue, NA),
    a_path_ci_lower = ifelse(nrow(a_path) > 0, a_path$ci.lower, NA),
    a_path_ci_upper = ifelse(nrow(a_path) > 0, a_path$ci.upper, NA),
    
    # B-path coefficients (Mediator -> DV)
    b_path_b = ifelse(nrow(b_path) > 0, b_path$est, NA),
    b_path_b_std = ifelse(nrow(b_path) > 0, b_path$std.all, NA),
    b_path_se = ifelse(nrow(b_path) > 0, b_path$se, NA),
    b_path_p = ifelse(nrow(b_path) > 0, b_path$pvalue, NA),
    b_path_ci_lower = ifelse(nrow(b_path) > 0, b_path$ci.lower, NA),
    b_path_ci_upper = ifelse(nrow(b_path) > 0, b_path$ci.upper, NA),
    
    # Direct effect coefficients (c' path)
    direct_effect_b = ifelse(nrow(cp_path) > 0, cp_path$est, NA),
    direct_effect_b_std = ifelse(nrow(cp_path) > 0, cp_path$std.all, NA),
    direct_effect_se = ifelse(nrow(cp_path) > 0, cp_path$se, NA),
    direct_effect_p = ifelse(nrow(cp_path) > 0, cp_path$pvalue, NA),
    direct_effect_ci_lower = ifelse(nrow(cp_path) > 0, cp_path$ci.lower, NA),
    direct_effect_ci_upper = ifelse(nrow(cp_path) > 0, cp_path$ci.upper, NA),
    
    # Indirect effect
    indirect_effect = ifelse(nrow(indirect) > 0, indirect$est, NA),
    indirect_effect_std = ifelse(nrow(indirect) > 0, indirect$std.all, NA),
    indirect_se = ifelse(nrow(indirect) > 0, indirect$se, NA),
    indirect_p = ifelse(nrow(indirect) > 0, indirect$pvalue, NA),
    indirect_ci_lower = ifelse(nrow(indirect) > 0, indirect$ci.lower, NA),
    indirect_ci_upper = ifelse(nrow(indirect) > 0, indirect$ci.upper, NA),
    
    # Total effect
    total_effect = ifelse(nrow(total) > 0, total$est, NA),
    total_effect_std = ifelse(nrow(total) > 0, total$std.all, NA),
    total_se = ifelse(nrow(total) > 0, total$se, NA),
    total_p = ifelse(nrow(total) > 0, total$pvalue, NA),
    total_ci_lower = ifelse(nrow(total) > 0, total$ci.lower, NA),
    total_ci_upper = ifelse(nrow(total) > 0, total$ci.upper, NA),
    
    # Other statistics
    prop_mediated = ifelse(nrow(prop_med) > 0, prop_med$est, NA),
    r_squared = ifelse(length(r2) > 0, r2[length(r2)], NA),
    cfi = fit_indices["cfi"],
    rmsea = fit_indices["rmsea"]
  ))
}


#' Create summary data frame from list of mediation results
#'
#' @param results_list List of results from run_simple_mediation()
#' @return Data frame with all path coefficients and statistics
create_simple_summary <- function(results_list) {
  
  summary_df <- data.frame(
    Model = character(0),
    A_Path_B = numeric(0),
    A_Path_B_Std = numeric(0),
    A_Path_p = numeric(0),
    A_Path_CI = character(0),
    B_Path_B = numeric(0),
    B_Path_B_Std = numeric(0),
    B_Path_p = numeric(0),
    B_Path_CI = character(0),
    Direct_B = numeric(0),
    Direct_B_Std = numeric(0),
    Direct_p = numeric(0),
    Direct_CI = character(0),
    Indirect_B = numeric(0),
    Indirect_B_Std = numeric(0),
    Indirect_p = numeric(0),
    Indirect_CI = character(0),
    Total_B = numeric(0),
    Total_B_Std = numeric(0),
    Total_p = numeric(0),
    Total_CI = character(0),
    Prop_Mediated = numeric(0),
    R_squared = numeric(0),
    CFI = numeric(0),
    RMSEA = numeric(0)
  )
  
  for(result in results_list) {
    summary_df <- rbind(summary_df, data.frame(
      Model = result$model_name,
      
      # A-path
      A_Path_B = round(result$a_path_b, 3),
      A_Path_B_Std = round(result$a_path_b_std, 3),
      A_Path_p = round(result$a_path_p, 3),
      A_Path_CI = paste0("[", round(result$a_path_ci_lower, 3), ", ", round(result$a_path_ci_upper, 3), "]"),
      
      # B-path
      B_Path_B = round(result$b_path_b, 3),
      B_Path_B_Std = round(result$b_path_b_std, 3),
      B_Path_p = round(result$b_path_p, 3),
      B_Path_CI = paste0("[", round(result$b_path_ci_lower, 3), ", ", round(result$b_path_ci_upper, 3), "]"),
      
      # Direct effect
      Direct_B = round(result$direct_effect_b, 3),
      Direct_B_Std = round(result$direct_effect_b_std, 3),
      Direct_p = round(result$direct_effect_p, 3),
      Direct_CI = paste0("[", round(result$direct_effect_ci_lower, 3), ", ", round(result$direct_effect_ci_upper, 3), "]"),
      
      # Indirect effect
      Indirect_B = round(result$indirect_effect, 3),
      Indirect_B_Std = round(result$indirect_effect_std, 3),
      Indirect_p = round(result$indirect_p, 3),
      Indirect_CI = paste0("[", round(result$indirect_ci_lower, 3), ", ", round(result$indirect_ci_upper, 3), "]"),
      
      # Total effect
      Total_B = round(result$total_effect, 3),
      Total_B_Std = round(result$total_effect_std, 3),
      Total_p = round(result$total_p, 3),
      Total_CI = paste0("[", round(result$total_ci_lower, 3), ", ", round(result$total_ci_upper, 3), "]"),
      
      # Model statistics
      Prop_Mediated = round(result$prop_mediated, 3),
      R_squared = round(result$r_squared, 3),
      CFI = round(result$cfi, 3),
      RMSEA = round(result$rmsea, 3)
    ))
  }
  
  return(summary_df)
}


#' Add FDR correction to mediation summary by family
#'
#' @param simple_summary Data frame from create_simple_summary()
#' @return Data frame with FDR_p column added
add_mediation_fdr <- function(simple_summary) {
  
  if(nrow(simple_summary) == 0) return(simple_summary)
  
  # Define families
  phenomenology_mediators <- c("alt_postfloat_6_dasc_5d_anxious_ego_dissolution_score", 
                               "alt_postfloat_6_dasc_5d_oceanic_boundlessness_score")
  
  interoception_one_mediators <- c("maias_bodylisten", "maias_emoaware", "maias_selfreg")
  
  interoception_two_mediators <- c("breath_intensity_mean", "heart_intensity_mean", 
                                   "gi_intensity_mean", "breath_pleasant_mean",
                                   "heart_pleasant_mean", "gi_pleasant_mean")
  
  affect_outcomes <- c("diff_panasx_posaffect_score", "diff_panasx_negaffect_score")
  anxiety_outcomes <- c("diff_mean_stais", "diff_hama", "diff_asi3r_total")
  
  # Extract mediator and outcome from model name
  simple_summary$mediator <- gsub("Simple: (.*) -> .*", "\\1", simple_summary$Model)
  simple_summary$outcome <- gsub("Simple: .* -> (.*)", "\\1", simple_summary$Model)
  
  # Create family labels
  simple_summary$family <- paste0(
    ifelse(simple_summary$mediator %in% phenomenology_mediators, "Phenomenology", 
           ifelse(simple_summary$mediator %in% interoception_one_mediators, "Interoception_One", "Interoception_Two")), "_",
    ifelse(simple_summary$outcome %in% affect_outcomes, "Affect",
           ifelse(simple_summary$outcome %in% anxiety_outcomes, "Anxiety", "Depression"))
  )
  
  # Apply FDR by family
  simple_summary$FDR_p <- round(ave(simple_summary$Indirect_p, simple_summary$family, 
                                    FUN = function(x) p.adjust(x, method = "BH")), 3)
  
  return(simple_summary)
}
