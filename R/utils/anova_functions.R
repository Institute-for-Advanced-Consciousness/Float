# ============================================
# utils/anova_functions.R
# Shared functions for running ANOVA analyses
# ============================================

#' Run ANOVA analysis with automatic test selection
#' 
#' Checks assumptions (Levene's test, Shapiro-Wilk) and selects
#' one-way ANOVA or Welch ANOVA accordingly. Computes generalized eta squared.
#'
#' @param data Data frame containing the variables
#' @param var Character string of the dependent variable column name
#' @param var_name Character string for display name in output
#' @param group_var Character string of the grouping variable (default: "group_name")
#' @return Data frame with one row containing analysis results
run_anova_analysis <- function(data, var, var_name, group_var = "group_name") {
  
  # Subset to complete cases
  data <- data[!is.na(data[[var]]) & !is.na(data[[group_var]]), ]
  
  # Assumption checks: Levene's test + Shapiro-Wilk test
  levene_p <- car::leveneTest(data[[var]] ~ as.factor(data[[group_var]]))$`Pr(>F)`[1]
  shapiro_p <- shapiro.test(residuals(lm(data[[var]] ~ data[[group_var]])))$p.value
  assumptions_met <- levene_p > 0.05 & shapiro_p > 0.05
  
  # Initialize

  gen_eta_squared <- NA
  
  if (assumptions_met) {
    # Standard one-way ANOVA
    aov_result <- aov(data[[var]] ~ as.factor(data[[group_var]]))
    anova_result <- summary(aov_result)[[1]]
    f_stat <- anova_result$`F value`[1]
    df1 <- anova_result$Df[1]
    df2 <- anova_result$Df[2]
    p_val <- anova_result$`Pr(>F)`[1]
    test_used <- "One-way ANOVA"
    
    # Generalized eta squared from effectsize
    eta_result <- effectsize::eta_squared(aov_result, generalized = TRUE)
    gen_eta_squared <- eta_result$Eta2_generalized[1]
    
  } else {
    # Welch ANOVA (robust to heterogeneity of variance)
    oneway_result <- oneway.test(data[[var]] ~ as.factor(data[[group_var]]))
    f_stat <- oneway_result$statistic
    df1 <- oneway_result$parameter[1]
    df2 <- oneway_result$parameter[2]
    p_val <- oneway_result$p.value
    test_used <- "Welch ANOVA"
    
    # Manual generalized eta squared computation
    group_means <- aggregate(data[[var]], by = list(data[[group_var]]), FUN = mean, na.rm = TRUE)
    overall_mean <- mean(data[[var]], na.rm = TRUE)
    group_sizes <- table(data[[group_var]])
    ss_between <- sum(group_sizes * (group_means$x - overall_mean)^2)
    ss_total <- sum((data[[var]] - overall_mean)^2, na.rm = TRUE)
    gen_eta_squared <- ss_between / ss_total
  }
  
  return(data.frame(
    Subscale = var_name,
    Test = test_used,
    F_stat = round(f_stat, 2),
    df1 = round(df1, 2),
    df2 = round(df2, 1),
    p_value = p_val,
    gen_eta_squared = gen_eta_squared
  ))
}


#' Create a GT table for ANOVA results
#'
#' @param results Data frame from run_anova_analysis (multiple rows)
#' @param title Character string for table title
#' @param include_test Logical, whether to include Test column (default: TRUE)
#' @return gt table object
create_anova_table <- function(results, title, include_test = TRUE) {
  
  # Add FDR correction
  results$p_FDR <- p.adjust(results$p_value, method = "BH")
  
  # Start building table

  gt_table <- results %>%
    gt::gt() %>%
    gt::tab_header(title = title)
  
  # Conditional column labels based on whether Test column is included
  if (include_test && "Test" %in% names(results)) {
    gt_table <- gt_table %>%
      gt::cols_label(
        Subscale = "Subscale",
        Test = "Test",
        F_stat = gt::md("Estimate (*F*)"),
        df1 = "df1",
        df2 = "df2",
        p_value = gt::md("*p*"),
        p_FDR = gt::md("FDR *p*"),
        gen_eta_squared = "Generalized eta"
      )
  } else {
    gt_table <- gt_table %>%
      gt::cols_hide(columns = any_of("Test")) %>%
      gt::cols_label(
        Subscale = "Subscale",
        F_stat = gt::md("Estimate (*F*)"),
        df1 = "df1",
        df2 = "df2",
        p_value = gt::md("*p*"),
        p_FDR = gt::md("FDR *p*"),
        gen_eta_squared = "Generalized eta"
      )
  }
  
  # Apply formatting
  gt_table <- gt_table %>%
    gt::fmt_number(columns = gen_eta_squared, decimals = 3) %>%
    gt::fmt(
      columns = c(p_value, p_FDR),
      fns = function(x) ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = p_FDR < 0.05)
    ) %>%
    gt::tab_options(
      table.font.size = 10,
      data_row.padding = gt::px(4),
      table.font.names = "Times New Roman",
      heading.title.font.size = gt::px(14)
    )
  
  return(gt_table)
}
