# ============================================
# utils/table_functions.R
# Shared functions for creating GT tables
# ============================================

# Suppress R CMD check notes about dplyr/tidyverse NSE
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Model", "Mediator", "Outcome", "Outcome_Raw", "Mediator_Display",
    "A_Path_B", "A_Path_B_Std", "A_Path_p", "A_Path_p_formatted", "A_Path_CI",
    "B_Path_B", "B_Path_B_Std", "B_Path_p", "B_Path_p_formatted", "B_Path_CI",
    "Direct_B", "Direct_B_Std", "Direct_p", "Direct_p_formatted", "Direct_CI",
    "Indirect_B", "Indirect_B_Std", "Indirect_p", "Indirect_p_formatted", "Indirect_CI",
    "Total_B", "Total_B_Std", "Total_p", "Total_p_formatted", "Total_CI",
    "Prop_Mediated", "R_squared", "FDR_p",
    "measure", "chosen_test", "p", "n", "mean_pre", "mean_post", "mean_diff"
  ))
}

#' Create simple mediation summary table
#'
#' @param simple_summary Data frame from create_simple_summary() with FDR_p
#' @return gt table object
create_simple_mediation_table <- function(simple_summary) {

  `%>%` <- magrittr::`%>%`

  enhanced_summary_clean <- simple_summary %>%
    dplyr::mutate(
      Mediator = dplyr::case_when(
        grepl("oceanic_boundlessness", Model) ~ "Oceanic Boundlessness (5D-ASC)",
        grepl("anxious_ego", Model) ~ "Anxious Ego Dissolution (5D-ASC)",
        grepl("maias_bodylisten", Model) ~ "Body Listening (MAIA-S)",
        grepl("maias_emoaware", Model) ~ "Emotional Awareness (MAIA-S)",
        grepl("maias_selfreg", Model) ~ "Self-Regulation (MAIA-S)",
        grepl("breath_intensity_mean", Model) ~ "Breath Intensity",
        grepl("breath_pleasant_mean", Model) ~ "Breath Pleasantness",
        grepl("heart_intensity_mean", Model) ~ "Heart Intensity",
        grepl("heart_pleasant_mean", Model) ~ "Heart Pleasantness",
        grepl("gi_intensity_mean", Model) ~ "GI Intensity",
        grepl("gi_pleasant_mean", Model) ~ "GI Pleasantness",
        TRUE ~ "Other"
      ),
      Outcome = dplyr::case_when(
        grepl("posaffect", Model) ~ "Positive Affect (PANAS)",
        grepl("negaffect", Model) ~ "Negative Affect (PANAS)",
        grepl("stais", Model) ~ "State Anxiety (STAI-S)",
        grepl("stait", Model) ~ "Trait Anxiety (STAI-T)",
        grepl("asi", Model) ~ "Anxiety Sensitivity (ASI-3R)",
        grepl("hama", Model) ~ "General Anxiety (HAM-A)",
        grepl("madrs", Model) ~ "Depression (MADRS)",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::arrange(Mediator) %>%
    dplyr::mutate(
      Mediator_Display = ifelse(Mediator != dplyr::lag(Mediator, default = ""), Mediator, "")
    ) %>%
    dplyr::mutate(
      A_Path_p_formatted = ifelse(A_Path_p < 0.001, "< 0.001", sprintf("%.3f", A_Path_p)),
      B_Path_p_formatted = ifelse(B_Path_p < 0.001, "< 0.001", sprintf("%.3f", B_Path_p)),
      Direct_p_formatted = ifelse(Direct_p < 0.001, "< 0.001", sprintf("%.3f", Direct_p)),
      Indirect_p_formatted = ifelse(Indirect_p < 0.001, "< 0.001", sprintf("%.3f", Indirect_p)),
      Total_p_formatted = ifelse(Total_p < 0.001, "< 0.001", sprintf("%.3f", Total_p))
    )
  
  gt_table <- enhanced_summary_clean %>%
    dplyr::select(Mediator_Display, Outcome,
           A_Path_B, A_Path_B_Std, A_Path_p_formatted, A_Path_CI,
           B_Path_B, B_Path_B_Std, B_Path_p_formatted, B_Path_CI,
           Direct_B, Direct_B_Std, Direct_p_formatted, Direct_CI,
           Indirect_B, Indirect_B_Std, Indirect_p_formatted, Indirect_CI,
           Total_B, Total_B_Std, Total_p_formatted, Total_CI,
           Prop_Mediated, R_squared, FDR_p) %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Summary of Simple Mediation Models**"),
      subtitle = gt::md("Path Coefficients (Unstandardized and Standardized) with Confidence Intervals (CIs) and Model Statistics")
    ) %>%
    gt::tab_spanner(label = gt::md("IV to Mediator (*a*)"), columns = c(A_Path_B, A_Path_B_Std, A_Path_p_formatted, A_Path_CI)) %>%
    gt::tab_spanner(label = gt::md("Mediator to DV (*b*)"), columns = c(B_Path_B, B_Path_B_Std, B_Path_p_formatted, B_Path_CI)) %>%
    gt::tab_spanner(label = gt::md("Direct Effect (*c'*)"), columns = c(Direct_B, Direct_B_Std, Direct_p_formatted, Direct_CI)) %>%
    gt::tab_spanner(label = gt::md("Indirect Effect (*ab*)"), columns = c(Indirect_B, Indirect_B_Std, Indirect_p_formatted, Indirect_CI)) %>%
    gt::tab_spanner(label = gt::md("Total Effect (*c*)"), columns = c(Total_B, Total_B_Std, Total_p_formatted, Total_CI)) %>%
    gt::tab_spanner(label = "Model Statistics", columns = c(Prop_Mediated, R_squared)) %>%
    gt::cols_label(
      Mediator_Display = "Mediator", Outcome = "Outcome",
      A_Path_B = "Estimate", A_Path_B_Std = gt::md("*β*"), A_Path_p_formatted = gt::md("*p*"), A_Path_CI = "95% CI",
      B_Path_B = "Estimate", B_Path_B_Std = gt::md("*β*"), B_Path_p_formatted = gt::md("*p*"), B_Path_CI = "95% CI",
      Direct_B = "Estimate", Direct_B_Std = gt::md("*β*"), Direct_p_formatted = gt::md("*p*"), Direct_CI = "95% CI",
      Indirect_B = "Estimate", Indirect_B_Std = gt::md("*β*"), Indirect_p_formatted = gt::md("*p*"), Indirect_CI = "95% CI",
      Total_B = "Estimate", Total_B_Std = gt::md("*β*"), Total_p_formatted = gt::md("*p*"), Total_CI = "95% CI",
      Prop_Mediated = "Proportion Mediated", R_squared = "R²", FDR_p = gt::md("FDR *p*")
    ) %>%
    gt::fmt_number(columns = c(Prop_Mediated, R_squared), decimals = 3) %>%
    gt::tab_options(table.font.size = 8, data_row.padding = gt::px(2)) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold")), locations = gt::cells_body(rows = Indirect_p_formatted < 0.05)) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold", style = "italic")), locations = gt::cells_body(rows = !is.na(FDR_p) & FDR_p < 0.05)) %>%
    gt::tab_options(
      table.border.top.width = gt::px(2), column_labels.border.bottom.width = gt::px(1),
      table.border.bottom.width = gt::px(2), table.font.names = "Times New Roman",
      heading.title.font.size = gt::px(14), heading.subtitle.font.size = gt::px(10)
    )
  
  return(gt_table)
}


#' Create OB follow-up mediation table (full version with all paths)
#'
#' @param post_hoc_summary Data frame from create_simple_summary()
#' @return gt table object
create_ob_followup_table <- function(post_hoc_summary) {

  `%>%` <- magrittr::`%>%`

  enhanced_summary_clean <- post_hoc_summary %>%
    dplyr::mutate(
      Mediator = dplyr::case_when(
        grepl("unity", Model, ignore.case = TRUE) ~ "Unity (11D-ASC)",
        grepl("spiritual", Model, ignore.case = TRUE) ~ "Spiritual (11D-ASC)",
        grepl("bliss", Model, ignore.case = TRUE) ~ "Bliss (11D-ASC)",
        grepl("disembodiment", Model, ignore.case = TRUE) ~ "Disembodiment (11D-ASC)",
        grepl("insight", Model, ignore.case = TRUE) ~ "Insight (11D-ASC)",
        TRUE ~ "Other"
      ),
      Outcome = dplyr::case_when(
        grepl("posaffect", Model) ~ "Positive Affect (PANAS-X)",
        grepl("negaffect", Model) ~ "Negative Affect (PANAS-X)",
        grepl("stais", Model) ~ "State Anxiety (STAI-S)",
        grepl("asi", Model) ~ "Anxiety Sensitivity (ASI-3R)",
        grepl("hama", Model) ~ "General Anxiety (HAM-A)",
        grepl("madrs", Model) ~ "Depression (MADRS)",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::arrange(Mediator) %>%
    dplyr::mutate(
      Mediator_Display = ifelse(Mediator != dplyr::lag(Mediator, default = ""), Mediator, "")
    ) %>%
    dplyr::mutate(
      A_Path_p_formatted = ifelse(A_Path_p < 0.001, "< 0.001", sprintf("%.3f", A_Path_p)),
      B_Path_p_formatted = ifelse(B_Path_p < 0.001, "< 0.001", sprintf("%.3f", B_Path_p)),
      Direct_p_formatted = ifelse(Direct_p < 0.001, "< 0.001", sprintf("%.3f", Direct_p)),
      Indirect_p_formatted = ifelse(Indirect_p < 0.001, "< 0.001", sprintf("%.3f", Indirect_p)),
      Total_p_formatted = ifelse(Total_p < 0.001, "< 0.001", sprintf("%.3f", Total_p))
    )
  
  gt_table <- enhanced_summary_clean %>%
    dplyr::select(Mediator_Display, Outcome, 
           A_Path_B, A_Path_B_Std, A_Path_p_formatted, A_Path_CI,
           B_Path_B, B_Path_B_Std, B_Path_p_formatted, B_Path_CI,
           Direct_B, Direct_B_Std, Direct_p_formatted, Direct_CI,
           Indirect_B, Indirect_B_Std, Indirect_p_formatted, Indirect_CI,
           Total_B, Total_B_Std, Total_p_formatted, Total_CI,
           Prop_Mediated, R_squared, FDR_p) %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Summary of Oceanic Boundlessness Mediation Models**"),
      subtitle = gt::md("Path Coefficients (Unstandardized and Standardized) with Confidence Intervals (CIs) and Model Statistics")
    ) %>%
    gt::tab_spanner(label = gt::md("IV to Mediator (*a*)"), columns = c(A_Path_B, A_Path_B_Std, A_Path_p_formatted, A_Path_CI)) %>%
    gt::tab_spanner(label = gt::md("Mediator to DV (*b*)"), columns = c(B_Path_B, B_Path_B_Std, B_Path_p_formatted, B_Path_CI)) %>%
    gt::tab_spanner(label = gt::md("Direct Effect (*c'*)"), columns = c(Direct_B, Direct_B_Std, Direct_p_formatted, Direct_CI)) %>%
    gt::tab_spanner(label = gt::md("Indirect Effect (*ab*)"), columns = c(Indirect_B, Indirect_B_Std, Indirect_p_formatted, Indirect_CI)) %>%
    gt::tab_spanner(label = gt::md("Total Effect (*c*)"), columns = c(Total_B, Total_B_Std, Total_p_formatted, Total_CI)) %>%
    gt::tab_spanner(label = "Model Statistics", columns = c(Prop_Mediated, R_squared)) %>%
    gt::cols_label(
      Mediator_Display = "Mediator", Outcome = "Outcome",
      A_Path_B = "Estimate", A_Path_B_Std = gt::md("*β*"), A_Path_p_formatted = gt::md("*p*"), A_Path_CI = "95% CI",
      B_Path_B = "Estimate", B_Path_B_Std = gt::md("*β*"), B_Path_p_formatted = gt::md("*p*"), B_Path_CI = "95% CI",
      Direct_B = "Estimate", Direct_B_Std = gt::md("*β*"), Direct_p_formatted = gt::md("*p*"), Direct_CI = "95% CI",
      Indirect_B = "Estimate", Indirect_B_Std = gt::md("*β*"), Indirect_p_formatted = gt::md("*p*"), Indirect_CI = "95% CI",
      Total_B = "Estimate", Total_B_Std = gt::md("*β*"), Total_p_formatted = gt::md("*p*"), Total_CI = "95% CI",
      Prop_Mediated = "Proportion Mediated", R_squared = "R²", FDR_p = gt::md("FDR *p*")
    ) %>%
    gt::fmt_number(columns = c(Prop_Mediated, R_squared), decimals = 3) %>%
    gt::tab_options(table.font.size = 8, data_row.padding = gt::px(2)) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold")), locations = gt::cells_body(rows = Indirect_p_formatted < 0.05)) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold", style = "italic")), locations = gt::cells_body(rows = !is.na(FDR_p) & FDR_p < 0.05)) %>%
    gt::tab_options(
      table.border.top.width = gt::px(2), column_labels.border.bottom.width = gt::px(1), 
      table.border.bottom.width = gt::px(2), table.font.names = "Times New Roman",
      heading.title.font.size = gt::px(14), heading.subtitle.font.size = gt::px(10)
    )
  
  return(gt_table)
}


#' Create phenomenology follow-up mediation table
#'
#' @param post_hoc_phenom Data frame from create_simple_summary()
#' @return gt table object
create_phenom_followup_table <- function(post_hoc_phenom) {

  `%>%` <- magrittr::`%>%`

  enhanced_summary_clean <- post_hoc_phenom %>%
    dplyr::mutate(
      Mediator = dplyr::case_when(
        grepl("oceanic", Model, ignore.case = TRUE) ~ "Oceanic Boundlessness (5D-ASC)",
        grepl("anxious", Model, ignore.case = TRUE) ~ "Anxious Ego Dissolution (5D-ASC)",
        TRUE ~ "Other"
      ),
      Outcome_Raw = sub(".*->\\s*", "", Model),
      Outcome = dplyr::case_when(
        Outcome_Raw == "mean_pos_effects" ~ "Average Positive Side Effects",
        Outcome_Raw == "average_sec_30" ~ "Creativity",
        Outcome_Raw == "average_sec_31" ~ "Racing Thoughts",
        Outcome_Raw == "average_sec_32" ~ "Joy/Happiness",
        Outcome_Raw == "average_sec_33" ~ "Increased Energy",
        Outcome_Raw == "average_sec_34" ~ "Increased Focus",
        Outcome_Raw == "average_sec_35" ~ "Peacefulness",
        Outcome_Raw == "average_sec_36" ~ "Increased Sexual Desire",
        Outcome_Raw == "average_sec_37" ~ "Compassion for Others",
        Outcome_Raw == "average_sec_38" ~ "Appreciation for Life",
        Outcome_Raw == "average_sec_39" ~ "Refreshed",
        Outcome_Raw == "average_sec_40" ~ "Relaxed",
        Outcome_Raw == "average_sec_41" ~ "Silent Mind",
        Outcome_Raw == "average_sec_42" ~ "Pain Free Existence",
        Outcome_Raw == "average_sec_43" ~ "Feeling of Flow",
        Outcome_Raw == "average_sec_44" ~ "Other Positive",
        TRUE ~ Outcome_Raw
      )
    ) %>%
    dplyr::arrange(Mediator) %>%
    dplyr::mutate(
      Mediator_Display = ifelse(Mediator != dplyr::lag(Mediator, default = ""), Mediator, "")
    ) %>%
    dplyr::mutate(
      A_Path_p_formatted = ifelse(A_Path_p < 0.001, "< 0.001", sprintf("%.3f", A_Path_p)),
      B_Path_p_formatted = ifelse(B_Path_p < 0.001, "< 0.001", sprintf("%.3f", B_Path_p)),
      Direct_p_formatted = ifelse(Direct_p < 0.001, "< 0.001", sprintf("%.3f", Direct_p)),
      Indirect_p_formatted = ifelse(Indirect_p < 0.001, "< 0.001", sprintf("%.3f", Indirect_p)),
      Total_p_formatted = ifelse(Total_p < 0.001, "< 0.001", sprintf("%.3f", Total_p))
    )
  
  gt_table <- enhanced_summary_clean %>%
    dplyr::select(Mediator_Display, Outcome, 
           A_Path_B, A_Path_B_Std, A_Path_p_formatted, A_Path_CI,
           B_Path_B, B_Path_B_Std, B_Path_p_formatted, B_Path_CI,
           Direct_B, Direct_B_Std, Direct_p_formatted, Direct_CI,
           Indirect_B, Indirect_B_Std, Indirect_p_formatted, Indirect_CI,
           Total_B, Total_B_Std, Total_p_formatted, Total_CI,
           Prop_Mediated, R_squared, FDR_p) %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Summary of Oceanic Boundlessness Mediation Models on Positive Side Effects**"),
      subtitle = gt::md("Path Coefficients (Unstandardized and Standardized) with Confidence Intervals (CIs) and Model Statistics")
    ) %>%
    gt::tab_spanner(label = gt::md("IV to Mediator (*a*)"), columns = c(A_Path_B, A_Path_B_Std, A_Path_p_formatted, A_Path_CI)) %>%
    gt::tab_spanner(label = gt::md("Mediator to DV (*b*)"), columns = c(B_Path_B, B_Path_B_Std, B_Path_p_formatted, B_Path_CI)) %>%
    gt::tab_spanner(label = gt::md("Direct Effect (*c'*)"), columns = c(Direct_B, Direct_B_Std, Direct_p_formatted, Direct_CI)) %>%
    gt::tab_spanner(label = gt::md("Indirect Effect (*ab*)"), columns = c(Indirect_B, Indirect_B_Std, Indirect_p_formatted, Indirect_CI)) %>%
    gt::tab_spanner(label = gt::md("Total Effect (*c*)"), columns = c(Total_B, Total_B_Std, Total_p_formatted, Total_CI)) %>%
    gt::tab_spanner(label = "Model Statistics", columns = c(Prop_Mediated, R_squared)) %>%
    gt::cols_label(
      Mediator_Display = "Mediator", Outcome = "Outcome",
      A_Path_B = "Estimate", A_Path_B_Std = gt::md("*β*"), A_Path_p_formatted = gt::md("*p*"), A_Path_CI = "95% CI",
      B_Path_B = "Estimate", B_Path_B_Std = gt::md("*β*"), B_Path_p_formatted = gt::md("*p*"), B_Path_CI = "95% CI",
      Direct_B = "Estimate", Direct_B_Std = gt::md("*β*"), Direct_p_formatted = gt::md("*p*"), Direct_CI = "95% CI",
      Indirect_B = "Estimate", Indirect_B_Std = gt::md("*β*"), Indirect_p_formatted = gt::md("*p*"), Indirect_CI = "95% CI",
      Total_B = "Estimate", Total_B_Std = gt::md("*β*"), Total_p_formatted = gt::md("*p*"), Total_CI = "95% CI",
      Prop_Mediated = "Proportion Mediated", R_squared = "R²", FDR_p = gt::md("Bonferroni *p*")
    ) %>%
    gt::fmt_number(columns = c(Prop_Mediated, R_squared), decimals = 3) %>%
    gt::tab_options(table.font.size = 8, data_row.padding = gt::px(2)) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold")), locations = gt::cells_body(rows = Indirect_p_formatted < 0.05)) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold", style = "italic")), locations = gt::cells_body(rows = !is.na(FDR_p) & FDR_p < 0.05)) %>%
    gt::tab_options(
      table.border.top.width = gt::px(2), column_labels.border.bottom.width = gt::px(1), 
      table.border.bottom.width = gt::px(2), table.font.names = "Times New Roman",
      heading.title.font.size = gt::px(14), heading.subtitle.font.size = gt::px(10)
    )
  
  # Hide FDR column if only 2 or fewer models
  if (nrow(enhanced_summary_clean) <= 2) {
    gt_table <- gt_table %>% gt::cols_hide(columns = FDR_p)
  }
  
  return(gt_table)
}


#' Create pre-post t-test summary table
#'
#' @param auto_results Data frame from run_prepost_auto()
#' @return gt table object
create_prepost_table <- function(auto_results) {

  `%>%` <- magrittr::`%>%`

  auto_table_like <- auto_results %>%
    dplyr::mutate(
      Subscale = measure,
      Test = dplyr::case_when(
        chosen_test == "t_test"   ~ "Paired t-test",
        chosen_test == "wilcoxon" ~ "Wilcoxon signed-rank",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(Subscale, Test, p, n, mean_pre, mean_post, mean_diff)
  
  pre_post_table <- auto_table_like %>%
    gt::gt() %>%
    gt::tab_header(title = gt::md("**Summary of Pre–Post Effects**")) %>%
    gt::cols_label(
      Subscale = "Subscale",
      Test = "Test",
      p = gt::md("*p*"),
      n = "N",
      mean_pre = "Pre (M)",
      mean_post = "Post (M)",
      mean_diff = "Mean Difference"
    ) %>%
    gt::fmt_number(columns = c(mean_pre, mean_post, mean_diff), decimals = 2) %>%
    gt::fmt(
      columns = p,
      fns = function(x) ifelse(is.na(x), NA_character_,
                               ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x)))
    ) %>%
    gt::tab_options(table.font.size = 10, table.font.names = "Times New Roman")
  
  return(pre_post_table)
}
