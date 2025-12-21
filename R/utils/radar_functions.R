# ============================================
# utils/radar_functions.R
# Functions for creating radar plots
# ============================================


# ============================================
# Definitions of variables and labels
# ============================================

eleven_subscales <- c(
  "alt_postfloat_6_dasc_11d_unity_score",
  "alt_postfloat_6_dasc_11d_spiritual_score",
  "alt_postfloat_6_dasc_11d_bliss_score",
  "alt_postfloat_6_dasc_11d_insight_score",
  "alt_postfloat_6_dasc_11d_disembodiment_score",
  "alt_postfloat_6_dasc_11d_impaired_score",
  "alt_postfloat_6_dasc_11d_anxiety_score",
  "alt_postfloat_6_dasc_11d_ele_imagery_score",
  "alt_postfloat_6_dasc_11d_com_imagery_score",
  "alt_postfloat_6_dasc_11d_percepts_score",
  "alt_postfloat_6_dasc_11d_synesthesiae_score"
)

group_labels <- c("1" = "Pool", "2" = "Pool Preferred", "3" = "Chair (Control)")

subscale_labels <- c(
  "alt_postfloat_6_dasc_11d_unity_score" = "Experience of Unity",
  "alt_postfloat_6_dasc_11d_spiritual_score" = "Spiritual Experience",
  "alt_postfloat_6_dasc_11d_bliss_score" = "Blissful State",
  "alt_postfloat_6_dasc_11d_insight_score" = "Insightfulness",
  "alt_postfloat_6_dasc_11d_disembodiment_score" = "Disembodiment",
  "alt_postfloat_6_dasc_11d_impaired_score" = "Impaired Control and Cognition",
  "alt_postfloat_6_dasc_11d_anxiety_score" = "Anxiety",
  "alt_postfloat_6_dasc_11d_ele_imagery_score" = "Elementary Imagery",
  "alt_postfloat_6_dasc_11d_com_imagery_score" = "Complex Imagery",
  "alt_postfloat_6_dasc_11d_percepts_score" = "Changed Meaning \n of Percepts",
  "alt_postfloat_6_dasc_11d_synesthesiae_score" = "Audio-Visual Synesthesiae"
)

fived_subscales <- c(
  "alt_postfloat_6_dasc_5d_oceanic_boundlessness_score",
  "alt_postfloat_6_dasc_5d_anxious_ego_dissolution_score"
)

fived_labels <- c(
  "alt_postfloat_6_dasc_5d_oceanic_boundlessness_score" = "Oceanic Boundlessness",
  "alt_postfloat_6_dasc_5d_anxious_ego_dissolution_score" = "Anxiety of Ego Dissolution"
)

# ============================================
# Data from Studerus et al., 2010
# ============================================

get_psilo_ket_points <- function() {
  tibble::tibble(
    subscale = c(
      "alt_postfloat_6_dasc_11d_unity_score",
      "alt_postfloat_6_dasc_11d_spiritual_score",
      "alt_postfloat_6_dasc_11d_bliss_score",
      "alt_postfloat_6_dasc_11d_insight_score",
      "alt_postfloat_6_dasc_11d_disembodiment_score",
      "alt_postfloat_6_dasc_11d_impaired_score",
      "alt_postfloat_6_dasc_11d_anxiety_score",
      "alt_postfloat_6_dasc_11d_ele_imagery_score",
      "alt_postfloat_6_dasc_11d_com_imagery_score",
      "alt_postfloat_6_dasc_11d_percepts_score",
      "alt_postfloat_6_dasc_11d_synesthesiae_score"
    ),
    psilocybin_Score = c(31.4, 18.5, 36.6, 25.3, 27.1, 23.5, 8.9, 50.0, 43.3, 37.3, 36.2),
    psilocybin_se = c(1.65, 1.3, 1.6, 1.35, 1.6, 1.15, 1.05, 1.85, 1.85, 1.8, 1.9),
    ketamine_Score = c(37.7, 19.8, 27.2, 23.4, 48.6, 32.6, 13.3, 32.7, 35.5, 32, 27.2),
    ketamine_se = c(2.4, 1.85, 1.1, 1.9, 2.5, 2.0, 1.5, 2.35, 2.3, 2.1, 2.6)
  ) %>%
    mutate(subscale = factor(subscale, levels = eleven_subscales)) %>%
    pivot_longer(
      -subscale,
      names_to = c("Drug", ".value"),
      names_sep = "_"
    ) %>%
    mutate(
      Drug = tools::toTitleCase(Drug),
      subscale = factor(subscale, levels = eleven_subscales)
    )
}

# ============================================
# Data prep for radar (normalization, labeling, etc.)
# ============================================

prepare_asc_summary <- function(only_complete_data) {
  only_complete_data %>%
    group_by(group_num) %>%
    summarize(across(all_of(eleven_subscales),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))))) %>%
    pivot_longer(cols = -group_num,
                 names_to = c("subscale", ".value"),
                 names_pattern = "(.*)_(mean|se)") %>%
    mutate(group_num = factor(group_num, levels = names(group_labels), labels = group_labels)) %>%
    filter(!is.na(group_num))
}

prepare_radar_data <- function(ASC_code_summary, psilo_ket_points) {
  # asc for radar
  asc_radar <- ASC_code_summary %>%
    select(group_num, subscale, mean) %>%
    pivot_wider(names_from = subscale, values_from = mean) %>%
    rename(Group = group_num)
  
  # psilo/ket for radar
  pk_radar <- psilo_ket_points %>%
    select(subscale, Drug, Score) %>%
    pivot_wider(names_from = subscale, values_from = Score) %>%
    rename(Group = Drug)
  
  radar_data <- bind_rows(asc_radar, pk_radar)
  
  # Normalize to 0 to 1 scale
  radar_data_norm <- radar_data %>%
    mutate(across(where(is.numeric), ~ .x / 100)) %>%
    mutate(Group = as.character(Group))
  
  # Order
  radar_data_norm$Group <- factor(
    radar_data_norm$Group,
    levels = c("Pool Preferred", "Pool", "Chair (Control)", "Ketamine", "Psilocybin"),
    labels = c("Pool-REST Preferred", "Pool-REST", "Chair-REST (Control)", "Ketamine", "Psilocybin")
  )
  
  names(radar_data_norm) <- c("Group", subscale_labels[names(radar_data_norm)[-1]])
  
  return(radar_data_norm)
}

# ============================================
# Two plot functions:  and Dose-Response
# ============================================

create_simple_radar <- function(radar_data_norm) {
  
  # Color friendly
  custom_colors <- c(
    "Pool-REST Preferred" = "#13659f",
    "Pool-REST"           = "#77b5e0",
    "Chair-REST (Control)" = "#c05658",
    "Ketamine"            = "#DE8F05",
    "Psilocybin"          = "black"
  )
  
  # Legend order
  legend_order <- c(
    "Pool-REST Preferred",
    "Pool-REST",
    "Chair-REST (Control)",
    "Ketamine",
    "Psilocybin"
  )
  
  # Draw order (FIRST drawn = bottom). Psilocybin first => bottom.
  draw_order <- c(
    "Psilocybin",
    "Ketamine",
    "Chair-REST (Control)",
    "Pool-REST",
    "Pool-REST Preferred"
  )
  
  # Force DRAW order via factor levels
  radar_data_norm <- radar_data_norm %>%
    mutate(Group = factor(as.character(Group), levels = draw_order))
  
  radar_plot <- ggradar(
    radar_data_norm,
    group.colours = custom_colors,
    grid.min = 0, grid.mid = 0.25, grid.max = 0.5,
    values.radar = c("0%", "25%", "50%"),
    grid.label.size = 4,
    group.line.width = 1.5,
    group.point.size = 1.5,
    legend.position = "right",
    legend.text.size = 10,
    legend.title = "Intervention",
    axis.label.size = 2.25,
    background.circle.colour = "white",
    gridline.mid.colour = "grey"
  ) +
    scale_colour_manual(values = custom_colors, breaks = legend_order) +
    scale_fill_manual(values = custom_colors, breaks = legend_order) +
    theme(
      plot.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      plot.margin = margin(t = 10, r = 0, b = 10, l = 20)
    )
  
  return(radar_plot)
}

# ============================================
# Dose-response radar
# ============================================

prepare_dose_radar_data <- function(dose_response_points) {
  
  # Build the base radar dataset
  dose_radar <- dose_response_points %>%
    select(group, subscale, score) %>%
    pivot_wider(names_from = subscale, values_from = score) %>%
    rename(Group = group)
  
  # Normalize numeric columns to 0-1 scale
  dose_radar_norm <- dose_radar %>%
    mutate(across(where(is.numeric), ~ .x / 100)) %>%
    mutate(Group = as.character(Group))
  
  # Fix text encoding problems and rebuild Group names cleanly
  dose_radar_norm <- dose_radar_norm %>%
    mutate(
      Group = str_replace_all(Group, "_g/kg", "μg/kg"),
      Group = str_replace_all(Group, "ug/kg", "μg/kg"),
      Group = str_replace_all(Group, "mcg/kg", "μg/kg"),
      Group = str_trim(Group)
    ) %>%
    mutate(
      Group = case_when(
        Group %in% c("Pool Preferred", "Pool-REST Preferred") ~ "Pool-REST Preferred",
        Group %in% c("Pool", "Pool-REST") ~ "Pool-REST",
        Group %in% c("Chair (Control)", "Chair-REST (Control)") ~ "Chair-REST (Control)",
        str_detect(Group, "100") ~ "Psilocybin 100 μg/kg",
        str_detect(Group, "200") ~ "Psilocybin 200 μg/kg",
        str_detect(Group, "300") ~ "Psilocybin 300 μg/kg",
        str_detect(Group, "400") ~ "Psilocybin 400 μg/kg",
        TRUE ~ Group
      )
    )
  
  # Force the proper factor order
  dose_radar_reordered <- dose_radar_norm %>%
    mutate(
      Group = factor(
        Group,
        levels = c(
          "Pool-REST Preferred", "Pool-REST", "Chair-REST (Control)",
          "Psilocybin 100 μg/kg", "Psilocybin 200 μg/kg",
          "Psilocybin 300 μg/kg", "Psilocybin 400 μg/kg"
        )
      )
    )
  
  names(dose_radar_reordered) <- c("Group", subscale_labels[names(dose_radar_reordered)[-1]])
  
  return(dose_radar_reordered)
}

create_dose_radar <- function(dose_radar_reordered) {
  
  # Color palette
  psilo_blues <- c("#CCCCCC", "#aaaaaa", "#666666", "black")
  
  radar_colors <- c(
    "Pool-REST Preferred"   = "#13659f",
    "Pool-REST"             = "#77b5e0",
    "Chair-REST (Control)"  = "#c05658",
    "Psilocybin 100 μg/kg"  = psilo_blues[1],
    "Psilocybin 200 μg/kg"  = psilo_blues[2],
    "Psilocybin 300 μg/kg"  = psilo_blues[3],
    "Psilocybin 400 μg/kg"  = psilo_blues[4]
  )
  
  dose_legend_order <- c(
    "Pool-REST Preferred",
    "Pool-REST",
    "Chair-REST (Control)",
    "Psilocybin 100 μg/kg",
    "Psilocybin 200 μg/kg",
    "Psilocybin 300 μg/kg",
    "Psilocybin 400 μg/kg"
  )
  
  dose_draw_order <- c(
    "Psilocybin 400 μg/kg",
    "Psilocybin 300 μg/kg",
    "Psilocybin 200 μg/kg",
    "Psilocybin 100 μg/kg",
    "Chair-REST (Control)",
    "Pool-REST",
    "Pool-REST Preferred"
  )
  
  # Force DRAW order via factor levels
  dose_radar_reordered <- dose_radar_reordered %>%
    mutate(Group = factor(as.character(Group), levels = dose_draw_order))
  
  dose_radar_plot <- ggradar(
    dose_radar_reordered,
    group.colours = radar_colors,
    grid.min = 0, grid.mid = 0.5, grid.max = 0.75,
    values.radar = c("0%", "50%", "75%"),
    grid.label.size = 4,
    group.line.width = 1.5,
    group.point.size = 1.5,
    legend.position = "right",
    legend.text.size = 10,
    legend.title = "Intervention",
    axis.label.size = 2.25,
    background.circle.colour = "white",
    gridline.mid.colour = "grey"
  ) +
    scale_colour_manual(values = radar_colors, breaks = dose_legend_order) +
    scale_fill_manual(values = radar_colors, breaks = dose_legend_order) +
    theme(
      plot.title = element_text(size = 12),
      legend.title = element_text(size = 12),
      plot.margin = margin(t = 10, r = 0, b = 10, l = 20)
    )
  
  return(dose_radar_plot)
}