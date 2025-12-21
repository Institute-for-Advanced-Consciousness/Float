# ============================================
# utils/barplot_functions.R
# Functions for creating bar plot
# ============================================

# ============================================
# Data prep
# ============================================

prepare_OB_summary <- function(only_complete_data) {
  
  only_complete_data %>%
    select(
      group_num,
      alt_postfloat_6_dasc_5d_oceanic_boundlessness_score
    ) %>%
    group_by(group_num) %>%
    summarize(
      across(
        everything(),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          se   = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -group_num,
      names_to = c("subscale", "stat"),
      names_pattern = "^(.*)_([^_]*)$"
    ) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(
      group = factor(as.character(group_num),
                     levels = names(group_labels),
                     labels = unname(group_labels)),
      subscale_label = fived_labels[subscale] |> unname()
    )
}

# ============================================
# Games-Howell Test
# ============================================

run_games_howell_OB <- function(only_complete_data) {
  
  # Prepare data
  df <- only_complete_data %>%
    select(group_num, alt_postfloat_6_dasc_5d_oceanic_boundlessness_score) %>%
    filter(!is.na(alt_postfloat_6_dasc_5d_oceanic_boundlessness_score)) %>%
    mutate(group = factor(group_num, 
                          levels = c(1, 2, 3),
                          labels = c("Pool", "Pool Preferred", "Chair (Control)")))
  
  # Run Games-Howell using rstatix
  gh_results <- rstatix::games_howell_test(
    df, 
    alt_postfloat_6_dasc_5d_oceanic_boundlessness_score ~ group
  )
  
  return(gh_results)

}


# ============================================
# Plot
# ============================================

create_OB_barplot <- function(OB_summary, gh_results = NULL) {
  
  OB_filtered <- filter(OB_summary, subscale_label == "Oceanic Boundlessness")
  
  # BOTTOM to TOP comparison order
  comparisons <- list(
    c("Pool Preferred", "Pool"),
    c("Chair (Control)", "Pool Preferred"),
    c("Chair (Control)", "Pool")
  )
  
  # Build annotations from Games-Howell results if provided
  if (!is.null(gh_results)) {
    # Extract p-values in correct order
    get_p <- function(g1, g2) {
      row <- gh_results %>% 
        filter((group1 == g1 & group2 == g2) | (group1 == g2 & group2 == g1))
      if (nrow(row) > 0) return(row$p.adj[1])
      return(NA)
    }
    
    p_pool_pref_vs_pool <- get_p("Pool Preferred", "Pool")
    p_chair_vs_pref <- get_p("Chair (Control)", "Pool Preferred")
    p_chair_vs_pool <- get_p("Chair (Control)", "Pool")
    
    format_p <- function(p) {
      if (is.na(p)) return("n.s.")
      if (p < 0.001) return("p < 0.001")
      if (p >= 0.05) return("\nn.s.")
      return(paste0("p = ", sprintf("%.3f", p)))
    }
    
    annotations <- c(
      format_p(p_pool_pref_vs_pool),
      format_p(p_chair_vs_pref),
      format_p(p_chair_vs_pool)
    )
  } else {
    # Fallback hardcoded values
    annotations <- c("\nn.s.", "p < 0.001", "p = 0.010")
  }
  
  # Dynamic y-position spacing based on bar heights
  bar_max <- max(OB_filtered$mean + OB_filtered$se, na.rm = TRUE)
  spacing <- diff(range(OB_filtered$mean, na.rm = TRUE)) * 0.2
  y_pos <- bar_max + spacing * c(1, 2, 3)
  
  ggplot(OB_filtered, aes(x = group, y = mean, fill = group)) +
    geom_bar(stat = "identity", color = "black", width = 0.6) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.15) +
    labs(
      title = "Between-Group Differences in Oceanic Boundlessness",
      x = "Group",
      y = "% of Theoretical Scale Maximum"
    ) +
    scale_fill_manual(values = c(
      "Chair (Control)" = "#c05658",
      "Pool Preferred" = "#13659f",
      "Pool" = "#77b5e0"
    )) +
    scale_x_discrete(labels = c(
      "Pool Preferred" = "Pool-REST Preferred",
      "Chair (Control)" = "Chair (Control)",
      "Pool" = "Pool-REST"
    )) +
    theme_bw(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 12),
      plot.margin = margin(t = 10, r = 10, b = 20, l = 10),
      legend.position = "none"
    ) +
    geom_signif(
      comparisons = comparisons,
      annotations = annotations,
      y_position = y_pos,
      tip_length = 0.02,
      textsize = 4,
      vjust = -0.01
    ) +
    ylim(0, 58)
}
