# ============================================
# 04_ttests_prepost.R
# Pre-post paired comparisons
# ============================================

source("R/00_setup.R")
source("R/utils/table_functions.R")

# Load cleaned data
dat <- readRDS("data/cleaned_data.rds")
only_complete_data <- dat$only_complete
prepost_pairs <- dat$prepost_pairs

# ============================================
# Pre-post analysis function
# ============================================

#' Run pre-post paired comparisons with automatic test selection
#'
#' Checks normality of difference scores and selects paired t-test
#' or Wilcoxon signed-rank test accordingly.
#'
#' @param data Data frame
#' @param pairs Named list of c(pre_col, post_col) pairs
#' @param alpha_norm Alpha level for normality test (default: 0.05)
#' @param exact_wilcox Whether to use exact Wilcoxon test (default: FALSE)
#' @return Data frame with test results
run_prepost_auto <- function(data, pairs, alpha_norm = 0.05, exact_wilcox = FALSE) {
  
  purrr::imap_dfr(pairs, function(cols, label) {
    pre  <- cols[1]
    post <- cols[2]
    
    d <- data %>%
      dplyr::select(all_of(c(pre, post))) %>%
      dplyr::filter(!is.na(.data[[pre]]), !is.na(.data[[post]]))
    
    x_pre  <- d[[pre]]
    x_post <- d[[post]]
    diff   <- x_post - x_pre
    n      <- length(diff)
    
    # Shapiro on differences (only when feasible)
    shapiro_p <- NA_real_
    norm_ok <- FALSE
    if (n >= 3 && n <= 5000 && length(unique(diff[!is.na(diff)])) > 2) {
      sh <- tryCatch(shapiro.test(diff), error = function(e) NULL)
      if (!is.null(sh)) {
        shapiro_p <- sh$p.value
        norm_ok <- is.finite(shapiro_p) && shapiro_p >= alpha_norm
      }
    }
    
    # Paired t-test
    tt <- tryCatch(t.test(x_post, x_pre, paired = TRUE), error = function(e) NULL)
    tt_out <- if (!is.null(tt)) broom::tidy(tt) else tibble::tibble(
      statistic = NA_real_, parameter = NA_real_, p.value = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_
    )
    
    # Wilcoxon signed-rank
    wt <- tryCatch(
      suppressWarnings(wilcox.test(x_post, x_pre, paired = TRUE, exact = exact_wilcox)),
      error = function(e) NULL
    )
    V  <- if (!is.null(wt)) unname(wt$statistic) else NA_real_
    pW <- if (!is.null(wt)) unname(wt$p.value) else NA_real_
    
    chosen_test <- if (norm_ok && !is.na(tt_out$p.value)) "t_test" else "wilcoxon"
    p_chosen    <- if (chosen_test == "t_test") tt_out$p.value else pW
    
    tibble::tibble(
      measure   = label,
      n         = n,
      mean_pre  = mean(x_pre),
      mean_post = mean(x_post),
      mean_diff = mean(diff),
      shapiro_p = shapiro_p,
      chosen_test = chosen_test,
      p = p_chosen,
      t  = unname(tt_out$statistic),
      df = unname(tt_out$parameter),
      ci_low  = tt_out$conf.low,
      ci_high = tt_out$conf.high,
      V = V
    )
  })
}

# ============================================
# Run pre-post analyses
# ============================================

cat("Running pre-post paired comparisons...\n")

auto_results <- run_prepost_auto(only_complete_data, prepost_pairs, alpha_norm = 0.05)

# ============================================
# Create and save table
# ============================================

pre_post_table <- create_prepost_table(auto_results)

gt::gtsave(pre_post_table, "output/tables/pre_post_table.png")

# Save results for reference
saveRDS(auto_results, "output/prepost_results.rds")

cat("\nPre-post comparisons complete.\n")
cat("  Table saved to output/tables/pre_post_table.png\n")
