# ============================================
# 01_data_cleaning.R
# Import data, compute derived variables, define analysis parameters
# ============================================

source("R/00_setup.R")

# ============================================
# 1. IMPORT RAW DATA
# ============================================

raw_data <- read.csv("data/raw_data.csv")
only_complete_data <- read.csv("data/only_complete_data.csv")

# Subset to first 70 rows
raw_data <- raw_data[1:70, ]

# Convert group variables to factors
raw_data$binary_group_num <- as.factor(raw_data$binary_group_num)
raw_data$pooled_group_num <- as.factor(raw_data$pooled_group_num)

# ============================================
# 2. COMPUTE PANAS MEANS AND DIFFERENCE SCORES
# ============================================

affects <- c(
  "posaffect_score", "negaffect_score", "cheerful", "attentive", "sluggish", 
  "strong", "relaxed", "irritable", "delighted", "inspired", "calm", "afraid", 
  "tired", "happy", "alert", "upset", "active", "guilty", "joyful", "nervous", 
  "sleepy", "excited", "hostile", "proud", "jittery", "lively", "ashamed", 
  "atease", "scared", "drowsy", "enthusiastic", "distressed", "determined", 
  "interested", "hungry", "fatigue", "serenity_score", "joviality_score",
  "attentiveness_score1", "fatigue_score", "fear_score1"
)

for (a in affects) {
  pre_cols  <- grep(paste0("prefloat_[1-6]_panasx_.*", a, ".*"), names(raw_data), value = TRUE)
  post_cols <- grep(paste0("postfloat_[1-6]_panasx_.*", a, ".*"), names(raw_data), value = TRUE)
  
  if (length(pre_cols) > 0) {
    raw_data[[paste0("prefloat_mean_panasx_", a)]] <-
      rowMeans(raw_data[, pre_cols, drop = FALSE], na.rm = TRUE)
  }
  if (length(post_cols) > 0) {
    raw_data[[paste0("postfloat_mean_panasx_", a)]] <-
      rowMeans(raw_data[, post_cols, drop = FALSE], na.rm = TRUE)
  }
  if (length(pre_cols) > 0 && length(post_cols) > 0) {
    raw_data[[paste0("diff_panasx_", a)]] <-
      raw_data[[paste0("postfloat_mean_panasx_", a)]] -
      raw_data[[paste0("prefloat_mean_panasx_", a)]]
  }
}

# ============================================
# 3. COMPUTE STAI AND OTHER DIFFERENCE SCORES
# ============================================

raw_data <- raw_data %>%
  rowwise() %>%
  mutate(
    # STAI means
    prefloat_mean_stais = mean(c(prefloat_1_stais_state_score, 
                                 prefloat_2_stais_state_score,
                                 prefloat_3_stais_state_score, 
                                 prefloat_4_stais_state_score,
                                 prefloat_5_stais_state_score, 
                                 prefloat_6_stais_state_score), na.rm = TRUE),
    
    postfloat_mean_stais = mean(c(postfloat_1_stais_state_score, 
                                  postfloat_2_stais_state_score,
                                  postfloat_3_stais_state_score, 
                                  postfloat_4_stais_state_score,
                                  postfloat_5_stais_state_score, 
                                  postfloat_6_stais_state_score), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Difference scores
    diff_mean_stais = postfloat_mean_stais - prefloat_mean_stais,
    diff_asi3r_total = postintervention_asi3r_score - baseline_asi3r_score,
    
    # Intensity and Pleasantness means
    breath_intensity_mean = rowMeans(
      select(., matches("postfloat_\\d+_fspost_17")), na.rm = TRUE),
    heart_intensity_mean = rowMeans(
      select(., matches("postfloat_\\d+_fspost_18")), na.rm = TRUE),
    gi_intensity_mean = rowMeans(
      select(., matches("postfloat_\\d+_fspost_19")), na.rm = TRUE),
    breath_pleasant_mean = rowMeans(
      select(., matches("postfloat_\\d+_fspost_50")), na.rm = TRUE),
    heart_pleasant_mean = rowMeans(
      select(., matches("postfloat_\\d+_fspost_51")), na.rm = TRUE),
    gi_pleasant_mean = rowMeans(
      select(., matches("postfloat_\\d+_fspost_52")), na.rm = TRUE)
  )

# ============================================
# 4. COMPUTE MAIA SUBSCALE MEANS
# ============================================

maia_subscales <- list(
  selfreg        = 23:26,
  bodylisten     = 27:29,
  noticing       = 1:4,
  notdistracting = 5:7,
  notworrying    = 8:10,
  attreg         = 11:17,
  emoaware       = 18:22,
  trusting       = 30:32
)

maia_floats <- c(1, 4, 6)

for (subscale_name in names(maia_subscales)) {
  
  items <- maia_subscales[[subscale_name]]
  
  expected_cols <- unlist(lapply(maia_floats, function(flt) {
    paste0("postfloat_", flt, "_maia_float_", items)
  }))
  
  existing_cols <- intersect(expected_cols, names(raw_data))
  
  if (length(existing_cols) > 0) {
    raw_data[existing_cols] <- lapply(raw_data[existing_cols], function(x) {
      as.numeric(as.character(x))
    })
  }
  
  # Per-float means
  for (flt in maia_floats) {
    float_cols <- paste0("postfloat_", flt, "_maia_float_", items)
    float_cols <- intersect(float_cols, names(raw_data))
    
    if (length(float_cols) > 0) {
      new_col <- paste0("maia_", subscale_name, "_float_", flt, "_mean")
      raw_data[[new_col]] <- rowMeans(raw_data[, float_cols, drop = FALSE], na.rm = TRUE)
    }
  }
  
  # Overall mean across floats
  mean_cols <- grep(paste0("^maia_", subscale_name, "_float_\\d+_mean$"),
                    names(raw_data), value = TRUE)
  
  if (length(mean_cols) > 0) {
    overall_col <- paste0("maias_", subscale_name)
    raw_data[[overall_col]] <- rowMeans(raw_data[, mean_cols, drop = FALSE], na.rm = TRUE)
  }
}

# ============================================
# 5. MERGE NEW COLUMNS INTO only_complete_data
# ============================================

raw_data$grp <- as.integer(raw_data$grp)
only_complete_data$grp <- as.integer(only_complete_data$grp)

cols_to_add <- setdiff(names(raw_data), names(only_complete_data))
cols_to_add <- union("grp", cols_to_add)

only_complete_data <- only_complete_data %>%
  left_join(raw_data %>% select(all_of(cols_to_add)), by = "grp")

# Check for unmatched
unmatched <- anti_join(only_complete_data %>% select(grp), raw_data %>% select(grp), by = "grp")
if (nrow(unmatched) > 0) {
  message("Warning: some grp values in only_complete_data did not match raw_data: ",
          paste(unmatched$grp, collapse = ", "))
}

# ============================================
# 6. COMPUTE SIDE EFFECTS AVERAGES (for phenom follow-up)
# ============================================

for (sec in 30:44) {
  cols <- paste0("postfloat_", 1:6, "_sec_", sec)
  cols <- intersect(cols, names(only_complete_data))
  only_complete_data[[paste0("average_sec_", sec)]] <- if (length(cols)) {
    rowMeans(only_complete_data[, cols, drop = FALSE], na.rm = TRUE)
  } else {
    NA_real_
  }
}

only_complete_data <- only_complete_data %>%
  mutate(mean_pos_effects = rowMeans(across(paste0("average_sec_", 30:44)), na.rm = TRUE))

# ============================================
# 7. DEFINE ANALYSIS PARAMETERS AND RENAME
# ============================================

# Outcome variables
outcomes <- c(
  "diff_panasx_negaffect_score", 
  "diff_mean_stais", 
  "diff_hama", 
  "diff_madrs", 
  "diff_asi3r_total", 
  "diff_panasx_posaffect_score"
)

# Mediator variables
mediators <- c(
  "alt_postfloat_6_dasc_5d_oceanic_boundlessness_score",
  "alt_postfloat_6_dasc_5d_anxious_ego_dissolution_score",
  "maias_bodylisten",
  "maias_emoaware",
  "maias_selfreg",
  "breath_intensity_mean",
  "heart_intensity_mean",
  "gi_intensity_mean",
  "breath_pleasant_mean",
  "heart_pleasant_mean",
  "gi_pleasant_mean"
)

# Covariates
covariates <- c("age")

# 5D-ASC subscales
subscales_5d <- c(
  "Oceanic Boundlessness" = "alt_postfloat_6_dasc_5d_oceanic_boundlessness_score",
  "Anxious Ego Dissolution" = "alt_postfloat_6_dasc_5d_anxious_ego_dissolution_score",
  "Visual Restructuralization" = "alt_postfloat_6_dasc_5d_visual_restructuralization_score",
  "Auditory Alterations" = "alt_postfloat_6_dasc_5d_auditory_alterations_score",
  "Reduction of Vigilance" = "alt_postfloat_6_dasc_5d_reduction_of_vigilance_score"
)

# 11-ASC subscales
subscales_11d <- c(
  "Experience of Unity" = "alt_postfloat_6_dasc_11d_unity_score",
  "Spiritual Experience" = "alt_postfloat_6_dasc_11d_spiritual_score",
  "Blissful State" = "alt_postfloat_6_dasc_11d_bliss_score",
  "Insightfulness" = "alt_postfloat_6_dasc_11d_insight_score",
  "Disembodiment" = "alt_postfloat_6_dasc_11d_disembodiment_score",
  "Impaired Control and Cognition" = "alt_postfloat_6_dasc_11d_impaired_score",
  "Anxiety" = "alt_postfloat_6_dasc_11d_anxiety_score",
  "Complex Imagery" = "alt_postfloat_6_dasc_11d_com_imagery_score",
  "Elementary Imagery" = "alt_postfloat_6_dasc_11d_ele_imagery_score",
  "Audio-Visual Synesthesia" = "alt_postfloat_6_dasc_11d_synesthesiae_score",
  "Changed Meaning of Percepts" = "alt_postfloat_6_dasc_11d_percepts_score"
)


# Interoception subscales
subscales_intero <- c(
  "Self-Regulation" = "maias_selfreg",
  "Body Listening" = "maias_bodylisten",
  "Noticing" = "maias_noticing",
  "Not Distracting" = "maias_notdistracting",
  "Not Worrying" = "maias_notworrying",
  "Attention Regulation" = "maias_attreg",
  "Emotional Awareness" = "maias_emoaware",
  "Trusting" = "maias_trusting",
  "Breath Intensity" = "breath_intensity_mean",
  "Heart Intensity" = "heart_intensity_mean",
  "GI Intensity" = "gi_intensity_mean",
  "Breath Pleasantness" = "breath_pleasant_mean",
  "Heart Pleasantness" = "heart_pleasant_mean",
  "GI Pleasantness" = "gi_pleasant_mean"
)

# Clinical outcomes subscales
subscales_clinical <- c(
  "Negative Affect (PANAS)" = "diff_panasx_negaffect_score",
  "STAI-S" = "diff_mean_stais",
  "HAM-A" = "diff_hama",
  "MADRS" = "diff_madrs",
  "ASI-3R" = "diff_asi3r_total",
  "Positive Affect (PANAS)" = "diff_panasx_posaffect_score"
)

# PANAS subscales
subscales_panas <- c(
  "Joviality" = "diff_panasx_joviality_score",
  "Attentiveness" = "diff_panasx_attentiveness_score1",
  "Fatigue" = "diff_panasx_fatigue_score",
  "Serenity" = "diff_panasx_serenity_score",
  "Fear" = "diff_panasx_fear_score1"
)

# Pre-post pairs for t-tests
prepost_pairs <- list(
  "Negative Affect (PANAS)" = c("prefloat_mean_panasx_negaffect_score", "postfloat_mean_panasx_negaffect_score"),
  "Positive Affect (PANAS)" = c("prefloat_mean_panasx_posaffect_score", "postfloat_mean_panasx_posaffect_score"),
  "STAI-S" = c("prefloat_mean_stais", "postfloat_mean_stais"),
  "ASI-3R" = c("baseline_asi3r_score", "postintervention_asi3r_score"),
  "HAM-A" = c("baseline_hama_score", "postintervention_hama_score"),
  "MADRS" = c("baseline_madrs_score", "postintervention_madrs_score")
)

# ============================================
# 8. SAVE CLEANED DATA AND PARAMETERS
# ============================================

cleaned_data <- list(
  raw_data = raw_data,
  only_complete = only_complete_data,
  
  # Analysis parameters
  outcomes = outcomes,
  mediators = mediators,
  covariates = covariates,
  
  # Subscale definitions
  subscales_11d = subscales_11d,
  subscales_5d = subscales_5d,
  subscales_intero = subscales_intero,
  subscales_clinical = subscales_clinical,
  subscales_panas = subscales_panas,
  prepost_pairs = prepost_pairs
)

saveRDS(cleaned_data, "data/cleaned_data.rds")

cat("Data cleaning complete. Saved to data/cleaned_data.rds\n")
