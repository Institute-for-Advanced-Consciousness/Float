# ============================================
# 05_measure_admin_table.R
# Produces table summarizing measurement admin data
# ============================================

source("R/00_setup.R")

# Load the schedule data
sched <- readxl::read_excel("data/final_schedule_binary.xlsx", sheet = "Schedule")

# ============================================
# Create table
# ============================================

# Convert binary indicators to checkmarks and dashes
sched_formatted <- sched %>%
  dplyr::mutate(dplyr::across(-Measure, ~ ifelse(. == 1, "\u2713", "–")))

# Create gt table
measure_admin_table <- sched_formatted %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Schedule of Measure Administration Across Sessions"
  ) %>%
  gt::cols_align(
    align = "center",
    columns = -Measure
  ) %>%
  gt::cols_align(
    align = "left",
    columns = Measure
  ) %>%
  gt::tab_options(
    table.font.size = 10,
    heading.title.font.size = 12,
    column_labels.font.weight = "bold"
  )

# ============================================
# Save table
# ============================================

gt::gtsave(measure_admin_table, "output/tables/measure_admin_table.png")

cat("Measure administration table complete.\n")
cat("Table saved to output/tables/measure_admin_table.png\n")
