# ============================================
# 01_demos_table.R
# Create demographics table
# ============================================

source("R/00_setup.R")
demo_data <- read.csv("data/demo_data.csv")

demo_table <- demo_data %>%
    tbl_summary(
        by = ConditionCollapsed,
        statistic = list(
            AgeAtBaseline ~ "{mean} ({sd})",
            Sex ~ "{n} ({p}%)",
            Education_Corrected ~ "{n} ({p}%)",
            BMI ~ "{mean} ({sd})",
            oasis_score ~ "{mean} ({sd})",
            asi3r_score ~ "{mean} ({sd})",
            phq_score ~ "{mean} ({sd})"
        ),
        missing = "no"
    )

# Display table
print(demo_table)
