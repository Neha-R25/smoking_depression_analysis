# Cigarette Smoking and Depression Among Adolescents (YRBS 2023)
# Analysis script for a public health portfolio project

# =========================
# 1) Setup
# =========================

# Load required packages
required_packages <- c("tidyverse", "janitor")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(tidyverse)
library(janitor)

# Define project-relative paths (no hard-coded absolute paths)
project_root <- "."
data_path <- file.path(project_root, "data", "yrbs_2023.csv")
results_path <- file.path(project_root, "results")
figures_path <- file.path(project_root, "figures")

# Create output folders if they do not exist
if (!dir.exists(results_path)) dir.create(results_path, recursive = TRUE)
if (!dir.exists(figures_path)) dir.create(figures_path, recursive = TRUE)

# =========================
# 2) Data loading
# =========================

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Data file not found at: ", data_path,
      "\nPlace YRBS 2023 data as 'data/yrbs_2023.csv' and rerun."
    )
  )
}

yrbs_raw <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  janitor::clean_names()

# =========================
# 3) Data cleaning and variable creation
# =========================
# Expected source variables (adjust if your variable names differ):
# - smoked_30d: smoking in past 30 days (Yes/No or 1/0)
# - felt_sad_hopeless: persistent sadness/hopelessness (Yes/No or 1/0)

analysis_df <- yrbs_raw %>%
  mutate(
    smoked_30d = case_when(
      smoked_30d %in% c("Yes", "Y", "1", 1, TRUE) ~ 1,
      smoked_30d %in% c("No", "N", "0", 0, FALSE) ~ 0,
      TRUE ~ NA_real_
    ),
    depressed = case_when(
      felt_sad_hopeless %in% c("Yes", "Y", "1", 1, TRUE) ~ 1,
      felt_sad_hopeless %in% c("No", "N", "0", 0, FALSE) ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(smoked_30d), !is.na(depressed)) %>%
  mutate(
    smoked_30d = factor(smoked_30d, levels = c(0, 1), labels = c("No", "Yes")),
    depressed = factor(depressed, levels = c(0, 1), labels = c("No", "Yes"))
  )

# =========================
# 4) Statistical analysis
# =========================

# 4a. Descriptive statistics
sample_n <- nrow(analysis_df)
smoking_prev <- mean(analysis_df$smoked_30d == "Yes")
depression_prev <- mean(analysis_df$depressed == "Yes")

summary_tbl <- tibble(
  metric = c("Sample size", "Current smoking prevalence", "Depression prevalence"),
  value = c(
    as.character(sample_n),
    scales::percent(smoking_prev, accuracy = 0.1),
    scales::percent(depression_prev, accuracy = 0.1)
  )
)

# 4b. Cross-tabulation
cross_tab <- analysis_df %>%
  count(depressed, smoked_30d) %>%
  group_by(depressed) %>%
  mutate(percent_within_depression = n / sum(n)) %>%
  ungroup()

# 4c. Chi-square test of association
contingency_table <- table(analysis_df$depressed, analysis_df$smoked_30d)
chi_result <- chisq.test(contingency_table)

chi_tbl <- tibble(
  statistic = unname(chi_result$statistic),
  df = unname(chi_result$parameter),
  p_value = chi_result$p.value
)

# Save tabular outputs
readr::write_csv(summary_tbl, file.path(results_path, "descriptive_summary.csv"))
readr::write_csv(cross_tab, file.path(results_path, "cross_tab_depression_smoking.csv"))
readr::write_csv(chi_tbl, file.path(results_path, "chi_square_results.csv"))

# =========================
# 5) Visualization
# =========================

plot_df <- cross_tab %>%
  mutate(percent = percent_within_depression * 100)

fig <- ggplot(plot_df, aes(x = depressed, y = percent, fill = smoked_30d)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Smoking prevalence by depression status (YRBS 2023)",
    x = "Persistent sadness/hopelessness",
    y = "Percent of students",
    fill = "Smoked in past 30 days"
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal(base_size = 12)

figure_file <- file.path(figures_path, "depression_smoking_analysis.png")
ggsave(figure_file, fig, width = 8, height = 5, dpi = 300)

message("Analysis completed successfully.")
message("Outputs written to '/results' and '/figures'.")
