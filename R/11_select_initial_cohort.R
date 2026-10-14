
# ============================================================
# R-InsightRAG - Phase 2K
# Select Initial Eight-Year Company Cohort
# ============================================================

library(tidyverse)

project_path <- getwd()

processed_path <- file.path(
  project_path,
  "data",
  "processed"
)

coverage_file <- file.path(
  processed_path,
  "company_fiscal_year_coverage.csv"
)

if (!file.exists(coverage_file)) {
  stop("Run Phase 2J first. Coverage file not found.")
}

company_coverage <- read_csv(
  coverage_file,
  show_col_types = FALSE
)

# Initial selection based on historical coverage only
cohort <- company_coverage %>%
  filter(years_available == 8) %>%
  arrange(company_id)

# Basic validation
if (nrow(cohort) == 0) {
  stop("No companies with eight fiscal years were found.")
}

if (anyDuplicated(cohort$company_id) > 0) {
  stop("Duplicate company IDs detected.")
}

write_csv(
  cohort,
  file.path(
    processed_path,
    "initial_company_cohort_8years.csv"
  )
)

cat("\n===== PHASE 2K RESULTS =====\n")
cat("Companies selected:", nrow(cohort), "\n")
cat(
  "Distinct company IDs:",
  n_distinct(cohort$company_id),
  "\n"
)

cat("\nSample selected companies:\n")

print(
  cohort %>%
    select(
      company_id,
      company_name,
      years_available,
      first_fiscal_year,
      last_fiscal_year
    ) %>%
    head(10)
)

cat("\nSaved to:\n")
cat(
  file.path(
    processed_path,
    "initial_company_cohort_8years.csv"
  ),
  "\n"
)
