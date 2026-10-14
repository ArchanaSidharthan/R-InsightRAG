
# ============================================================
# PROJECT: R-InsightRAG
# PHASE 2M: FINANCIAL DATA QUALITY AUDIT
# File: R/13_audit_financial_data.R
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. PROJECT PATHS
# ------------------------------------------------------------

project_root <- file.path(
  "C:/Users/archa/OneDrive/Desktop/Projects",
  "R-InsightRAG"
)

processed_dir <- file.path(
  project_root,
  "data",
  "processed"
)

data_file <- file.path(
  processed_dir,
  "financial_facts_company_year.csv"
)

# ------------------------------------------------------------
# 2. CHECK FILE EXISTS AND LOAD DATA
# ------------------------------------------------------------

if (!file.exists(data_file)) {
  stop(
    "Dataset not found: ", data_file,
    "\nCheck the project path and filename."
  )
}

df <- read_csv(
  data_file,
  show_col_types = FALSE
)

cat("\n")
cat("====================================================\n")
cat("R-INSIGHTRAG: PHASE 2M DATA QUALITY AUDIT\n")
cat("====================================================\n")

# Confirm required columns exist
required_columns <- c(
  "company_id",
  "year",
  "company_name",
  "revenue",
  "net_income",
  "total_assets"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  stop(
    "Required columns are missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Standardize identifiers and year
df <- df %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  )

# ------------------------------------------------------------
# 3. DATASET DIMENSIONS
# ------------------------------------------------------------

cat("\n--- 1. DATASET DIMENSIONS ---\n")

cat("Total rows:", nrow(df), "\n")
cat("Total columns:", ncol(df), "\n")
cat(
  "Unique companies:",
  n_distinct(df$company_id),
  "\n"
)
cat(
  "First fiscal year:",
  min(df$year, na.rm = TRUE),
  "\n"
)
cat(
  "Last fiscal year:",
  max(df$year, na.rm = TRUE),
  "\n"
)

cat("\nColumn names:\n")
print(names(df))

cat("\nData types:\n")

data_types <- tibble(
  column = names(df),
  data_type = map_chr(
    df,
    ~ paste(class(.x), collapse = "/")
  )
)

print(data_types, n = Inf)

# ------------------------------------------------------------
# 4. COMPANY-YEAR DUPLICATE CHECK
# ------------------------------------------------------------

cat("\n--- 2. DUPLICATE COMPANY-YEAR CHECK ---\n")

duplicate_keys <- df %>%
  count(
    company_id,
    year,
    name = "row_count"
  ) %>%
  filter(row_count > 1) %>%
  arrange(company_id, year)

if (nrow(duplicate_keys) == 0) {
  cat("PASS: No duplicate company-year keys found.\n")
} else {
  cat(
    "WARNING: Duplicate company-year keys found:",
    nrow(duplicate_keys),
    "\n"
  )

  print(duplicate_keys, n = Inf)
}

# ------------------------------------------------------------
# 5. ROW COUNTS BY YEAR
# ------------------------------------------------------------

cat("\n--- 3. COMPANY-YEAR ROWS BY YEAR ---\n")

year_summary <- df %>%
  group_by(year) %>%
  summarise(
    company_year_rows = n(),
    unique_companies = n_distinct(company_id),
    .groups = "drop"
  ) %>%
  arrange(year)

print(year_summary, n = Inf)

# ------------------------------------------------------------
# 6. MISSING VALUES BY COLUMN
# ------------------------------------------------------------

cat("\n--- 4. MISSING VALUES BY COLUMN ---\n")

missing_summary <- df %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  mutate(
    total_rows = nrow(df),
    available_count = total_rows - missing_count,
    missing_percentage = round(
      100 * missing_count / total_rows,
      2
    )
  ) %>%
  select(
    variable,
    total_rows,
    available_count,
    missing_count,
    missing_percentage
  ) %>%
  arrange(desc(missing_percentage))

print(missing_summary, n = Inf)

# ------------------------------------------------------------
# 7. REVENUE QUALITY CHECK
# ------------------------------------------------------------

cat("\n--- 5. REVENUE QUALITY CHECK ---\n")

revenue_summary <- df %>%
  summarise(
    total_rows = n(),
    available_revenue = sum(!is.na(revenue)),
    missing_revenue = sum(is.na(revenue)),
    zero_revenue = sum(revenue == 0, na.rm = TRUE),
    negative_revenue = sum(revenue < 0, na.rm = TRUE),
    revenue_coverage_pct = round(
      100 * sum(!is.na(revenue)) / n(),
      2
    )
  )

print(revenue_summary)

# Revenue coverage by year
cat("\nRevenue coverage by year:\n")

revenue_by_year <- df %>%
  group_by(year) %>%
  summarise(
    company_year_rows = n(),
    revenue_available = sum(!is.na(revenue)),
    revenue_missing = sum(is.na(revenue)),
    zero_revenue = sum(revenue == 0, na.rm = TRUE),
    negative_revenue = sum(revenue < 0, na.rm = TRUE),
    revenue_coverage_pct = round(
      100 * sum(!is.na(revenue)) / n(),
      2
    ),
    .groups = "drop"
  ) %>%
  arrange(year)

print(revenue_by_year, n = Inf)

# ------------------------------------------------------------
# 8. NET INCOME QUALITY CHECK
# ------------------------------------------------------------

cat("\n--- 6. NET INCOME QUALITY CHECK ---\n")

net_income_summary <- df %>%
  summarise(
    total_rows = n(),
    available_net_income = sum(!is.na(net_income)),
    missing_net_income = sum(is.na(net_income)),
    zero_net_income = sum(net_income == 0, na.rm = TRUE),
    negative_net_income = sum(net_income < 0, na.rm = TRUE),
    net_income_coverage_pct = round(
      100 * sum(!is.na(net_income)) / n(),
      2
    )
  )

print(net_income_summary)

cat("\nNet income coverage by year:\n")

net_income_by_year <- df %>%
  group_by(year) %>%
  summarise(
    company_year_rows = n(),
    net_income_available = sum(!is.na(net_income)),
    net_income_missing = sum(is.na(net_income)),
    net_income_coverage_pct = round(
      100 * sum(!is.na(net_income)) / n(),
      2
    ),
    .groups = "drop"
  ) %>%
  arrange(year)

print(net_income_by_year, n = Inf)

# ------------------------------------------------------------
# 9. TOTAL ASSETS QUALITY CHECK
# ------------------------------------------------------------

cat("\n--- 7. TOTAL ASSETS QUALITY CHECK ---\n")

assets_summary <- df %>%
  summarise(
    total_rows = n(),
    available_assets = sum(!is.na(total_assets)),
    missing_assets = sum(is.na(total_assets)),
    zero_assets = sum(total_assets == 0, na.rm = TRUE),
    negative_assets = sum(total_assets < 0, na.rm = TRUE),
    assets_coverage_pct = round(
      100 * sum(!is.na(total_assets)) / n(),
      2
    )
  )

print(assets_summary)

# ------------------------------------------------------------
# 10. COVERAGE OF ALL FINANCIAL VARIABLES BY YEAR
# ------------------------------------------------------------

cat("\n--- 8. FINANCIAL VARIABLE COVERAGE BY YEAR ---\n")

financial_variables <- intersect(
  c(
    "current_assets",
    "current_liabilities",
    "financing_cash_flow",
    "investing_cash_flow",
    "operating_cash_flow",
    "operating_income",
    "rd_expense",
    "revenue",
    "total_assets",
    "total_equity",
    "total_liabilities",
    "net_income"
  ),
  names(df)
)

coverage_by_year <- df %>%
  group_by(year) %>%
  summarise(
    across(
      all_of(financial_variables),
      ~ sum(!is.na(.x)),
      .names = "{.col}_available"
    ),
    .groups = "drop"
  ) %>%
  arrange(year)

print(coverage_by_year, n = Inf)

# ------------------------------------------------------------
# 11. COMPANY COVERAGE BY YEAR
# ------------------------------------------------------------

cat("\n--- 9. UNIQUE COMPANY COVERAGE BY YEAR ---\n")

company_coverage <- df %>%
  group_by(year) %>%
  summarise(
    unique_companies = n_distinct(company_id),
    .groups = "drop"
  ) %>%
  arrange(year)

print(company_coverage, n = Inf)

# ------------------------------------------------------------
# 12. SAVE AUDIT REPORTS
# ------------------------------------------------------------

write_csv(
  data_types,
  file.path(processed_dir, "audit_data_types.csv")
)

write_csv(
  duplicate_keys,
  file.path(
    processed_dir,
    "audit_duplicate_company_years.csv"
  )
)

write_csv(
  year_summary,
  file.path(processed_dir, "audit_rows_by_year.csv")
)

write_csv(
  missing_summary,
  file.path(processed_dir, "audit_missingness.csv")
)

write_csv(
  revenue_summary,
  file.path(processed_dir, "audit_revenue_summary.csv")
)

write_csv(
  revenue_by_year,
  file.path(processed_dir, "audit_revenue_by_year.csv")
)

write_csv(
  net_income_summary,
  file.path(processed_dir, "audit_net_income_summary.csv")
)

write_csv(
  net_income_by_year,
  file.path(processed_dir, "audit_net_income_by_year.csv")
)

write_csv(
  assets_summary,
  file.path(processed_dir, "audit_assets_summary.csv")
)

write_csv(
  coverage_by_year,
  file.path(processed_dir, "audit_financial_coverage_by_year.csv")
)

write_csv(
  company_coverage,
  file.path(processed_dir, "audit_company_coverage_by_year.csv")
)

# ------------------------------------------------------------
# 13. FINAL STATUS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("PHASE 2M: AUDIT COMPLETED SUCCESSFULLY\n")
cat("====================================================\n")

cat("Dataset rows:", nrow(df), "\n")
cat("Unique companies:", n_distinct(df$company_id), "\n")
cat("Duplicate company-year keys:", nrow(duplicate_keys), "\n")

cat(
  "Revenue coverage:",
  round(100 * sum(!is.na(df$revenue)) / nrow(df), 2),
  "%\n"
)

cat(
  "Net income coverage:",
  round(100 * sum(!is.na(df$net_income)) / nrow(df), 2),
  "%\n"
)

cat(
  "\nAudit reports saved in:\n",
  processed_dir,
  "\n"
)

cat("====================================================\n")
