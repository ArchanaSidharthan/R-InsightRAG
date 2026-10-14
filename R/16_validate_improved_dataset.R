
# ============================================================
# R-InsightRAG
# PHASE 2P: VALIDATE IMPROVED FINANCIAL DATASET
# File: R/16_validate_improved_dataset.R
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

project_root <- file.path(
  "C:/Users/archa/OneDrive/Desktop/Projects",
  "R-InsightRAG"
)

processed_dir <- file.path(
  project_root, "data", "processed"
)

output_dir <- file.path(
  project_root, "outputs"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

original_file <- file.path(
  processed_dir,
  "financial_facts_company_year.csv"
)

improved_file <- file.path(
  processed_dir,
  "financial_facts_company_year_improved.csv"
)

selected_facts_file <- file.path(
  output_dir,
  "phase2o_selected_facts.csv"
)

required_files <- c(
  original_file,
  improved_file,
  selected_facts_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Required files not found:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------

original <- read_csv(
  original_file,
  show_col_types = FALSE
)

improved <- read_csv(
  improved_file,
  show_col_types = FALSE
)

selected_facts <- read_csv(
  selected_facts_file,
  show_col_types = FALSE
)

original <- original %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  )

improved <- improved %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  )

cat("\n====================================================\n")
cat("PHASE 2P: DATASET VALIDATION\n")
cat("====================================================\n")

cat("\nOriginal rows:", nrow(original), "\n")
cat("Improved rows:", nrow(improved), "\n")
cat(
  "Original companies:",
  n_distinct(original$company_id), "\n"
)
cat(
  "Improved companies:",
  n_distinct(improved$company_id), "\n"
)

# ------------------------------------------------------------
# 3. CHECK COMPANY-YEAR KEYS
# ------------------------------------------------------------

cat("\n--- Duplicate company-year keys ---\n")

original_duplicates <- original %>%
  count(company_id, year) %>%
  filter(n > 1)

improved_duplicates <- improved %>%
  count(company_id, year) %>%
  filter(n > 1)

cat(
  "Original duplicate keys:",
  nrow(original_duplicates), "\n"
)

cat(
  "Improved duplicate keys:",
  nrow(improved_duplicates), "\n"
)

write_csv(
  original_duplicates,
  file.path(output_dir, "phase2p_original_duplicates.csv")
)

write_csv(
  improved_duplicates,
  file.path(output_dir, "phase2p_improved_duplicates.csv")
)

# ------------------------------------------------------------
# 4. COMPARE VARIABLE COVERAGE
# ------------------------------------------------------------

variables <- intersect(
  c(
    "revenue",
    "net_income",
    "operating_income",
    "total_assets",
    "total_liabilities",
    "total_equity",
    "current_assets",
    "current_liabilities",
    "operating_cash_flow",
    "investing_cash_flow",
    "financing_cash_flow",
    "rd_expense"
  ),
  intersect(names(original), names(improved))
)

coverage_comparison <- map_dfr(
  variables,
  function(variable) {
    
    original_count <- sum(
      !is.na(original[[variable]])
    )
    
    improved_count <- sum(
      !is.na(improved[[variable]])
    )
    
    tibble(
      variable = variable,
      original_available = original_count,
      improved_available = improved_count,
      change_in_available_rows =
        improved_count - original_count,
      original_coverage_pct = round(
        100 * original_count / nrow(original), 2
      ),
      improved_coverage_pct = round(
        100 * improved_count / nrow(improved), 2
      )
    )
  }
)

cat("\n--- Original vs improved coverage ---\n")
print(coverage_comparison, n = Inf)

write_csv(
  coverage_comparison,
  file.path(output_dir, "phase2p_coverage_comparison.csv")
)

# ------------------------------------------------------------
# 5. COMPARE REVENUE AND NET INCOME VALUES
# ------------------------------------------------------------

cat("\n--- Compare financial values ---\n")

value_comparison <- original %>%
  select(
    company_id,
    year,
    company_name,
    original_revenue = revenue,
    original_net_income = net_income
  ) %>%
  inner_join(
    improved %>%
      select(
        company_id,
        year,
        improved_revenue = revenue,
        improved_net_income = net_income
      ),
    by = c("company_id", "year")
  ) %>%
  mutate(
    revenue_became_available =
      is.na(original_revenue) &
      !is.na(improved_revenue),
    
    net_income_became_available =
      is.na(original_net_income) &
      !is.na(improved_net_income),
    
    revenue_changed = case_when(
      is.na(original_revenue) |
        is.na(improved_revenue) ~ NA,
      TRUE ~ abs(
        original_revenue - improved_revenue
      ) > pmax(
        1,
        abs(original_revenue) * 1e-8
      )
    ),
    
    net_income_changed = case_when(
      is.na(original_net_income) |
        is.na(improved_net_income) ~ NA,
      TRUE ~ abs(
        original_net_income - improved_net_income
      ) > pmax(
        1,
        abs(original_net_income) * 1e-8
      )
    )
  )

cat("\nRevenue newly available:",
    sum(value_comparison$revenue_became_available),
    "\n")

cat("Net income newly available:",
    sum(value_comparison$net_income_became_available),
    "\n")

cat(
  "Revenue values changed where both existed:",
  sum(value_comparison$revenue_changed, na.rm = TRUE),
  "\n"
)

cat(
  "Net income values changed where both existed:",
  sum(value_comparison$net_income_changed, na.rm = TRUE),
  "\n"
)

write_csv(
  value_comparison,
  file.path(output_dir, "phase2p_value_comparison.csv")
)

# Save observations where improved values differ.
changed_values <- value_comparison %>%
  filter(
    revenue_became_available |
      net_income_became_available |
      revenue_changed %in% TRUE |
      net_income_changed %in% TRUE
  )

write_csv(
  changed_values,
  file.path(output_dir, "phase2p_changed_values.csv")
)

# ------------------------------------------------------------
# 6. CHECK WHICH TAGS WERE SELECTED
# ------------------------------------------------------------

cat("\n--- Selected tags by variable ---\n")

tag_usage <- selected_facts %>%
  count(
    variable,
    tag,
    priority,
    name = "selected_fact_rows"
  ) %>%
  arrange(variable, priority, desc(selected_fact_rows))

print(tag_usage, n = Inf)

write_csv(
  tag_usage,
  file.path(output_dir, "phase2p_selected_tag_usage.csv")
)

# ------------------------------------------------------------
# 7. CHECK FACT PERIOD ALIGNMENT
# ------------------------------------------------------------

cat("\n--- Selected fact period alignment ---\n")

period_audit <- selected_facts %>%
  mutate(
    ddate = as.character(ddate),
    period = as.character(period),
    period_matches = ddate == period
  ) %>%
  group_by(variable) %>%
  summarise(
    selected_facts = n(),
    matching_dates = sum(period_matches, na.rm = TRUE),
    nonmatching_dates = sum(!period_matches, na.rm = TRUE),
    missing_date_comparisons = sum(is.na(period_matches)),
    .groups = "drop"
  ) %>%
  arrange(variable)

print(period_audit, n = Inf)

write_csv(
  period_audit,
  file.path(output_dir, "phase2p_period_audit.csv")
)

# ------------------------------------------------------------
# 8. CHECK DUPLICATE SELECTED FACT KEYS
# ------------------------------------------------------------

cat("\n--- Duplicate selected fact keys ---\n")

selected_fact_duplicates <- selected_facts %>%
  count(
    company_id,
    fiscal_year,
    variable,
    name = "fact_count"
  ) %>%
  filter(fact_count > 1)

cat(
  "Duplicate company-year-variable keys:",
  nrow(selected_fact_duplicates), "\n"
)

write_csv(
  selected_fact_duplicates,
  file.path(output_dir, "phase2p_selected_fact_duplicates.csv")
)

# ------------------------------------------------------------
# 9. REVENUE GROWTH SANITY CHECK
# ------------------------------------------------------------

cat("\n--- Revenue growth sanity check ---\n")

if ("revenue" %in% names(improved)) {
  
  revenue_growth <- improved %>%
    arrange(company_id, year) %>%
    group_by(company_id) %>%
    mutate(
      previous_year = lag(year),
      previous_revenue = lag(revenue),
      
      revenue_growth = case_when(
        year - previous_year != 1 ~ NA_real_,
        is.na(revenue) ~ NA_real_,
        is.na(previous_revenue) ~ NA_real_,
        previous_revenue <= 0 ~ NA_real_,
        TRUE ~ revenue / previous_revenue - 1
      )
    ) %>%
    ungroup()
  
  growth_summary <- revenue_growth %>%
    summarise(
      valid_growth_rows = sum(!is.na(revenue_growth)),
      negative_growth_rows = sum(
        revenue_growth < 0, na.rm = TRUE
      ),
      growth_over_100pct = sum(
        revenue_growth > 1, na.rm = TRUE
      ),
      growth_below_minus_50pct = sum(
        revenue_growth < -0.5, na.rm = TRUE
      ),
      maximum_growth = if (
        all(is.na(revenue_growth))
      ) NA_real_ else max(
        revenue_growth, na.rm = TRUE
      ),
      minimum_growth = if (
        all(is.na(revenue_growth))
      ) NA_real_ else min(
        revenue_growth, na.rm = TRUE
      )
    )
  
  print(growth_summary)
  
  unusual_growth <- revenue_growth %>%
    filter(
      !is.na(revenue_growth),
      revenue_growth > 1 |
        revenue_growth < -0.5
    ) %>%
    select(
      company_id,
      company_name,
      year,
      previous_revenue,
      revenue,
      revenue_growth
    ) %>%
    arrange(desc(abs(revenue_growth)))
  
  cat("\nUnusual growth observations for review:\n")
  print(head(unusual_growth, 30), n = 30)
  
  write_csv(
    revenue_growth,
    file.path(output_dir, "phase2p_revenue_growth_audit.csv")
  )
  
  write_csv(
    unusual_growth,
    file.path(output_dir, "phase2p_unusual_revenue_growth.csv")
  )
}

# ------------------------------------------------------------
# 10. FINAL STATUS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("PHASE 2P VALIDATION COMPLETED\n")
cat("====================================================\n")

cat("\nReports saved to:\n")
cat(output_dir, "\n")

cat("\nReview these reports first:\n")
cat("1. phase2p_coverage_comparison.csv\n")
cat("2. phase2p_selected_tag_usage.csv\n")
cat("3. phase2p_period_audit.csv\n")
cat("4. phase2p_value_comparison.csv\n")
cat("5. phase2p_unusual_revenue_growth.csv\n")

cat(
  "\nDo not train models until unusual values, tag choices, ",
  "and period alignment have been reviewed.\n",
  sep = ""
)

cat("====================================================\n")
