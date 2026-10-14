
# ============================================================
# PHASE 3A: BUILD THE MODELLING DATASET
# Project: R-InsightRAG
# Target: high_growth_next_year
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 3A: BUILD MODELLING DATASET\n")
cat("============================================\n")

# 1. Load the validated company-year financial data
data <- read_csv(
  "data/processed/financial_facts_company_year_improved.csv",
  show_col_types = FALSE
) %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  ) %>%
  arrange(company_id, year)

# 2. Check required columns
required_columns <- c(
  "company_id", "company_name", "year", "revenue",
  "net_income", "operating_income", "total_assets",
  "total_liabilities", "total_equity",
  "current_assets", "current_liabilities",
  "operating_cash_flow", "investing_cash_flow",
  "financing_cash_flow", "rd_expense"
)

missing_columns <- setdiff(required_columns, names(data))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 3. Verify company-year keys
duplicate_keys <- data %>%
  count(company_id, year) %>%
  filter(n > 1)

if (nrow(duplicate_keys) > 0) {
  stop("Duplicate company-year keys found. Resolve before modelling.")
}

# 4. Sort and construct current-year features and next-year target
model_data <- data %>%
  group_by(company_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    previous_year = lag(year),
    previous_revenue = lag(revenue),
    
    next_year = lead(year),
    next_revenue = lead(revenue),
    
    consecutive_previous_year =
      year == previous_year + 1,
    
    consecutive_next_year =
      next_year == year + 1,
    
    # Historical growth: uses only the current and previous years
    revenue_growth_current = if_else(
      !is.na(revenue) &
        !is.na(previous_revenue) &
        previous_revenue > 0 &
        consecutive_previous_year,
      revenue / previous_revenue - 1,
      NA_real_
    ),
    
    # Financial ratios calculated from current-year information
    operating_margin = if_else(
      !is.na(operating_income) &
        !is.na(revenue) &
        revenue > 0,
      operating_income / revenue,
      NA_real_
    ),
    
    net_profit_margin = if_else(
      !is.na(net_income) &
        !is.na(revenue) &
        revenue > 0,
      net_income / revenue,
      NA_real_
    ),
    
    current_ratio = if_else(
      !is.na(current_assets) &
        !is.na(current_liabilities) &
        current_liabilities > 0,
      current_assets / current_liabilities,
      NA_real_
    ),
    
    liabilities_to_assets = if_else(
      !is.na(total_liabilities) &
        !is.na(total_assets) &
        total_assets > 0,
      total_liabilities / total_assets,
      NA_real_
    ),
    
    equity_to_assets = if_else(
      !is.na(total_equity) &
        !is.na(total_assets) &
        total_assets > 0,
      total_equity / total_assets,
      NA_real_
    ),
    
    operating_cash_flow_to_assets = if_else(
      !is.na(operating_cash_flow) &
        !is.na(total_assets) &
        total_assets > 0,
      operating_cash_flow / total_assets,
      NA_real_
    ),
    
    rd_to_revenue = if_else(
      !is.na(rd_expense) &
        !is.na(revenue) &
        revenue > 0,
      rd_expense / revenue,
      NA_real_
    ),
    
    # Target: next-year revenue growth >= 10%
    valid_target =
      !is.na(revenue) &
      revenue > 0 &
      !is.na(next_revenue) &
      next_revenue >= 0 &
      consecutive_next_year,
    
    next_year_growth = if_else(
      valid_target,
      next_revenue / revenue - 1,
      NA_real_
    ),
    
    high_growth_next_year = if_else(
      valid_target,
      as.integer(next_year_growth >= 0.10),
      NA_integer_
    )
  ) %>%
  ungroup()

# 5. Keep only rows with a valid target for the modelling file
model_ready <- model_data %>%
  filter(valid_target) %>%
  select(
    company_id,
    company_name,
    year,
    high_growth_next_year,
    
    # Current-year raw financial features
    revenue,
    net_income,
    operating_income,
    total_assets,
    total_liabilities,
    total_equity,
    current_assets,
    current_liabilities,
    operating_cash_flow,
    investing_cash_flow,
    financing_cash_flow,
    rd_expense,
    
    # Derived predictors
    revenue_growth_current,
    operating_margin,
    net_profit_margin,
    current_ratio,
    liabilities_to_assets,
    equity_to_assets,
    operating_cash_flow_to_assets,
    rd_to_revenue
  ) %>%
  arrange(year, company_id)

# 6. Summary checks
cat("\n--- Dataset dimensions ---\n")
cat("Original rows:", nrow(data), "\n")
cat("Modelling rows:", nrow(model_ready), "\n")
cat("Companies:", n_distinct(model_ready$company_id), "\n")

cat("\n--- Target class distribution ---\n")

class_summary <- model_ready %>%
  count(high_growth_next_year, name = "count") %>%
  mutate(
    percentage = round(100 * count / sum(count), 2)
  )

print(class_summary)

cat("\n--- Rows per target year ---\n")

print(
  model_ready %>%
    count(year, name = "rows") %>%
    arrange(year),
  n = Inf
)

# 7. Feature missingness
feature_columns <- setdiff(
  names(model_ready),
  c("company_id", "company_name", "year",
    "high_growth_next_year")
)

missingness <- data.frame(
  feature = feature_columns,
  missing_count = vapply(
    model_ready[feature_columns],
    function(x) sum(is.na(x)),
    integer(1)
  )
) %>%
  mutate(
    missing_percentage = round(
      100 * missing_count / nrow(model_ready),
      2
    )
  ) %>%
  arrange(desc(missing_percentage))

cat("\n--- Feature missingness ---\n")
print(missingness, row.names = FALSE)

# 8. Save outputs
write_csv(
  model_ready,
  "data/processed/modeling_dataset.csv"
)

write_csv(
  class_summary,
  "outputs/phase3a_target_class_summary.csv"
)

write_csv(
  missingness,
  "outputs/phase3a_feature_missingness.csv"
)

write_csv(
  model_data %>%
    filter(!valid_target) %>%
    select(company_id, company_name, year, revenue, next_revenue),
  "outputs/phase3a_rows_without_valid_target.csv"
)

cat("\n--- Saved files ---\n")
cat("data/processed/modeling_dataset.csv\n")
cat("outputs/phase3a_target_class_summary.csv\n")
cat("outputs/phase3a_feature_missingness.csv\n")
cat("outputs/phase3a_rows_without_valid_target.csv\n")

cat("\nNo original financial values were modified.\n")
cat("No missing predictor values were imputed.\n")
cat("PHASE 3A COMPLETED\n")
cat("============================================\n")
