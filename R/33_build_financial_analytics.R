
# ============================================================
# R-InsightRAG
# PHASE 4A: FINANCIAL ANALYTICS ENGINE
# ============================================================

cat("\n=========================================\n")
cat("PHASE 4A: FINANCIAL ANALYTICS ENGINE\n")
cat("=========================================\n")

library(dplyr)

# ------------------------------------------------------------
# 1. Load improved company-year financial data
# ------------------------------------------------------------

input_file <-
  "data/processed/financial_facts_company_year_improved.csv"

if (!file.exists(input_file)) {
  stop(paste("Input file not found:", input_file))
}

financial <- read.csv(
  input_file,
  stringsAsFactors = FALSE
)

required_columns <- c(
  "company_id", "company_name", "year",
  "revenue", "net_income", "operating_income",
  "total_assets", "total_liabilities", "total_equity",
  "current_assets", "current_liabilities",
  "operating_cash_flow", "investing_cash_flow",
  "financing_cash_flow", "rd_expense"
)

missing_columns <- setdiff(required_columns, names(financial))

if (length(missing_columns) > 0) {
  stop(paste(
    "Missing required columns:",
    paste(missing_columns, collapse = ", ")
  ))
}

financial <- financial %>%
  mutate(
    year = as.integer(year)
  ) %>%
  arrange(company_id, year)

# ------------------------------------------------------------
# 2. Validate company-year keys
# ------------------------------------------------------------

duplicate_keys <- financial %>%
  count(company_id, year) %>%
  filter(n > 1)

if (nrow(duplicate_keys) > 0) {
  stop("Duplicate company-year records detected.")
}

cat("\nInput rows:", nrow(financial), "\n")
cat("Companies:", n_distinct(financial$company_id), "\n")

# ------------------------------------------------------------
# 3. Build financial analytics features
# ------------------------------------------------------------

analytics <- financial %>%
  group_by(company_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    previous_year = lag(year),
    previous_revenue = lag(revenue),
    
    # Revenue growth is valid only for consecutive years
    revenue_growth = if_else(
      year - previous_year == 1 &
        !is.na(revenue) &
        !is.na(previous_revenue) &
        previous_revenue > 0,
      revenue / previous_revenue - 1,
      NA_real_
    ),
    
    # Profitability
    operating_margin = if_else(
      !is.na(revenue) & revenue > 0 &
        !is.na(operating_income),
      operating_income / revenue,
      NA_real_
    ),
    
    net_profit_margin = if_else(
      !is.na(revenue) & revenue > 0 &
        !is.na(net_income),
      net_income / revenue,
      NA_real_
    ),
    
    # Liquidity
    current_ratio = if_else(
      !is.na(current_assets) &
        !is.na(current_liabilities) &
        current_liabilities > 0,
      current_assets / current_liabilities,
      NA_real_
    ),
    
    # Capital structure
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
    
    # Cash flow
    operating_cash_flow_to_assets = if_else(
      !is.na(operating_cash_flow) &
        !is.na(total_assets) &
        total_assets > 0,
      operating_cash_flow / total_assets,
      NA_real_
    ),
    
    investing_cash_flow_to_assets = if_else(
      !is.na(investing_cash_flow) &
        !is.na(total_assets) &
        total_assets > 0,
      investing_cash_flow / total_assets,
      NA_real_
    ),
    
    financing_cash_flow_to_assets = if_else(
      !is.na(financing_cash_flow) &
        !is.na(total_assets) &
        total_assets > 0,
      financing_cash_flow / total_assets,
      NA_real_
    ),
    
    # R&D intensity
    rd_to_revenue = if_else(
      !is.na(rd_expense) &
        !is.na(revenue) &
        revenue > 0,
      rd_expense / revenue,
      NA_real_
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 4. Create a company-year quality summary
# ------------------------------------------------------------

metric_columns <- c(
  "revenue_growth",
  "operating_margin",
  "net_profit_margin",
  "current_ratio",
  "liabilities_to_assets",
  "equity_to_assets",
  "operating_cash_flow_to_assets",
  "investing_cash_flow_to_assets",
  "financing_cash_flow_to_assets",
  "rd_to_revenue"
)

coverage <- data.frame(
  metric = metric_columns,
  available_rows = vapply(
    metric_columns,
    function(column) sum(!is.na(analytics[[column]])),
    integer(1)
  ),
  missing_rows = vapply(
    metric_columns,
    function(column) sum(is.na(analytics[[column]])),
    integer(1)
  )
)

coverage$coverage_pct <-
  100 * coverage$available_rows / nrow(analytics)

# ------------------------------------------------------------
# 5. Save outputs
# ------------------------------------------------------------

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "outputs",
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  analytics,
  "data/processed/financial_analytics_company_year.csv",
  row.names = FALSE
)

write.csv(
  coverage,
  "outputs/phase4a_financial_metric_coverage.csv",
  row.names = FALSE
)

cat("\n--- Metric coverage ---\n")
print(coverage, row.names = FALSE)

cat("\nSaved:\n")
cat("- data/processed/financial_analytics_company_year.csv\n")
cat("- outputs/phase4a_financial_metric_coverage.csv\n")

cat("\nPHASE 4A COMPLETED\n")
cat("=========================================\n")
