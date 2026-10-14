# ============================================================
# R-InsightRAG
# Phase 2E - Build Financial Dataset
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

project_path <- getwd()

sec_path <- file.path(
  project_path,
  "data",
  "raw",
  "sec_2025q4"
)

processed_path <- file.path(
  project_path,
  "data",
  "processed"
)

# ------------------------------------------------------------
# 2. Read SEC files
# ------------------------------------------------------------

cat("============================================\n")
cat(" R-InsightRAG - PHASE 2E\n")
cat(" BUILD FINANCIAL DATASET\n")
cat("============================================\n\n")

cat("Reading SUB...\n")

sub_data <- read.delim(
  file.path(sec_path, "sub.txt"),
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = ""
)

cat("Reading NUM...\n")

num_data <- read.delim(
  file.path(sec_path, "num.txt"),
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = ""
)

# ------------------------------------------------------------
# 3. Select annual filings
# ------------------------------------------------------------

annual_filings <- sub_data %>%
  filter(
    form %in% c("10-K", "10-K/A"),
    fp == "FY"
  ) %>%
  select(
    adsh,
    cik,
    name,
    sic,
    form,
    period,
    fy,
    filed
  )

cat(
  "Annual filings:",
  nrow(annual_filings),
  "\n"
)

# ------------------------------------------------------------
# 4. Join NUM with filing metadata
# ------------------------------------------------------------

financial_facts <- num_data %>%
  
  inner_join(
    annual_filings,
    by = "adsh"
  )

cat(
  "Annual financial facts:",
  nrow(financial_facts),
  "\n"
)

# ------------------------------------------------------------
# 5. Keep consolidated facts
# ------------------------------------------------------------

financial_facts <- financial_facts %>%
  
  filter(
    is.na(segments) | segments == "",
    is.na(coreg) | coreg == ""
  )

# ------------------------------------------------------------
# 6. Standardized XBRL tag mapping
# ------------------------------------------------------------

tag_mapping <- tribble(
  
  ~variable, ~tag, ~fact_type,
  
  "revenue",
  "RevenueFromContractWithCustomerExcludingAssessedTax",
  "flow",
  
  "net_income",
  "ProfitLoss",
  "flow",
  
  "operating_income",
  "OperatingIncomeLoss",
  "flow",
  
  "total_assets",
  "Assets",
  "instant",
  
  "total_liabilities",
  "Liabilities",
  "instant",
  
  "total_equity",
  "StockholdersEquity",
  "instant",
  
  "operating_cash_flow",
  "NetCashProvidedByUsedInOperatingActivities",
  "flow",
  
  "investing_cash_flow",
  "NetCashProvidedByUsedInInvestingActivities",
  "flow",
  
  "financing_cash_flow",
  "NetCashProvidedByUsedInFinancingActivities",
  "flow",
  
  "rd_expense",
  "ResearchAndDevelopmentExpense",
  "flow"
)

# ------------------------------------------------------------
# 7. Extract annual flow variables
# ------------------------------------------------------------

flow_mapping <- tag_mapping %>%
  filter(fact_type == "flow")

annual_flows <- financial_facts %>%
  
  filter(
    tag %in% flow_mapping$tag,
    qtrs == 4
  ) %>%
  
  select(
    adsh,
    cik,
    name,
    sic,
    fy,
    period,
    filed,
    tag,
    value
  )

# ------------------------------------------------------------
# 8. Extract balance sheet variables
# ------------------------------------------------------------

instant_mapping <- tag_mapping %>%
  filter(fact_type == "instant")

balance_sheet <- financial_facts %>%
  
  filter(
    tag %in% instant_mapping$tag,
    qtrs == 0
  ) %>%
  
  select(
    adsh,
    cik,
    name,
    sic,
    fy,
    period,
    filed,
    tag,
    ddate,
    value
  )

# ------------------------------------------------------------
# 9. Remove duplicate facts
# ------------------------------------------------------------

annual_flows <- annual_flows %>%
  
  arrange(
    cik,
    fy,
    tag,
    desc(filed)
  ) %>%
  
  distinct(
    cik,
    fy,
    tag,
    .keep_all = TRUE
  )

balance_sheet <- balance_sheet %>%
  
  arrange(
    cik,
    fy,
    tag,
    desc(ddate),
    desc(filed)
  ) %>%
  
  distinct(
    cik,
    fy,
    tag,
    .keep_all = TRUE
  )

# ------------------------------------------------------------
# 10. Convert flow data to wide format
# ------------------------------------------------------------

flow_wide <- annual_flows %>%
  
  left_join(
    flow_mapping,
    by = "tag"
  ) %>%
  
  select(
    cik,
    name,
    sic,
    fy,
    variable,
    value
  ) %>%
  
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# ------------------------------------------------------------
# 11. Convert balance sheet to wide format
# ------------------------------------------------------------

balance_wide <- balance_sheet %>%
  
  left_join(
    instant_mapping,
    by = "tag"
  ) %>%
  
  select(
    cik,
    fy,
    variable,
    value
  ) %>%
  
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# ------------------------------------------------------------
# 12. Combine financial data
# ------------------------------------------------------------

financial_dataset <- flow_wide %>%
  
  left_join(
    balance_wide,
    by = c(
      "cik",
      "fy"
    )
  )

# ------------------------------------------------------------
# 13. Standardize column names
# ------------------------------------------------------------

financial_dataset <- financial_dataset %>%
  
  rename(
    company_id = cik,
    company_name = name,
    year = fy
  )

# ------------------------------------------------------------
# 14. Create sector placeholder
# ------------------------------------------------------------

financial_dataset <- financial_dataset %>%
  
  mutate(
    sector = NA_character_
  )

# ------------------------------------------------------------
# 15. Revenue growth
# ------------------------------------------------------------

financial_dataset <- financial_dataset %>%
  
  arrange(
    company_id,
    year
  ) %>%
  
  group_by(company_id) %>%
  
  mutate(
    
    revenue_growth =
      (revenue - lag(revenue)) /
      abs(lag(revenue)) * 100
    
  ) %>%
  
  ungroup()

# ------------------------------------------------------------
# 16. Financial ratios
# ------------------------------------------------------------

financial_dataset <- financial_dataset %>%
  
  mutate(
    
    net_profit_margin =
      net_income / revenue * 100,
    
    operating_margin =
      operating_income / revenue * 100,
    
    debt_to_equity =
      total_liabilities /
      total_equity,
    
    roa =
      net_income /
      total_assets * 100,
    
    roe =
      net_income /
      total_equity * 100
    
  )

# ------------------------------------------------------------
# 17. Current ratio
# ------------------------------------------------------------

financial_dataset$current_ratio <- NA_real_

# Current assets/current liabilities
# will be added after validating those XBRL tags.

# ------------------------------------------------------------
# 18. Employees
# ------------------------------------------------------------

financial_dataset$employees <- NA_real_

# Employee extraction will be added after
# validating EntityNumberOfEmployees coverage.

# ------------------------------------------------------------
# 19. High-growth target
# ------------------------------------------------------------

financial_dataset <- financial_dataset %>%
  
  group_by(company_id) %>%
  
  arrange(year) %>%
  
  mutate(
    
    next_year_growth =
      lead(revenue_growth),
    
    high_growth_next_year =
      case_when(
        
        is.na(next_year_growth) ~ NA_integer_,
        
        next_year_growth >= 10 ~ 1L,
        
        TRUE ~ 0L
        
      )
    
  ) %>%
  
  ungroup()

# ------------------------------------------------------------
# 20. Keep project columns
# ------------------------------------------------------------

financial_dataset <- financial_dataset %>%
  
  select(
    company_id,
    company_name,
    year,
    sector,
    revenue,
    net_income,
    operating_income,
    total_assets,
    total_liabilities,
    total_equity,
    operating_cash_flow,
    investing_cash_flow,
    financing_cash_flow,
    rd_expense,
    employees,
    revenue_growth,
    net_profit_margin,
    operating_margin,
    debt_to_equity,
    current_ratio,
    roa,
    roe,
    high_growth_next_year
  )

# ------------------------------------------------------------
# 21. Basic quality checks
# ------------------------------------------------------------

cat("\n============================================\n")
cat("DATASET QUALITY CHECK\n")
cat("============================================\n\n")

cat(
  "Rows:",
  nrow(financial_dataset),
  "\n"
)

cat(
  "Companies:",
  n_distinct(financial_dataset$company_id),
  "\n"
)

cat(
  "Years:",
  min(financial_dataset$year, na.rm = TRUE),
  "to",
  max(financial_dataset$year, na.rm = TRUE),
  "\n"
)

cat("\nMissing values:\n\n")

missing_summary <- financial_dataset %>%
  
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_count"
  )

print(missing_summary)

# ------------------------------------------------------------
# 22. Target distribution
# ------------------------------------------------------------

cat("\n============================================\n")
cat("TARGET DISTRIBUTION\n")
cat("============================================\n\n")

print(
  table(
    financial_dataset$high_growth_next_year,
    useNA = "ifany"
  )
)

# ------------------------------------------------------------
# 23. Save dataset
# ------------------------------------------------------------

output_file <- file.path(
  processed_path,
  "financial_dataset_initial.csv"
)

write_csv(
  financial_dataset,
  output_file
)

cat("\n============================================\n")
cat("PHASE 2E COMPLETED\n")
cat("============================================\n\n")

cat(
  "Dataset saved to:\n",
  output_file,
  "\n"
)