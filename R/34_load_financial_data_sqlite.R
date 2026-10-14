
# ============================================================
# R-InsightRAG
# PHASE 4B: LOAD FINANCIAL ANALYTICS INTO SQLITE
# ============================================================

cat("\n=========================================\n")
cat("PHASE 4B: SQLITE FINANCIAL DATABASE\n")
cat("=========================================\n")

if (!requireNamespace("RSQLite", quietly = TRUE)) {
  stop("Install RSQLite first: install.packages('RSQLite')")
}

library(DBI)
library(RSQLite)

input_file <-
  "data/processed/financial_analytics_company_year.csv"

if (!file.exists(input_file)) {
  stop(paste("Dataset not found:", input_file))
}

financial <- read.csv(
  input_file,
  stringsAsFactors = FALSE
)

required_columns <- c(
  "company_id", "company_name", "year", "revenue",
  "net_income", "operating_income", "total_assets",
  "total_liabilities", "total_equity", "current_assets",
  "current_liabilities", "operating_cash_flow",
  "investing_cash_flow", "financing_cash_flow",
  "revenue_growth", "operating_margin",
  "net_profit_margin", "current_ratio",
  "liabilities_to_assets", "equity_to_assets"
)

missing_columns <- setdiff(required_columns, names(financial))

if (length(missing_columns) > 0) {
  stop(paste(
    "Missing columns:",
    paste(missing_columns, collapse = ", ")
  ))
}

if (anyDuplicated(financial[c("company_id", "year")])) {
  stop("Duplicate company-year records detected.")
}

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

database_path <- "data/processed/r_insightrag.db"

# Create or replace this project's local database
if (file.exists(database_path)) {
  file.remove(database_path)
}

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database_path
)

tryCatch({
  
  DBI::dbWriteTable(
    con,
    "financial_analytics",
    financial,
    overwrite = TRUE,
    row.names = FALSE
  )
  
  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX idx_financial_company_year",
      "ON financial_analytics(company_id, year)"
    )
  )
  
  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX idx_financial_year",
      "ON financial_analytics(year)"
    )
  )
  
  row_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS rows FROM financial_analytics"
  )
  
  company_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(DISTINCT company_id) AS companies
     FROM financial_analytics"
  )
  
  cat("\nRows loaded:", row_count$rows[1], "\n")
  cat("Companies:", company_count$companies[1], "\n")
  
  # Save useful SQL queries for later reuse
  query_dir <- "sql"
  dir.create(query_dir, showWarnings = FALSE)
  
  queries <- list(
    
    "01_revenue_by_company.sql" = "
SELECT company_id, company_name, year, revenue, revenue_growth
FROM financial_analytics
WHERE revenue IS NOT NULL
ORDER BY company_name, year;
",
    
    "02_top_revenue_companies_latest_year.sql" = "
SELECT company_id, company_name, year, revenue
FROM financial_analytics
WHERE year = (SELECT MAX(year) FROM financial_analytics)
  AND revenue IS NOT NULL
ORDER BY revenue DESC
LIMIT 20;
",
    
    "03_profitability_analysis.sql" = "
SELECT company_id, company_name, year,
       revenue, operating_income, net_income,
       operating_margin, net_profit_margin
FROM financial_analytics
WHERE revenue IS NOT NULL
ORDER BY company_name, year;
",
    
    "04_liquidity_analysis.sql" = "
SELECT company_id, company_name, year,
       current_assets, current_liabilities, current_ratio
FROM financial_analytics
WHERE current_ratio IS NOT NULL
ORDER BY current_ratio DESC;
",
    
    "05_cash_flow_analysis.sql" = "
SELECT company_id, company_name, year,
       operating_cash_flow, investing_cash_flow,
       financing_cash_flow
FROM financial_analytics
ORDER BY company_name, year;
",
    
    "06_revenue_growth_screen.sql" = "
SELECT company_id, company_name, year,
       revenue, revenue_growth
FROM financial_analytics
WHERE revenue_growth >= 0.10
ORDER BY revenue_growth DESC;
",
    
    "07_company_year_record_counts.sql" = "
SELECT year, COUNT(*) AS company_records,
       COUNT(revenue) AS records_with_revenue,
       COUNT(net_income) AS records_with_net_income
FROM financial_analytics
GROUP BY year
ORDER BY year;
"
  )
  
  for (filename in names(queries)) {
    writeLines(
      trimws(queries[[filename]]),
      file.path(query_dir, filename)
    )
  }
  
  # Test one query
  sample_result <- DBI::dbGetQuery(
    con,
    "SELECT year, COUNT(*) AS records
     FROM financial_analytics
     GROUP BY year
     ORDER BY year"
  )
  
  cat("\n--- Records by year ---\n")
  print(sample_result, row.names = FALSE)
  
  cat("\nSQL query files created in sql/.\n")
  
}, finally = {
  DBI::dbDisconnect(con)
})

cat("\nDatabase saved: data/processed/r_insightrag.db\n")
cat("PHASE 4B COMPLETED\n")
cat("=========================================\n")
