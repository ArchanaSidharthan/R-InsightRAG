
# ============================================================
# R-InsightRAG
# PHASE 4C: VALIDATE SQL ANALYTICS
# ============================================================

cat("=========================================\n")
cat("PHASE 4C: SQL ANALYTICS VALIDATION\n")
cat("=========================================\n\n")

# 1. Load required packages
required_packages <- c("DBI", "RSQLite", "dplyr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(DBI)
library(RSQLite)

# 2. Locate the project root
project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

db_path <- file.path(
  project_root,
  "data", "processed", "r_insightrag.db"
)

if (!file.exists(db_path)) {
  stop(
    "Database not found: ", db_path,
    "\nOpen the R-InsightRAG project root and run again."
  )
}

# 3. Connect to SQLite
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = db_path
)

if (!DBI::dbIsValid(con)) {
  stop("SQLite connection could not be established.")
}

cat("SQLite connection established successfully.\n")

# 4. Confirm database table
tables <- dbListTables(con)

cat("Tables found:\n")
print(tables)

if (!"financial_analytics" %in% tables) {
  stop("Expected table 'financial_analytics' was not found.")
}

# 5. Check overall database integrity
integrity <- dbGetQuery(
  con,
  "PRAGMA integrity_check;"
)

cat("\nDatabase integrity:\n")
print(integrity)

# 6. Check records and companies
overview <- dbGetQuery(
  con,
  "
  SELECT
      COUNT(*) AS total_records,
      COUNT(DISTINCT company_id) AS total_companies,
      MIN(year) AS first_year,
      MAX(year) AS last_year
  FROM financial_analytics;
  "
)

cat("\nDatabase overview:\n")
print(overview)

# 7. Check duplicate company-year keys
duplicates <- dbGetQuery(
  con,
  "
  SELECT company_id, year, COUNT(*) AS row_count
  FROM financial_analytics
  GROUP BY company_id, year
  HAVING COUNT(*) > 1;
  "
)

cat("\nDuplicate company-year keys:\n")
if (nrow(duplicates) == 0) {
  cat("None found.\n")
} else {
  print(duplicates)
}

# 8. Revenue by year
revenue_by_year <- dbGetQuery(
  con,
  "
  SELECT
      year,
      COUNT(*) AS company_records,
      COUNT(revenue) AS companies_with_revenue,
      ROUND(SUM(revenue), 2) AS total_revenue,
      ROUND(AVG(revenue), 2) AS average_revenue
  FROM financial_analytics
  GROUP BY year
  ORDER BY year;
  "
)

cat("\nRevenue analytics by year:\n")
print(revenue_by_year)

# 9. Top companies in the latest available year
latest_year <- dbGetQuery(
  con,
  "SELECT MAX(year) AS latest_year FROM financial_analytics;"
)$latest_year[1]

top_companies <- dbGetQuery(
  con,
  "
  SELECT company_id, company_name, year, revenue
  FROM financial_analytics
  WHERE year = ?
    AND revenue IS NOT NULL
  ORDER BY revenue DESC
  LIMIT 10;
  ",
  params = list(latest_year)
)

cat("\nTop 10 companies by revenue in year ", latest_year, ":\n", sep = "")
print(top_companies)

# 10. Profitability analysis
profitability <- dbGetQuery(
  con,
  "
  SELECT
      year,
      COUNT(operating_margin) AS available_margin_records,
      ROUND(AVG(operating_margin), 4) AS average_operating_margin,
      ROUND(AVG(net_profit_margin), 4) AS average_net_profit_margin
  FROM financial_analytics
  GROUP BY year
  ORDER BY year;
  "
)

cat("\nProfitability analysis:\n")
print(profitability)

# 11. Liquidity analysis
liquidity <- dbGetQuery(
  con,
  "
  SELECT
      year,
      COUNT(current_ratio) AS available_current_ratios,
      ROUND(AVG(current_ratio), 4) AS average_current_ratio
  FROM financial_analytics
  GROUP BY year
  ORDER BY year;
  "
)

cat("\nLiquidity analysis:\n")
print(liquidity)

# 12. Cash-flow analysis
cash_flow <- dbGetQuery(
  con,
  "
  SELECT
      year,
      COUNT(operating_cash_flow) AS available_ocf_records,
      ROUND(AVG(operating_cash_flow), 2) AS average_operating_cash_flow,
      ROUND(AVG(operating_cash_flow_to_assets), 4)
          AS average_ocf_to_assets
  FROM financial_analytics
  GROUP BY year
  ORDER BY year;
  "
)

cat("\nCash-flow analysis:\n")
print(cash_flow)

# 13. Screen for strong revenue growth
growth_screen <- dbGetQuery(
  con,
  "
  SELECT
      company_id,
      company_name,
      year,
      revenue,
      revenue_growth
  FROM financial_analytics
  WHERE revenue_growth >= 0.10
  ORDER BY revenue_growth DESC
  LIMIT 20;
  "
)

cat("\nTop 20 company-years with revenue growth >= 10%:\n")
print(growth_screen)

# 14. Save validation outputs
output_dir <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write.csv(
  revenue_by_year,
  file.path(output_dir, "phase4c_revenue_by_year.csv"),
  row.names = FALSE
)

write.csv(
  top_companies,
  file.path(output_dir, "phase4c_top_revenue_companies.csv"),
  row.names = FALSE
)

write.csv(
  profitability,
  file.path(output_dir, "phase4c_profitability.csv"),
  row.names = FALSE
)

write.csv(
  liquidity,
  file.path(output_dir, "phase4c_liquidity.csv"),
  row.names = FALSE
)

write.csv(
  cash_flow,
  file.path(output_dir, "phase4c_cash_flow.csv"),
  row.names = FALSE
)

write.csv(
  growth_screen,
  file.path(output_dir, "phase4c_revenue_growth_screen.csv"),
  row.names = FALSE
)

# 15. Final validation checks
stopifnot(overview$total_records == 1424)
stopifnot(overview$total_companies == 178)
stopifnot(nrow(duplicates) == 0)
stopifnot(identical(as.character(integrity[[1]][1]), "ok"))

cat("\n=========================================\n")
cat("PHASE 4C COMPLETED SUCCESSFULLY\n")
cat("Database integrity: OK\n")
cat("Duplicate company-year keys: 0\n")
cat("Validation outputs saved in outputs/.\n")
cat("=========================================\n")

if (DBI::dbIsValid(con)) {
  DBI::dbDisconnect(con)
  cat("SQLite connection closed successfully.\n")
}
