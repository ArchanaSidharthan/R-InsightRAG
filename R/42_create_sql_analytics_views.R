
# ============================================================
# PHASE 4J: CREATE SQL ANALYTICS VIEWS
# Project: R-InsightRAG
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(readr)

root <- getwd()

db_path <- file.path(
  root, "data", "processed",
  "r_insightrag.db"
)

output_dir <- file.path(root, "outputs")

if (!file.exists(db_path)) {
  stop(
    "SQLite database not found: ", db_path,
    "\nRun Phase 4B first."
  )
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

con <- dbConnect(
  RSQLite::SQLite(),
  db_path
)

tryCatch({
  
  # ----------------------------------------------------------
  # 1. CONFIRM SOURCE TABLE
  # ----------------------------------------------------------
  
  tables <- dbListTables(con)
  
  if (!("financial_analytics" %in% tables)) {
    stop("Table financial_analytics was not found.")
  }
  
  source_count <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM financial_analytics"
  )$n[1]
  
  cat("\n========================================\n")
  cat("PHASE 4J: SQL ANALYTICS VIEWS\n")
  cat("========================================\n")
  cat("Source rows:", source_count, "\n")
  
  # ----------------------------------------------------------
  # 2. REVENUE TREND VIEW
  # ----------------------------------------------------------
  
  dbExecute(con, "DROP VIEW IF EXISTS v_revenue_trends")
  
  dbExecute(con, "
    CREATE VIEW v_revenue_trends AS
    SELECT
      year,
      COUNT(*) AS company_year_records,
      COUNT(revenue) AS companies_with_revenue,
      SUM(revenue) AS total_reported_revenue,
      AVG(revenue) AS mean_revenue,
      AVG(revenue_growth) AS mean_revenue_growth
    FROM financial_analytics
    GROUP BY year
  ")
  
  # ----------------------------------------------------------
  # 3. LATEST-YEAR COMPANY REVENUE VIEW
  # ----------------------------------------------------------
  
  dbExecute(
    con,
    "DROP VIEW IF EXISTS v_latest_company_revenue"
  )
  
  dbExecute(con, "
    CREATE VIEW v_latest_company_revenue AS
    SELECT
      company_id,
      company_name,
      year,
      revenue,
      revenue_growth
    FROM financial_analytics
    WHERE year = (
      SELECT MAX(year)
      FROM financial_analytics
    )
  ")
  
  # ----------------------------------------------------------
  # 4. PROFITABILITY VIEW
  # ----------------------------------------------------------
  
  dbExecute(
    con,
    "DROP VIEW IF EXISTS v_profitability_metrics"
  )
  
  dbExecute(con, "
    CREATE VIEW v_profitability_metrics AS
    SELECT
      company_id,
      company_name,
      year,
      revenue,
      operating_income,
      net_income,
      operating_margin,
      net_profit_margin
    FROM financial_analytics
  ")
  
  # ----------------------------------------------------------
  # 5. LIQUIDITY AND LEVERAGE VIEW
  # ----------------------------------------------------------
  
  dbExecute(
    con,
    "DROP VIEW IF EXISTS v_liquidity_leverage"
  )
  
  dbExecute(con, "
    CREATE VIEW v_liquidity_leverage AS
    SELECT
      company_id,
      company_name,
      year,
      current_assets,
      current_liabilities,
      current_ratio,
      total_assets,
      total_liabilities,
      total_equity,
      liabilities_to_assets,
      equity_to_assets
    FROM financial_analytics
  ")
  
  # ----------------------------------------------------------
  # 6. CASH FLOW VIEW
  # ----------------------------------------------------------
  
  dbExecute(
    con,
    "DROP VIEW IF EXISTS v_cash_flow_metrics"
  )
  
  dbExecute(con, "
    CREATE VIEW v_cash_flow_metrics AS
    SELECT
      company_id,
      company_name,
      year,
      operating_cash_flow,
      investing_cash_flow,
      financing_cash_flow,
      operating_cash_flow_to_assets,
      investing_cash_flow_to_assets,
      financing_cash_flow_to_assets
    FROM financial_analytics
  ")
  
  # ----------------------------------------------------------
  # 7. REVENUE GROWTH SCREEN
  # ----------------------------------------------------------
  
  dbExecute(
    con,
    "DROP VIEW IF EXISTS v_revenue_growth_screen"
  )
  
  dbExecute(con, "
    CREATE VIEW v_revenue_growth_screen AS
    SELECT
      company_id,
      company_name,
      year,
      revenue,
      previous_revenue,
      revenue_growth,
      CASE
        WHEN revenue_growth >= 0.10 THEN 'Growth >= 10%'
        WHEN revenue_growth >= 0 THEN 'Growth 0% to <10%'
        WHEN revenue_growth < 0 THEN 'Revenue decline'
        ELSE 'Growth unavailable'
      END AS growth_category
    FROM financial_analytics
  ")
  
  # ----------------------------------------------------------
  # 8. COMPANY-YEAR RECORD QUALITY VIEW
  # ----------------------------------------------------------
  
  dbExecute(
    con,
    "DROP VIEW IF EXISTS v_company_year_quality"
  )
  
  dbExecute(con, "
    CREATE VIEW v_company_year_quality AS
    SELECT
      company_id,
      company_name,
      year,
      CASE
        WHEN revenue IS NULL THEN 'Missing revenue'
        WHEN revenue <= 0 THEN 'Nonpositive revenue'
        WHEN revenue < 1000000 THEN 'Small revenue'
        ELSE 'Revenue available'
      END AS revenue_quality,
      CASE
        WHEN revenue_growth > 1
          OR revenue_growth < -0.5
        THEN 1
        ELSE 0
      END AS extreme_growth_flag
    FROM financial_analytics
  ")
  
  # ----------------------------------------------------------
  # 9. VALIDATE VIEWS
  # ----------------------------------------------------------
  
  view_names <- c(
    "v_revenue_trends",
    "v_latest_company_revenue",
    "v_profitability_metrics",
    "v_liquidity_leverage",
    "v_cash_flow_metrics",
    "v_revenue_growth_screen",
    "v_company_year_quality"
  )
  
  view_validation <- bind_rows(
    lapply(view_names, function(view_name) {
      
      query <- paste0(
        "SELECT COUNT(*) AS n FROM ",
        dbQuoteIdentifier(con, view_name)
      )
      
      n <- dbGetQuery(con, query)$n[1]
      
      data.frame(
        view_name = view_name,
        row_count = n,
        status = if (n >= 0) "OK" else "CHECK"
      )
    })
  )
  
  cat("\n--- View validation ---\n")
  print(as.data.frame(view_validation), row.names = FALSE)
  
  # ----------------------------------------------------------
  # 10. RUN BUSINESS KPI QUERIES
  # ----------------------------------------------------------
  
  revenue_trends <- dbGetQuery(con, "
    SELECT *
    FROM v_revenue_trends
    ORDER BY year
  ")
  
  top_revenue <- dbGetQuery(con, "
    SELECT
      company_name,
      year,
      revenue,
      revenue_growth
    FROM v_latest_company_revenue
    WHERE revenue IS NOT NULL
    ORDER BY revenue DESC
    LIMIT 10
  ")
  
  growth_categories <- dbGetQuery(con, "
    SELECT
      year,
      growth_category,
      COUNT(*) AS company_count
    FROM v_revenue_growth_screen
    GROUP BY year, growth_category
    ORDER BY year, growth_category
  ")
  
  quality_summary <- dbGetQuery(con, "
    SELECT
      revenue_quality,
      COUNT(*) AS record_count
    FROM v_company_year_quality
    GROUP BY revenue_quality
    ORDER BY record_count DESC
  ")
  
  # ----------------------------------------------------------
  # 11. SAVE OUTPUTS
  # ----------------------------------------------------------
  
  write_csv(
    view_validation,
    file.path(output_dir, "phase4j_sql_view_validation.csv")
  )
  
  write_csv(
    revenue_trends,
    file.path(output_dir, "phase4j_revenue_trends.csv")
  )
  
  write_csv(
    top_revenue,
    file.path(output_dir, "phase4j_top_revenue_companies.csv")
  )
  
  write_csv(
    growth_categories,
    file.path(output_dir, "phase4j_growth_categories.csv")
  )
  
  write_csv(
    quality_summary,
    file.path(output_dir, "phase4j_revenue_quality_summary.csv")
  )
  
  # ----------------------------------------------------------
  # 12. PRINT RESULTS
  # ----------------------------------------------------------
  
  cat("\n--- Revenue trends ---\n")
  print(as.data.frame(revenue_trends), row.names = FALSE)
  
  cat("\n--- Top 10 companies by latest-year revenue ---\n")
  print(as.data.frame(top_revenue), row.names = FALSE)
  
  cat("\n--- Growth categories ---\n")
  print(as.data.frame(growth_categories), row.names = FALSE)
  
  cat("\n--- Revenue quality summary ---\n")
  print(as.data.frame(quality_summary), row.names = FALSE)
  
  cat("\n--- Saved outputs ---\n")
  cat("outputs/phase4j_sql_view_validation.csv\n")
  cat("outputs/phase4j_revenue_trends.csv\n")
  cat("outputs/phase4j_top_revenue_companies.csv\n")
  cat("outputs/phase4j_growth_categories.csv\n")
  cat("outputs/phase4j_revenue_quality_summary.csv\n")
  
  cat("\nPHASE 4J COMPLETED SUCCESSFULLY\n")
  
}, finally = {
  dbDisconnect(con)
})
