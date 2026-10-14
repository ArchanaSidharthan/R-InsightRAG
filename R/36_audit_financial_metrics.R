
# ============================================================
# R-InsightRAG
# PHASE 4D: FINANCIAL METRIC QUALITY AUDIT
# ============================================================

library(DBI)
library(RSQLite)

cat("=========================================\n")
cat("PHASE 4D: FINANCIAL METRIC QUALITY AUDIT\n")
cat("=========================================\n")

db_path <- file.path(
  getwd(),
  "data", "processed", "r_insightrag.db"
)

stopifnot(file.exists(db_path))

con <- dbConnect(SQLite(), db_path)

tryCatch({
  
  # 1. Inspect database columns
  cat("\n--- Database columns ---\n")
  print(dbGetQuery(
    con,
    "PRAGMA table_info(financial_analytics);"
  ))
  
  # 2. Check distribution of profitability ratios
  cat("\n--- Profitability ratio distribution ---\n")
  print(dbGetQuery(
    con,
    "
    SELECT
      COUNT(operating_margin) AS n_operating_margin,
      MIN(operating_margin) AS min_operating_margin,
      MAX(operating_margin) AS max_operating_margin,
      AVG(operating_margin) AS mean_operating_margin,
      COUNT(net_profit_margin) AS n_net_profit_margin,
      MIN(net_profit_margin) AS min_net_profit_margin,
      MAX(net_profit_margin) AS max_net_profit_margin,
      AVG(net_profit_margin) AS mean_net_profit_margin
    FROM financial_analytics;
    "
  ))
  
  # 3. Inspect extreme operating margins
  cat("\n--- 10 most negative operating margins ---\n")
  print(dbGetQuery(
    con,
    "
    SELECT company_name, year, revenue,
           operating_income, operating_margin
    FROM financial_analytics
    WHERE operating_margin IS NOT NULL
    ORDER BY operating_margin ASC
    LIMIT 10;
    "
  ))
  
  # 4. Inspect extreme net profit margins
  cat("\n--- 10 most negative net profit margins ---\n")
  print(dbGetQuery(
    con,
    "
    SELECT company_name, year, revenue,
           net_income, net_profit_margin
    FROM financial_analytics
    WHERE net_profit_margin IS NOT NULL
    ORDER BY net_profit_margin ASC
    LIMIT 10;
    "
  ))
  
  # 5. Verify whether margins equal income divided by revenue
  cat("\n--- Margin formula spot check ---\n")
  print(dbGetQuery(
    con,
    "
    SELECT
      company_name,
      year,
      revenue,
      operating_income,
      operating_margin,
      CASE
        WHEN revenue > 0 AND operating_income IS NOT NULL
        THEN operating_income / revenue
        ELSE NULL
      END AS recalculated_operating_margin,
      net_income,
      net_profit_margin,
      CASE
        WHEN revenue > 0 AND net_income IS NOT NULL
        THEN net_income / revenue
        ELSE NULL
      END AS recalculated_net_profit_margin
    FROM financial_analytics
    WHERE revenue > 0
      AND (operating_margin IS NOT NULL
           OR net_profit_margin IS NOT NULL)
    LIMIT 15;
    "
  ))
  
  # 6. Inspect largest positive growth rates
  cat("\n--- 10 largest revenue growth rates ---\n")
  print(dbGetQuery(
    con,
    "
    SELECT company_name, year, revenue, revenue_growth
    FROM financial_analytics
    WHERE revenue_growth IS NOT NULL
    ORDER BY revenue_growth DESC
    LIMIT 10;
    "
  ))
  
  cat("\nPHASE 4D AUDIT QUERIES COMPLETED\n")
  
}, finally = {
  if (dbIsValid(con)) {
    dbDisconnect(con)
  }
})
