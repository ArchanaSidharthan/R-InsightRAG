
# ============================================================
# R-InsightRAG
# PHASE 4E: FINANCIAL OUTLIER INVESTIGATION
# ============================================================

library(DBI)
library(RSQLite)

cat("=========================================\n")
cat("PHASE 4E: FINANCIAL OUTLIER INVESTIGATION\n")
cat("=========================================\n")

# 1. Locate database and output directory
project_root <- getwd()

db_path <- file.path(
  project_root,
  "data", "processed", "r_insightrag.db"
)

output_dir <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(db_path)) {
  stop("Database not found: ", db_path)
}

# 2. Connect to database
con <- dbConnect(SQLite(), db_path)

tryCatch({
  
  # 3. Read relevant fields
  financial_data <- dbGetQuery(
    con,
    "
    SELECT
      company_id,
      company_name,
      year,
      revenue,
      previous_year,
      previous_revenue,
      revenue_growth,
      operating_income,
      operating_margin,
      net_income,
      net_profit_margin,
      total_assets,
      operating_cash_flow,
      operating_cash_flow_to_assets
    FROM financial_analytics
    ORDER BY company_id, year;
    "
  )
  
  cat("\nRows inspected:", nrow(financial_data), "\n")
  
  # 4. Create review flags
  # These are investigation thresholds, not automatic error rules.
  
  financial_data$flag_missing_revenue <-
    is.na(financial_data$revenue)
  
  financial_data$flag_nonpositive_revenue <-
    !is.na(financial_data$revenue) &
    financial_data$revenue <= 0
  
  financial_data$flag_low_revenue <-
    !is.na(financial_data$revenue) &
    financial_data$revenue > 0 &
    financial_data$revenue < 1000000
  
  financial_data$flag_growth_over_100pct <-
    !is.na(financial_data$revenue_growth) &
    financial_data$revenue_growth > 1
  
  financial_data$flag_growth_below_minus_50pct <-
    !is.na(financial_data$revenue_growth) &
    financial_data$revenue_growth < -0.5
  
  financial_data$flag_operating_margin_extreme <-
    !is.na(financial_data$operating_margin) &
    abs(financial_data$operating_margin) > 1
  
  financial_data$flag_net_margin_extreme <-
    !is.na(financial_data$net_profit_margin) &
    abs(financial_data$net_profit_margin) > 1
  
  financial_data$flag_ocf_asset_ratio_extreme <-
    !is.na(financial_data$operating_cash_flow_to_assets) &
    abs(financial_data$operating_cash_flow_to_assets) > 1
  
  # 5. Build a combined list of records needing review
  financial_data$review_reasons <- vapply(
    seq_len(nrow(financial_data)),
    function(i) {
      reasons <- character(0)
      
      if (financial_data$flag_missing_revenue[i]) {
        reasons <- c(reasons, "Missing revenue")
      }
      
      if (financial_data$flag_nonpositive_revenue[i]) {
        reasons <- c(reasons, "Nonpositive revenue")
      }
      
      if (financial_data$flag_low_revenue[i]) {
        reasons <- c(reasons, "Revenue below $1 million")
      }
      
      if (financial_data$flag_growth_over_100pct[i]) {
        reasons <- c(reasons, "Revenue growth above 100%")
      }
      
      if (financial_data$flag_growth_below_minus_50pct[i]) {
        reasons <- c(reasons, "Revenue growth below -50%")
      }
      
      if (financial_data$flag_operating_margin_extreme[i]) {
        reasons <- c(reasons, "Operating margin magnitude above 100%")
      }
      
      if (financial_data$flag_net_margin_extreme[i]) {
        reasons <- c(reasons, "Net profit margin magnitude above 100%")
      }
      
      if (financial_data$flag_ocf_asset_ratio_extreme[i]) {
        reasons <- c(reasons, "Operating cash flow/assets magnitude above 100%")
      }
      
      paste(reasons, collapse = "; ")
    },
    character(1)
  )
  
  review_data <- financial_data[
    nzchar(financial_data$review_reasons),
  ]
  
  # 6. Save complete flagged records
  review_path <- file.path(
    output_dir,
    "phase4e_financial_outlier_review.csv"
  )
  
  write.csv(
    review_data,
    review_path,
    row.names = FALSE,
    na = ""
  )
  
  # 7. Create summary counts
  flag_names <- c(
    "Missing revenue",
    "Nonpositive revenue",
    "Revenue below $1 million",
    "Revenue growth above 100%",
    "Revenue growth below -50%",
    "Operating margin magnitude above 100%",
    "Net profit margin magnitude above 100%",
    "Operating cash flow/assets magnitude above 100%"
  )
  
  flag_counts <- data.frame(
    flag = flag_names,
    records = c(
      sum(financial_data$flag_missing_revenue),
      sum(financial_data$flag_nonpositive_revenue),
      sum(financial_data$flag_low_revenue),
      sum(financial_data$flag_growth_over_100pct),
      sum(financial_data$flag_growth_below_minus_50pct),
      sum(financial_data$flag_operating_margin_extreme),
      sum(financial_data$flag_net_margin_extreme),
      sum(financial_data$flag_ocf_asset_ratio_extreme)
    )
  )
  
  write.csv(
    flag_counts,
    file.path(output_dir, "phase4e_outlier_flag_summary.csv"),
    row.names = FALSE
  )
  
  cat("\n--- Flag counts ---\n")
  print(flag_counts, row.names = FALSE)
  
  cat("\n--- Highest revenue growth rates ---\n")
  
  growth_examples <- financial_data[
    !is.na(financial_data$revenue_growth),
  ]
  
  growth_examples <- growth_examples[
    order(-growth_examples$revenue_growth),
  ]
  
  print(
    head(
      growth_examples[
        ,
        c(
          "company_name", "year", "revenue",
          "previous_year", "previous_revenue",
          "revenue_growth", "review_reasons"
        )
      ],
      10
    ),
    row.names = FALSE
  )
  
  cat("\n--- Most extreme operating margins ---\n")
  
  margin_examples <- financial_data[
    !is.na(financial_data$operating_margin),
  ]
  
  margin_examples <- margin_examples[
    order(margin_examples$operating_margin),
  ]
  
  print(
    head(
      margin_examples[
        ,
        c(
          "company_name", "year", "revenue",
          "operating_income", "operating_margin"
        )
      ],
      10
    ),
    row.names = FALSE
  )
  
  cat("\nUnique company-year rows inspected:", nrow(financial_data), "\n")
  cat("Rows flagged for review:", nrow(review_data), "\n")
  
  cat("\nReview file saved:\n", review_path, "\n", sep = "")
  cat("Summary saved: outputs/phase4e_outlier_flag_summary.csv\n")
  
  cat("\nPHASE 4E COMPLETED\n")
  cat("No source records were deleted or modified.\n")
  
}, finally = {
  if (dbIsValid(con)) {
    dbDisconnect(con)
  }
})
