
# ============================================================
# PHASE 4I: ROBUST FINANCIAL ANALYTICS
# Project: R-InsightRAG
#
# Objectives:
# 1. Validate the company-year dataset.
# 2. Flag unusual financial records without deleting them.
# 3. Create 1st/99th percentile winsorized sensitivity metrics.
# 4. Compare raw and winsorized metric distributions.
# 5. Produce yearly summaries and quality reports.
#
# IMPORTANT:
# Original financial values are never overwritten.
# Winsorized values are additional columns for analysis only.
# ============================================================

library(dplyr)
library(readr)
library(tidyr)

# ------------------------------------------------------------
# 1. PROJECT PATHS
# ------------------------------------------------------------

root <- getwd()

input_path <- file.path(
  root, "data", "processed",
  "financial_analytics_company_year.csv"
)

output_path <- file.path(
  root, "data", "processed",
  "financial_analytics_robust.csv"
)

metric_summary_path <- file.path(
  root, "outputs",
  "phase4i_metric_distribution_summary.csv"
)

year_summary_path <- file.path(
  root, "outputs",
  "phase4i_yearly_robust_summary.csv"
)

flag_summary_path <- file.path(
  root, "outputs",
  "phase4i_quality_flag_summary.csv"
)

winsor_limits_path <- file.path(
  root, "outputs",
  "phase4i_winsorization_limits.csv"
)

# ------------------------------------------------------------
# 2. CHECK INPUT FILE
# ------------------------------------------------------------

if (!file.exists(input_path)) {
  stop(
    "Input file not found: ", input_path,
    "\nPlease run Phase 4A first."
  )
}

dir.create(
  file.path(root, "outputs"),
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 3. LOAD DATA
# ------------------------------------------------------------

financial_data <- read_csv(
  input_path,
  show_col_types = FALSE
)

required_columns <- c(
  "company_id",
  "company_name",
  "year",
  "revenue",
  "net_income",
  "operating_income",
  "total_assets",
  "total_liabilities",
  "total_equity",
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

missing_columns <- setdiff(
  required_columns,
  names(financial_data)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 4. DATA INTEGRITY CHECKS
# ------------------------------------------------------------

duplicate_keys <- financial_data %>%
  count(company_id, year, name = "record_count") %>%
  filter(record_count > 1)

if (nrow(duplicate_keys) > 0) {
  print(as.data.frame(duplicate_keys), row.names = FALSE)
  
  stop(
    "Duplicate company-year records found. ",
    "Resolve duplicates before continuing."
  )
}

cat("\n========================================\n")
cat("PHASE 4I: ROBUST FINANCIAL ANALYTICS\n")
cat("========================================\n")

cat("Input rows:", nrow(financial_data), "\n")
cat(
  "Companies:",
  dplyr::n_distinct(financial_data$company_id),
  "\n"
)
cat(
  "Years:",
  min(financial_data$year, na.rm = TRUE),
  "-",
  max(financial_data$year, na.rm = TRUE),
  "\n"
)

# ------------------------------------------------------------
# 5. QUALITY FLAGS
#
# Flags are for review only. No observations are deleted.
# ------------------------------------------------------------

robust_data <- financial_data %>%
  mutate(
    flag_missing_revenue =
      is.na(revenue),
    
    flag_nonpositive_revenue =
      !is.na(revenue) & revenue <= 0,
    
    flag_small_revenue =
      !is.na(revenue) &
      revenue > 0 &
      revenue < 1000000,
    
    flag_extreme_revenue_growth =
      !is.na(revenue_growth) &
      (
        revenue_growth > 1 |
          revenue_growth < -0.5
      ),
    
    flag_extreme_operating_margin =
      !is.na(operating_margin) &
      abs(operating_margin) > 1,
    
    flag_extreme_net_profit_margin =
      !is.na(net_profit_margin) &
      abs(net_profit_margin) > 1,
    
    flag_extreme_ocf_to_assets =
      !is.na(operating_cash_flow_to_assets) &
      abs(operating_cash_flow_to_assets) > 1
  ) %>%
  mutate(
    quality_flag_count =
      as.integer(flag_missing_revenue) +
      as.integer(flag_nonpositive_revenue) +
      as.integer(flag_small_revenue) +
      as.integer(flag_extreme_revenue_growth) +
      as.integer(flag_extreme_operating_margin) +
      as.integer(flag_extreme_net_profit_margin) +
      as.integer(flag_extreme_ocf_to_assets),
    
    quality_review_required =
      quality_flag_count > 0
  )

# ------------------------------------------------------------
# 6. DEFINE METRICS FOR SENSITIVITY ANALYSIS
# ------------------------------------------------------------

metrics_to_winsorize <- c(
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

# ------------------------------------------------------------
# 7. CALCULATE WINSORIZATION LIMITS
#
# Observations below the 1st percentile or above the 99th
# percentile will be capped in additional columns.
#
# These full-dataset limits are for descriptive sensitivity
# analysis only, not machine-learning test preprocessing.
# ------------------------------------------------------------

winsor_limits <- bind_rows(
  lapply(
    metrics_to_winsorize,
    function(metric_name) {
      
      x <- robust_data[[metric_name]]
      observed <- x[is.finite(x)]
      
      if (length(observed) == 0) {
        return(
          data.frame(
            metric = metric_name,
            nonmissing_n = 0L,
            lower_1pct = NA_real_,
            upper_99pct = NA_real_
          )
        )
      }
      
      limits <- quantile(
        observed,
        probs = c(0.01, 0.99),
        na.rm = TRUE,
        names = FALSE,
        type = 7
      )
      
      data.frame(
        metric = metric_name,
        nonmissing_n = length(observed),
        lower_1pct = as.numeric(limits[1]),
        upper_99pct = as.numeric(limits[2])
      )
    }
  )
)

# ------------------------------------------------------------
# 8. CREATE ADDITIONAL WINSORIZED COLUMNS
# ------------------------------------------------------------

for (metric_name in metrics_to_winsorize) {
  
  metric_limits <- winsor_limits %>%
    filter(metric == metric_name)
  
  lower_limit <- metric_limits$lower_1pct[1]
  upper_limit <- metric_limits$upper_99pct[1]
  
  output_column <- paste0(
    metric_name,
    "_winsorized"
  )
  
  x <- robust_data[[metric_name]]
  
  if (
    length(lower_limit) == 1 &&
    length(upper_limit) == 1 &&
    is.finite(lower_limit) &&
    is.finite(upper_limit)
  ) {
    
    robust_data[[output_column]] <- ifelse(
      is.na(x),
      NA_real_,
      pmin(
        pmax(x, lower_limit),
        upper_limit
      )
    )
    
  } else {
    
    robust_data[[output_column]] <- NA_real_
    
  }
}

# ------------------------------------------------------------
# 9. METRIC DISTRIBUTION SUMMARY FUNCTION
# ------------------------------------------------------------

summarise_metric <- function(data, column_name, version_name) {
  
  x <- data[[column_name]]
  observed <- x[is.finite(x)]
  
  if (length(observed) == 0) {
    
    return(
      data.frame(
        metric = column_name,
        version = version_name,
        n = 0L,
        missing_n = length(x),
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        minimum = NA_real_,
        p01 = NA_real_,
        p25 = NA_real_,
        p75 = NA_real_,
        p99 = NA_real_,
        maximum = NA_real_
      )
    )
  }
  
  quartiles <- quantile(
    observed,
    probs = c(0.01, 0.25, 0.75, 0.99),
    names = FALSE,
    na.rm = TRUE
  )
  
  data.frame(
    metric = column_name,
    version = version_name,
    n = length(observed),
    missing_n = sum(!is.finite(x)),
    mean = mean(observed),
    median = median(observed),
    sd = if (length(observed) > 1) {
      sd(observed)
    } else {
      NA_real_
    },
    minimum = min(observed),
    p01 = quartiles[1],
    p25 = quartiles[2],
    p75 = quartiles[3],
    p99 = quartiles[4],
    maximum = max(observed)
  )
}

# ------------------------------------------------------------
# 10. RAW VS WINSORIZED DISTRIBUTION COMPARISON
# ------------------------------------------------------------

summary_rows <- list()

for (metric_name in metrics_to_winsorize) {
  
  raw_summary <- summarise_metric(
    robust_data,
    metric_name,
    "raw"
  )
  
  winsorized_name <- paste0(
    metric_name,
    "_winsorized"
  )
  
  winsorized_summary <- summarise_metric(
    robust_data,
    winsorized_name,
    "winsorized_1pct_99pct"
  )
  
  # Give both versions the same base metric name.
  raw_summary$metric <- metric_name
  winsorized_summary$metric <- metric_name
  
  summary_rows[[length(summary_rows) + 1]] <-
    raw_summary
  
  summary_rows[[length(summary_rows) + 1]] <-
    winsorized_summary
}

metric_summary <- bind_rows(summary_rows) %>%
  arrange(metric, version)

# ------------------------------------------------------------
# 11. YEARLY SUMMARY
#
# Revenue totals and means are kept raw.
# Median ratios provide a robust comparison across years.
# ------------------------------------------------------------

yearly_summary <- robust_data %>%
  group_by(year) %>%
  summarise(
    company_year_rows = n(),
    
    companies_with_revenue =
      sum(!is.na(revenue)),
    
    total_reported_revenue =
      if (all(is.na(revenue))) {
        NA_real_
      } else {
        sum(revenue, na.rm = TRUE)
      },
    
    mean_revenue =
      if (all(is.na(revenue))) {
        NA_real_
      } else {
        mean(revenue, na.rm = TRUE)
      },
    
    median_revenue =
      if (all(is.na(revenue))) {
        NA_real_
      } else {
        median(revenue, na.rm = TRUE)
      },
    
    mean_revenue_growth =
      if (all(is.na(revenue_growth))) {
        NA_real_
      } else {
        mean(revenue_growth, na.rm = TRUE)
      },
    
    median_revenue_growth =
      if (all(is.na(revenue_growth))) {
        NA_real_
      } else {
        median(revenue_growth, na.rm = TRUE)
      },
    
    median_revenue_growth_winsorized =
      if (all(is.na(revenue_growth_winsorized))) {
        NA_real_
      } else {
        median(revenue_growth_winsorized, na.rm = TRUE)
      },
    
    mean_operating_margin =
      if (all(is.na(operating_margin))) {
        NA_real_
      } else {
        mean(operating_margin, na.rm = TRUE)
      },
    
    median_operating_margin =
      if (all(is.na(operating_margin))) {
        NA_real_
      } else {
        median(operating_margin, na.rm = TRUE)
      },
    
    median_operating_margin_winsorized =
      if (all(is.na(operating_margin_winsorized))) {
        NA_real_
      } else {
        median(operating_margin_winsorized, na.rm = TRUE)
      },
    
    mean_net_profit_margin =
      if (all(is.na(net_profit_margin))) {
        NA_real_
      } else {
        mean(net_profit_margin, na.rm = TRUE)
      },
    
    median_net_profit_margin =
      if (all(is.na(net_profit_margin))) {
        NA_real_
      } else {
        median(net_profit_margin, na.rm = TRUE)
      },
    
    median_net_profit_margin_winsorized =
      if (all(is.na(net_profit_margin_winsorized))) {
        NA_real_
      } else {
        median(net_profit_margin_winsorized, na.rm = TRUE)
      },
    
    records_flagged_for_review =
      sum(quality_review_required, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(year)

# ------------------------------------------------------------
# 12. QUALITY FLAG SUMMARY
# ------------------------------------------------------------

flag_columns <- c(
  "flag_missing_revenue",
  "flag_nonpositive_revenue",
  "flag_small_revenue",
  "flag_extreme_revenue_growth",
  "flag_extreme_operating_margin",
  "flag_extreme_net_profit_margin",
  "flag_extreme_ocf_to_assets",
  "quality_review_required"
)

flag_summary <- bind_rows(
  lapply(
    flag_columns,
    function(flag_name) {
      
      flagged_count <- sum(
        robust_data[[flag_name]],
        na.rm = TRUE
      )
      
      data.frame(
        flag = flag_name,
        flagged_records = flagged_count,
        percentage_of_all_rows =
          100 * flagged_count / nrow(robust_data)
      )
    }
  )
)

# ------------------------------------------------------------
# 13. SAVE ALL OUTPUTS
# ------------------------------------------------------------

write_csv(
  robust_data,
  output_path
)

write_csv(
  metric_summary,
  metric_summary_path
)

write_csv(
  yearly_summary,
  year_summary_path
)

write_csv(
  flag_summary,
  flag_summary_path
)

write_csv(
  winsor_limits,
  winsor_limits_path
)

# ------------------------------------------------------------
# 14. PRINT RESULTS
#
# Use base data frames to avoid tibble printing options
# that may cause an invalid na.print error.
# ------------------------------------------------------------

cat("\n--- Dataset integrity ---\n")

cat("Rows:", nrow(robust_data), "\n")

cat(
  "Companies:",
  dplyr::n_distinct(robust_data$company_id),
  "\n"
)

cat(
  "Duplicate company-year keys:",
  nrow(duplicate_keys),
  "\n"
)

cat("\n--- Quality flag summary ---\n")

print(
  as.data.frame(flag_summary),
  row.names = FALSE
)

cat("\n--- Raw vs winsorized metric summary ---\n")

metric_summary_to_print <- metric_summary %>%
  select(
    metric,
    version,
    n,
    missing_n,
    mean,
    median,
    minimum,
    maximum
  )

print(
  as.data.frame(metric_summary_to_print),
  row.names = FALSE
)

cat("\n--- Yearly robust summary ---\n")

print(
  as.data.frame(yearly_summary),
  row.names = FALSE
)

cat("\n--- Winsorization limits ---\n")

print(
  as.data.frame(winsor_limits),
  row.names = FALSE
)

cat("\n--- Files saved ---\n")
cat(output_path, "\n")
cat(metric_summary_path, "\n")
cat(year_summary_path, "\n")
cat(flag_summary_path, "\n")
cat(winsor_limits_path, "\n")

cat("\nPHASE 4I COMPLETED SUCCESSFULLY\n")
