
# ============================================================
# R-InsightRAG
# PHASE 2Q: REVENUE ANOMALY AND PARSING REVIEW
# File: R/17_review_revenue_anomalies.R
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

original_file <- file.path(
  processed_dir,
  "financial_facts_company_year.csv"
)

improved_file <- file.path(
  processed_dir,
  "financial_facts_company_year_improved.csv"
)

facts_file <- file.path(
  output_dir,
  "phase2o_selected_facts.csv"
)

input_files <- c(
  original = original_file,
  improved = improved_file,
  selected_facts = facts_file
)

missing_files <- input_files[!file.exists(input_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing input files:\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 2. LOAD FILES AND CHECK PARSING PROBLEMS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("PHASE 2Q: REVENUE ANOMALY REVIEW\n")
cat("====================================================\n")

read_and_check <- function(path, label) {
  
  cat("\n--- Reading:", label, "---\n")
  
  dat <- read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "unique"
  )
  
  parse_issues <- problems(dat)
  
  cat("Rows:", nrow(dat), "\n")
  cat("Columns:", ncol(dat), "\n")
  cat("Parsing issues:", nrow(parse_issues), "\n")
  
  if (nrow(parse_issues) > 0) {
    print(head(parse_issues, 20), n = 20)
    
    issue_file <- file.path(
      output_dir,
      paste0("phase2q_parse_issues_", label, ".csv")
    )
    
    write_csv(parse_issues, issue_file)
    cat("Parsing report:", issue_file, "\n")
  }
  
  dat
}

original <- read_and_check(
  original_file, "original"
)

improved <- read_and_check(
  improved_file, "improved"
)

selected_facts <- read_and_check(
  facts_file, "selected_facts"
)

# ------------------------------------------------------------
# 3. STANDARDIZE IDENTIFIERS AND YEARS
# ------------------------------------------------------------

standardize_keys <- function(dat) {
  
  required <- c("company_id", "year", "revenue")
  
  absent <- setdiff(required, names(dat))
  
  if (length(absent) > 0) {
    stop(
      "Required columns missing: ",
      paste(absent, collapse = ", ")
    )
  }
  
  dat %>%
    mutate(
      company_id = str_remove(
        str_trim(as.character(company_id)),
        "^0+"
      ),
      company_id = if_else(
        company_id == "",
        "0",
        company_id
      ),
      year = suppressWarnings(as.integer(year)),
      revenue = suppressWarnings(as.numeric(revenue))
    )
}

original <- standardize_keys(original)
improved <- standardize_keys(improved)

# ------------------------------------------------------------
# 4. CALCULATE YEAR-ON-YEAR REVENUE GROWTH
# ------------------------------------------------------------

revenue_history <- improved %>%
  arrange(company_id, year) %>%
  group_by(company_id) %>%
  mutate(
    previous_year = lag(year),
    previous_revenue = lag(revenue),
    
    revenue_growth = case_when(
      is.na(year) ~ NA_real_,
      is.na(previous_year) ~ NA_real_,
      year != previous_year + 1L ~ NA_real_,
      is.na(revenue) ~ NA_real_,
      is.na(previous_revenue) ~ NA_real_,
      previous_revenue <= 0 ~ NA_real_,
      TRUE ~ revenue / previous_revenue - 1
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 5. FLAG UNUSUAL REVENUE GROWTH
# ------------------------------------------------------------

# These are investigation thresholds, not automatic error rules.
# Growth over 100% or below -50% may be legitimate.

unusual_growth <- revenue_history %>%
  filter(
    !is.na(revenue_growth),
    revenue_growth > 1 |
      revenue_growth < -0.5
  ) %>%
  mutate(
    review_reason = case_when(
      revenue_growth > 1 ~
        "Revenue growth above 100%",
      revenue_growth < -0.5 ~
        "Revenue decline greater than 50%",
      TRUE ~ "Review"
    )
  ) %>%
  select(
    company_id,
    company_name,
    year,
    previous_year,
    previous_revenue,
    revenue,
    revenue_growth,
    review_reason
  ) %>%
  arrange(desc(abs(revenue_growth)))

cat("\n--- Unusual revenue growth ---\n")
cat("Flagged rows:", nrow(unusual_growth), "\n")
print(head(unusual_growth, 40), n = 40)

write_csv(
  unusual_growth,
  file.path(output_dir, "phase2q_unusual_growth.csv")
)

# ------------------------------------------------------------
# 6. CHECK ZERO AND NEGATIVE REVENUE
# ------------------------------------------------------------

cat("\n--- Zero and negative revenue ---\n")

revenue_edge_cases <- improved %>%
  filter(
    !is.na(revenue),
    revenue <= 0
  ) %>%
  select(
    company_id,
    year,
    company_name,
    revenue
  ) %>%
  arrange(year, company_name)

cat("Rows with zero or negative revenue:",
    nrow(revenue_edge_cases), "\n")

print(revenue_edge_cases, n = Inf)

write_csv(
  revenue_edge_cases,
  file.path(output_dir, "phase2q_nonpositive_revenue.csv")
)

# ------------------------------------------------------------
# 7. COMPARE ORIGINAL AND IMPROVED REVENUE
# ------------------------------------------------------------

cat("\n--- Original vs improved revenue ---\n")

revenue_comparison <- original %>%
  select(
    company_id,
    year,
    company_name,
    original_revenue = revenue
  ) %>%
  inner_join(
    improved %>%
      select(
        company_id,
        year,
        improved_revenue = revenue
      ),
    by = c("company_id", "year")
  ) %>%
  mutate(
    original_missing = is.na(original_revenue),
    improved_missing = is.na(improved_revenue),
    
    newly_available =
      original_missing & !improved_missing,
    
    became_missing =
      !original_missing & improved_missing,
    
    both_available =
      !original_missing & !improved_missing,
    
    absolute_difference = if_else(
      both_available,
      abs(improved_revenue - original_revenue),
      NA_real_
    ),
    
    relative_difference = if_else(
      both_available & original_revenue != 0,
      (improved_revenue - original_revenue) /
        abs(original_revenue),
      NA_real_
    )
  )

cat(
  "Newly available revenue rows:",
  sum(revenue_comparison$newly_available),
  "\n"
)

cat(
  "Rows that became missing:",
  sum(revenue_comparison$became_missing),
  "\n"
)

changed_revenue <- revenue_comparison %>%
  filter(
    both_available,
    absolute_difference >
      pmax(1, abs(original_revenue) * 1e-8)
  ) %>%
  arrange(desc(absolute_difference))

cat(
  "Rows where both datasets have different revenue:",
  nrow(changed_revenue),
  "\n"
)

print(head(changed_revenue, 30), n = 30)

write_csv(
  revenue_comparison,
  file.path(output_dir, "phase2q_revenue_comparison.csv")
)

write_csv(
  changed_revenue,
  file.path(output_dir, "phase2q_changed_revenue.csv")
)

# ------------------------------------------------------------
# 8. LINK UNUSUAL REVENUE TO SELECTED SEC FACTS
# ------------------------------------------------------------

cat("\n--- Selected SEC facts for unusual growth rows ---\n")

required_fact_columns <- c(
  "company_id",
  "fiscal_year",
  "variable",
  "tag",
  "priority",
  "ddate",
  "qtrs",
  "uom",
  "value"
)

if (all(required_fact_columns %in% names(selected_facts))) {
  
  selected_facts <- selected_facts %>%
    mutate(
      company_id = str_remove(
        str_trim(as.character(company_id)),
        "^0+"
      ),
      company_id = if_else(
        company_id == "",
        "0",
        company_id
      ),
      fiscal_year = as.integer(fiscal_year)
    )
  
  unusual_fact_details <- selected_facts %>%
    filter(
      variable == "revenue"
    ) %>%
    inner_join(
      unusual_growth %>%
        select(company_id, year, revenue_growth),
      by = c(
        "company_id" = "company_id",
        "fiscal_year" = "year"
      )
    ) %>%
    select(
      company_id,
      company_name,
      fiscal_year,
      revenue_growth,
      tag,
      priority,
      ddate,
      qtrs,
      uom,
      value,
      adsh
    ) %>%
    arrange(desc(abs(revenue_growth)))
  
  print(head(unusual_fact_details, 40), n = 40)
  
  write_csv(
    unusual_fact_details,
    file.path(output_dir, "phase2q_unusual_growth_fact_details.csv")
  )
  
} else {
  
  cat(
    "Selected-facts schema differs from expected columns.\n",
    "Review the printed column names and parsing report.\n",
    sep = ""
  )
}

# ------------------------------------------------------------
# 9. SUMMARIZE GROWTH DISTRIBUTION
# ------------------------------------------------------------

cat("\n--- Revenue growth distribution ---\n")

growth_summary <- revenue_history %>%
  summarise(
    valid_growth_rows = sum(!is.na(revenue_growth)),
    median_growth = if (
      all(is.na(revenue_growth))
    ) NA_real_ else median(
      revenue_growth, na.rm = TRUE
    ),
    minimum_growth = if (
      all(is.na(revenue_growth))
    ) NA_real_ else min(
      revenue_growth, na.rm = TRUE
    ),
    maximum_growth = if (
      all(is.na(revenue_growth))
    ) NA_real_ else max(
      revenue_growth, na.rm = TRUE
    ),
    growth_over_100pct = sum(
      revenue_growth > 1, na.rm = TRUE
    ),
    growth_below_minus_50pct = sum(
      revenue_growth < -0.5, na.rm = TRUE
    )
  )

print(growth_summary)

write_csv(
  growth_summary,
  file.path(output_dir, "phase2q_growth_summary.csv")
)

# ------------------------------------------------------------
# 10. FINAL STATUS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("PHASE 2Q REVIEW COMPLETED\n")
cat("====================================================\n")

cat("Reports saved in:\n")
cat(output_dir, "\n")

cat("\nReview these files:\n")
cat("1. phase2q_unusual_growth.csv\n")
cat("2. phase2q_nonpositive_revenue.csv\n")
cat("3. phase2q_changed_revenue.csv\n")
cat("4. phase2q_unusual_growth_fact_details.csv\n")
cat("5. phase2q_parse_issues_original.csv, if created\n")
cat("6. phase2q_parse_issues_improved.csv, if created\n")
cat("7. phase2q_parse_issues_selected_facts.csv, if created\n")

cat(
  "\nNo observations were deleted or imputed.\n",
  "Review the reports before training models.\n",
  sep = ""
)

cat("====================================================\n")
