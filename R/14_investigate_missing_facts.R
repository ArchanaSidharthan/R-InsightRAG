
# ============================================================
# PROJECT: R-InsightRAG
# PHASE 2N: INVESTIGATE MISSING FINANCIAL FACTS
# File: R/14_investigate_missing_facts.R
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. PROJECT PATHS
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

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

company_file <- file.path(
  processed_dir,
  "financial_facts_company_year.csv"
)

long_file <- file.path(
  processed_dir,
  "financial_facts_long.csv"
)

all_units_file <- file.path(
  processed_dir,
  "financial_facts_all_units.csv"
)

# ------------------------------------------------------------
# 2. CHECK INPUT FILES
# ------------------------------------------------------------

if (!file.exists(company_file)) {
  stop("Missing file: ", company_file)
}

if (!file.exists(long_file)) {
  stop(
    "Missing file: ", long_file,
    "\nCheck that Phase 2L extraction completed."
  )
}

if (!file.exists(all_units_file)) {
  stop(
    "Missing file: ", all_units_file,
    "\nCheck that Phase 2L extraction completed."
  )
}

# ------------------------------------------------------------
# 3. LOAD DATASETS
# ------------------------------------------------------------

company_year <- read_csv(
  company_file,
  show_col_types = FALSE
)

facts_long <- read_csv(
  long_file,
  show_col_types = FALSE
)

facts_all_units <- read_csv(
  all_units_file,
  show_col_types = FALSE
)

cat("\n")
cat("====================================================\n")
cat("R-INSIGHTRAG: PHASE 2N\n")
cat("INVESTIGATING MISSING FINANCIAL FACTS\n")
cat("====================================================\n")

cat("\nCompany-year dataset dimensions:\n")
cat(nrow(company_year), "rows |",
    ncol(company_year), "columns\n")

cat("\nLong facts dataset dimensions:\n")
cat(nrow(facts_long), "rows |",
    ncol(facts_long), "columns\n")

cat("\nAll-units facts dataset dimensions:\n")
cat(nrow(facts_all_units), "rows |",
    ncol(facts_all_units), "columns\n")

# ------------------------------------------------------------
# 4. INSPECT COLUMN NAMES
# ------------------------------------------------------------

cat("\n--- Company-year columns ---\n")
print(names(company_year))

cat("\n--- Long facts columns ---\n")
print(names(facts_long))

cat("\n--- All-units facts columns ---\n")
print(names(facts_all_units))

# ------------------------------------------------------------
# 5. STANDARDIZE COMPANY-YEAR KEYS
# ------------------------------------------------------------

# SEC CIK identifiers may be numeric in one file and
# character strings in another. Convert to character.

if (!"company_id" %in% names(company_year)) {
  stop("company_id column not found in company-year dataset.")
}

if (!"year" %in% names(company_year)) {
  stop("year column not found in company-year dataset.")
}

company_year <- company_year %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  )

# ------------------------------------------------------------
# 6. MISSING VALUE SUMMARY
# ------------------------------------------------------------

cat("\n--- Missing financial values ---\n")

missing_summary <- company_year %>%
  summarise(
    total_rows = n(),
    missing_revenue = sum(is.na(revenue)),
    missing_net_income = sum(is.na(net_income)),
    missing_operating_income = sum(is.na(operating_income)),
    missing_total_assets = sum(is.na(total_assets)),
    missing_total_liabilities = sum(is.na(total_liabilities))
  )

print(missing_summary)

write_csv(
  missing_summary,
  file.path(output_dir, "phase2n_missing_summary.csv")
)

# ------------------------------------------------------------
# 7. IDENTIFY COMPANY-YEARS WITH MISSING REVENUE
# ------------------------------------------------------------

cat("\n--- Company-years with missing revenue ---\n")

missing_revenue_rows <- company_year %>%
  filter(is.na(revenue)) %>%
  select(
    company_id,
    year,
    company_name,
    revenue,
    net_income
  ) %>%
  arrange(year, company_name)

cat("Rows with missing revenue:",
    nrow(missing_revenue_rows), "\n")

print(head(missing_revenue_rows, 20), n = 20)

write_csv(
  missing_revenue_rows,
  file.path(output_dir, "phase2n_missing_revenue_rows.csv")
)

# ------------------------------------------------------------
# 8. IDENTIFY COMPANY-YEARS WITH MISSING NET INCOME
# ------------------------------------------------------------

cat("\n--- Company-years with missing net income ---\n")

missing_net_income_rows <- company_year %>%
  filter(is.na(net_income)) %>%
  select(
    company_id,
    year,
    company_name,
    revenue,
    net_income
  ) %>%
  arrange(year, company_name)

cat("Rows with missing net income:",
    nrow(missing_net_income_rows), "\n")

print(head(missing_net_income_rows, 20), n = 20)

write_csv(
  missing_net_income_rows,
  file.path(output_dir, "phase2n_missing_net_income_rows.csv")
)

# ------------------------------------------------------------
# 9. HELPER: FIND A COLUMN USING POSSIBLE NAMES
# ------------------------------------------------------------

find_column <- function(data, candidates) {
  matches <- candidates[candidates %in% names(data)]
  
  if (length(matches) == 0) {
    return(NA_character_)
  }
  
  matches[[1]]
}

# ------------------------------------------------------------
# 10. STANDARDIZE FACT DATA COLUMN NAMES
# ------------------------------------------------------------

# The extraction may use different names for year, tag,
# company ID, value, unit, and reporting date.
# Detect common alternatives instead of assuming one schema.

id_col <- find_column(
  facts_long,
  c("company_id", "cik")
)

year_col <- find_column(
  facts_long,
  c("year", "fiscal_year", "fy")
)

variable_col <- find_column(
  facts_long,
  c("variable", "financial_variable", "metric")
)

tag_col <- find_column(
  facts_long,
  c("tag", "tag_name")
)

value_col <- find_column(
  facts_long,
  c("value", "fact_value", "amount")
)

unit_col <- find_column(
  facts_long,
  c("uom", "unit", "units")
)

date_col <- find_column(
  facts_long,
  c("ddate", "period", "end_date")
)

qtrs_col <- find_column(
  facts_long,
  c("qtrs", "quarters")
)

# ------------------------------------------------------------
# 11. CHECK WHETHER LONG FACTS HAS REQUIRED COLUMNS
# ------------------------------------------------------------

cat("\n--- Detected long-facts columns ---\n")

detected_columns <- tibble(
  purpose = c(
    "Company identifier",
    "Fiscal year",
    "Financial variable",
    "XBRL tag",
    "Fact value",
    "Unit",
    "Reporting date",
    "Quarter duration"
  ),
  detected_column = c(
    id_col,
    year_col,
    variable_col,
    tag_col,
    value_col,
    unit_col,
    date_col,
    qtrs_col
  )
)

print(detected_columns, n = Inf)

write_csv(
  detected_columns,
  file.path(output_dir, "phase2n_detected_columns.csv")
)

if (is.na(variable_col) || is.na(tag_col)) {
  cat(
    "\nWARNING: The long facts file does not contain ",
    "a recognizable variable or tag column.\n",
    "Review the printed column names before proceeding.\n",
    sep = ""
  )
}

# ------------------------------------------------------------
# 12. INVESTIGATE TAG COVERAGE
# ------------------------------------------------------------

if (!is.na(variable_col) && !is.na(tag_col)) {
  
  cat("\n--- Available tags by financial variable ---\n")
  
  tag_coverage <- facts_long %>%
    filter(
      !is.na(.data[[variable_col]]),
      !is.na(.data[[tag_col]])
    ) %>%
    count(
      .data[[variable_col]],
      .data[[tag_col]],
      name = "fact_rows"
    ) %>%
    arrange(
      .data[[variable_col]],
      desc(fact_rows)
    )
  
  print(tag_coverage, n = 100)
  
  write_csv(
    tag_coverage,
    file.path(output_dir, "phase2n_tag_coverage.csv")
  )
}

# ------------------------------------------------------------
# 13. INVESTIGATE UNIT COVERAGE
# ------------------------------------------------------------

cat("\n--- Units in long facts ---\n")

if (!is.na(unit_col)) {
  
  unit_summary <- facts_long %>%
    count(
      .data[[unit_col]],
      name = "fact_rows"
    ) %>%
    arrange(desc(fact_rows))
  
  print(unit_summary, n = Inf)
  
  write_csv(
    unit_summary,
    file.path(output_dir, "phase2n_unit_summary.csv")
  )
  
} else {
  cat("No recognizable unit column found in long facts.\n")
}

# Inspect the all-units dataset independently
all_units_unit_col <- find_column(
  facts_all_units,
  c("uom", "unit", "units")
)

cat("\n--- Units in all-units facts ---\n")

if (!is.na(all_units_unit_col)) {
  
  all_units_summary <- facts_all_units %>%
    count(
      .data[[all_units_unit_col]],
      name = "fact_rows"
    ) %>%
    arrange(desc(fact_rows))
  
  print(all_units_summary, n = Inf)
  
  write_csv(
    all_units_summary,
    file.path(output_dir, "phase2n_all_units_summary.csv")
  )
  
} else {
  cat("No recognizable unit column found in all-units facts.\n")
}

# ------------------------------------------------------------
# 14. INSPECT QUARTER-DURATION COVERAGE
# ------------------------------------------------------------

cat("\n--- Quarter-duration values ---\n")

if (!is.na(qtrs_col)) {
  
  qtrs_summary <- facts_long %>%
    count(
      .data[[qtrs_col]],
      name = "fact_rows"
    ) %>%
    arrange(.data[[qtrs_col]])
  
  print(qtrs_summary, n = Inf)
  
  write_csv(
    qtrs_summary,
    file.path(output_dir, "phase2n_qtrs_summary.csv")
  )
  
} else {
  cat("No recognizable qtrs column found in long facts.\n")
}

# ------------------------------------------------------------
# 15. INSPECT REPORTING DATE COVERAGE
# ------------------------------------------------------------

cat("\n--- Reporting date inspection ---\n")

if (!is.na(date_col)) {
  
  date_summary <- facts_long %>%
    summarise(
      total_fact_rows = n(),
      missing_dates = sum(is.na(.data[[date_col]])),
      distinct_dates = n_distinct(.data[[date_col]])
    )
  
  print(date_summary)
  
  sample_dates <- facts_long %>%
    select(all_of(date_col)) %>%
    filter(!is.na(.data[[date_col]])) %>%
    distinct() %>%
    slice_head(n = 30)
  
  cat("\nSample reporting dates:\n")
  print(sample_dates, n = 30)
  
  write_csv(
    sample_dates,
    file.path(output_dir, "phase2n_sample_dates.csv")
  )
  
} else {
  cat("No recognizable reporting-date column found.\n")
}

# ------------------------------------------------------------
# 16. CHECK NET INCOME AND REVENUE TAG NAMES
# ------------------------------------------------------------

cat("\n--- Revenue and net-income tag candidates ---\n")

if (!is.na(tag_col)) {
  
  candidate_tags <- facts_long %>%
    filter(
      str_detect(
        str_to_lower(.data[[tag_col]]),
        "revenue|salesrevenue|profitloss|netincome|income"
      )
    )
  
  cat(
    "Matching fact rows:",
    nrow(candidate_tags),
    "\n"
  )
  
  print(
    candidate_tags %>%
      count(
        .data[[tag_col]],
        name = "fact_rows"
      ) %>%
      arrange(desc(fact_rows)),
    n = 100
  )
  
  write_csv(
    candidate_tags,
    file.path(output_dir, "phase2n_candidate_financial_facts.csv")
  )
  
} else {
  cat("Cannot inspect tag candidates: tag column not detected.\n")
}

# ------------------------------------------------------------
# 17. SAVE A SMALL SAMPLE OF RAW EXTRACTED FACTS
# ------------------------------------------------------------

cat("\n--- Sample of long facts ---\n")

print(
  facts_long %>% slice_head(n = 20),
  width = Inf
)

write_csv(
  facts_long %>% slice_head(n = 100),
  file.path(output_dir, "phase2n_sample_long_facts.csv")
)

cat("\n--- Sample of all-units facts ---\n")

print(
  facts_all_units %>% slice_head(n = 10),
  width = Inf
)

# ------------------------------------------------------------
# 18. FINAL STATUS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("PHASE 2N INVESTIGATION COMPLETED\n")
cat("====================================================\n")

cat("\nOutput folder:\n")
cat(output_dir, "\n")

cat("\nImportant outputs:\n")
cat("1. phase2n_missing_summary.csv\n")
cat("2. phase2n_missing_revenue_rows.csv\n")
cat("3. phase2n_missing_net_income_rows.csv\n")
cat("4. phase2n_detected_columns.csv\n")
cat("5. phase2n_tag_coverage.csv (if tags are detected)\n")
cat("6. phase2n_unit_summary.csv (if units are detected)\n")
cat("7. phase2n_qtrs_summary.csv (if qtrs is detected)\n")
cat("8. phase2n_candidate_financial_facts.csv (if tags are detected)\n")

cat(
  "\nThis script only investigates and exports reports.\n",
  "It does not overwrite the original financial dataset.\n",
  sep = ""
)

cat("====================================================\n")
