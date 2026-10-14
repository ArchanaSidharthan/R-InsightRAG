
# ============================================================
# R-InsightRAG - Phase 2L
# SEC Historical Financial Fact Extraction
# Years: 2018-2025
# Initial cohort: 178 companies
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. PROJECT PATHS
# ------------------------------------------------------------

project_path <- getwd()

raw_path <- file.path(project_path, "data", "raw")
processed_path <- file.path(project_path, "data", "processed")

cohort_file <- file.path(
  processed_path,
  "initial_company_cohort_8years.csv"
)

dir.create(
  processed_path,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(cohort_file)) {
  stop(
    "Company cohort file not found. ",
    "Run R/11_select_initial_cohort.R first."
  )
}

# ------------------------------------------------------------
# 2. LOAD SELECTED COMPANY COHORT
# ------------------------------------------------------------

cohort <- read_csv(
  cohort_file,
  show_col_types = FALSE
) %>%
  mutate(company_id = as.character(company_id))

if (anyDuplicated(cohort$company_id) > 0) {
  stop("Duplicate company IDs found in the cohort.")
}

cat("Companies in cohort:", nrow(cohort), "\n")

# ------------------------------------------------------------
# 3. FINANCIAL TAG MAPPING
# ------------------------------------------------------------
# Revenue uses alternative standard tags.
# Tag priority is a fallback preference, not a guarantee
# that all alternatives have equivalent coverage.
# ------------------------------------------------------------

tag_map <- tribble(
  ~variable, ~tag, ~priority, ~fact_type,
  
  "revenue",
  "RevenueFromContractWithCustomerExcludingAssessedTax",
  1L, "flow",
  
  "revenue",
  "SalesRevenueNet",
  2L, "flow",
  
  "revenue",
  "Revenues",
  3L, "flow",
  
  "net_income",
  "ProfitLoss",
  1L, "flow",
  
  "operating_income",
  "OperatingIncomeLoss",
  1L, "flow",
  
  "total_assets",
  "Assets",
  1L, "instant",
  
  "total_liabilities",
  "Liabilities",
  1L, "instant",
  
  "total_equity",
  "StockholdersEquity",
  1L, "instant",
  
  "operating_cash_flow",
  "NetCashProvidedByUsedInOperatingActivities",
  1L, "flow",
  
  "investing_cash_flow",
  "NetCashProvidedByUsedInInvestingActivities",
  1L, "flow",
  
  "financing_cash_flow",
  "NetCashProvidedByUsedInFinancingActivities",
  1L, "flow",
  
  "rd_expense",
  "ResearchAndDevelopmentExpense",
  1L, "flow",
  
  "current_assets",
  "AssetsCurrent",
  1L, "instant",
  
  "current_liabilities",
  "LiabilitiesCurrent",
  1L, "instant"
)

required_tags <- unique(tag_map$tag)

# ------------------------------------------------------------
# 4. COLLECT ANNUAL FILING METADATA
# ------------------------------------------------------------

filing_list <- list()

for (archive_year in 2018:2025) {
  
  cat("\nInspecting filing metadata:", archive_year, "\n")
  
  sub_file <- file.path(
    raw_path,
    paste0("sec_", archive_year, "q4"),
    "sub.txt"
  )
  
  if (!file.exists(sub_file)) {
    warning("Missing SUB file for archive ", archive_year)
    next
  }
  
  sub_data <- read.delim(
    sub_file,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = "",
    colClasses = "character"
  )
  
  annual <- sub_data %>%
    mutate(
      company_id = as.character(cik),
      fiscal_year = suppressWarnings(as.integer(fy)),
      filed = as.character(filed),
      period = as.character(period)
    ) %>%
    filter(
      company_id %in% cohort$company_id,
      form == "10-K",
      fp == "FY",
      fiscal_year >= 2018,
      fiscal_year <= 2025
    ) %>%
    transmute(
      archive_year = as.integer(archive_year),
      adsh = as.character(adsh),
      company_id,
      company_name = as.character(name),
      fiscal_year,
      filed,
      period
    )
  
  filing_list[[as.character(archive_year)]] <- annual
  
  cat("Matching annual filings:", nrow(annual), "\n")
}

all_filings <- bind_rows(filing_list)

if (nrow(all_filings) == 0) {
  stop("No matching annual filings were found.")
}

# ------------------------------------------------------------
# 5. DEDUPLICATE FILING METADATA
# ------------------------------------------------------------
# Prefer the latest original 10-K filed for each company-year.
# Amendments are excluded because only form == 10-K is retained.
# ------------------------------------------------------------

selected_filings <- all_filings %>%
  arrange(company_id, fiscal_year, desc(filed)) %>%
  group_by(company_id, fiscal_year) %>%
  slice(1) %>%
  ungroup()

cat("\nSelected filings:", nrow(selected_filings), "\n")
cat(
  "Companies represented:",
  n_distinct(selected_filings$company_id),
  "\n"
)

write_csv(
  selected_filings,
  file.path(processed_path, "selected_annual_filings.csv")
)

# ------------------------------------------------------------
# 6. EXTRACT FINANCIAL FACTS YEAR BY YEAR
# ------------------------------------------------------------

fact_list <- list()

for (archive_year in 2018:2025) {
  
  cat("\n====================================\n")
  cat("Processing NUM:", archive_year, "\n")
  cat("====================================\n")
  
  filings_this_archive <- selected_filings %>%
    filter(.data$archive_year == !!archive_year)
  
  if (nrow(filings_this_archive) == 0) {
    cat("No selected filings for this archive.\n")
    next
  }
  
  num_file <- file.path(
    raw_path,
    paste0("sec_", archive_year, "q4"),
    "num.txt"
  )
  
  if (!file.exists(num_file)) {
    warning("Missing NUM file for archive ", archive_year)
    next
  }
  
  num_data <- read.delim(
    num_file,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = "",
    colClasses = "character"
  )
  
  # Standardize data types before joining across archives.
  num_data <- num_data %>%
    mutate(
      adsh = as.character(adsh),
      tag = as.character(tag),
      ddate = as.character(ddate),
      qtrs = suppressWarnings(as.integer(qtrs)),
      value = suppressWarnings(as.numeric(value)),
      segments = as.character(segments),
      coreg = as.character(coreg),
      uom = as.character(uom)
    )
  
  facts <- num_data %>%
    filter(
      adsh %in% filings_this_archive$adsh,
      tag %in% required_tags,
      is.na(segments) | segments == "",
      is.na(coreg) | coreg == ""
    ) %>%
    inner_join(
      filings_this_archive %>%
        select(
          adsh,
          company_id,
          company_name,
          fiscal_year
        ),
      by = "adsh"
    ) %>%
    inner_join(
      tag_map,
      by = "tag"
    ) %>%
    filter(
      !is.na(value),
      (
        fact_type == "flow" &
          qtrs == 4L
      ) |
        (
          fact_type == "instant" &
            qtrs == 0L
        )
    ) %>%
    select(
      company_id,
      company_name,
      fiscal_year,
      adsh,
      variable,
      tag,
      priority,
      fact_type,
      ddate,
      qtrs,
      uom,
      value
    )
  
  fact_list[[as.character(archive_year)]] <- facts
  
  cat("Matching facts retained:", nrow(facts), "\n")
  
  rm(num_data)
  gc()
}

# ------------------------------------------------------------
# 7. COMBINE EXTRACTED FACTS
# ------------------------------------------------------------

facts_all <- bind_rows(fact_list)

if (nrow(facts_all) == 0) {
  stop(
    "No financial facts matched. ",
    "Check SEC tags, units, and filing filters."
  )
}

# Keep USD financial facts for now.
# Employee counts and shares would require separate unit handling.
facts_usd <- facts_all %>%
  filter(uom == "USD")

if (nrow(facts_usd) == 0) {
  stop("No USD financial facts were found.")
}

# ------------------------------------------------------------
# 8. RESOLVE ALTERNATIVE TAGS AND DUPLICATES
# ------------------------------------------------------------
# Prefer the lowest priority number.
# Within a tag, prefer the latest fact date.
# If the same date still has multiple values, retain one row
# deterministically and flag this limitation for later auditing.
# ------------------------------------------------------------

facts_preferred <- facts_usd %>%
  arrange(
    company_id,
    fiscal_year,
    variable,
    priority,
    desc(ddate),
    tag,
    adsh
  ) %>%
  group_by(company_id, fiscal_year, variable) %>%
  slice(1) %>%
  ungroup()

# ------------------------------------------------------------
# 9. BUILD COMPANY-YEAR DATASET
# ------------------------------------------------------------

financial_wide <- facts_preferred %>%
  select(
    company_id,
    company_name,
    fiscal_year,
    variable,
    value
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  rename(year = fiscal_year) %>%
  arrange(company_id, year)

# ------------------------------------------------------------
# 10. COVERAGE REPORT
# ------------------------------------------------------------

coverage_report <- facts_preferred %>%
  count(variable, name = "available_company_years") %>%
  arrange(desc(available_company_years))

# Complete the expected company-year grid so missing years
# are visible instead of disappearing from the output.
expected_grid <- crossing(
  company_id = cohort$company_id,
  year = 2018:2025
)

complete_panel <- expected_grid %>%
  left_join(
    financial_wide,
    by = c("company_id", "year")
  ) %>%
  left_join(
    cohort %>%
      select(company_id, cohort_company_name = company_name),
    by = "company_id"
  ) %>%
  mutate(
    company_name = coalesce(company_name, cohort_company_name)
  ) %>%
  select(-cohort_company_name) %>%
  arrange(company_id, year)

# ------------------------------------------------------------
# 11. QUALITY SUMMARY
# ------------------------------------------------------------

company_year_report <- complete_panel %>%
  summarise(
    expected_company_year_rows = n(),
    unique_companies = n_distinct(company_id),
    first_year = min(year),
    last_year = max(year),
    rows_missing_revenue = sum(is.na(revenue)),
    rows_nonpositive_revenue = sum(
      !is.na(revenue) & revenue <= 0
    ),
    rows_missing_assets = sum(is.na(total_assets)),
    rows_missing_net_income = sum(is.na(net_income)),
    duplicate_company_years = sum(
      duplicated(paste(company_id, year))
    )
  )

# ------------------------------------------------------------
# 12. SAVE OUTPUTS
# ------------------------------------------------------------

write_csv(
  facts_all,
  file.path(processed_path, "financial_facts_all_units.csv")
)

write_csv(
  facts_preferred,
  file.path(processed_path, "financial_facts_long.csv")
)

write_csv(
  complete_panel,
  file.path(processed_path, "financial_facts_company_year.csv")
)

write_csv(
  coverage_report,
  file.path(processed_path, "financial_fact_coverage.csv")
)

write_csv(
  company_year_report,
  file.path(processed_path, "financial_fact_quality_summary.csv")
)

# ------------------------------------------------------------
# 13. DISPLAY RESULTS
# ------------------------------------------------------------

cat("\n============================================\n")
cat("FINANCIAL FACT COVERAGE\n")
cat("============================================\n")

print(coverage_report)

cat("\n============================================\n")
cat("COMPANY-YEAR QUALITY SUMMARY\n")
cat("============================================\n")

print(company_year_report)

cat("\n============================================\n")
cat("PHASE 2L EXTRACTION COMPLETED\n")
cat("============================================\n")

cat("\nFiles saved in:\n", processed_path, "\n")
