
# ============================================================
# R-INSIGHTRAG
# PHASE 2O: IMPROVED FINANCIAL FACT EXTRACTION
# File: R/15_improve_financial_fact_extraction.R
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

project_root <- file.path(
  "C:/Users/archa/OneDrive/Desktop/Projects",
  "R-InsightRAG"
)

raw_dir <- file.path(project_root, "data", "raw")
processed_dir <- file.path(project_root, "data", "processed")
output_dir <- file.path(project_root, "outputs")

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

cohort_file <- file.path(
  processed_dir,
  "initial_company_cohort_8years.csv"
)

if (!file.exists(cohort_file)) {
  stop("Company cohort file not found: ", cohort_file)
}

cohort <- read_csv(
  cohort_file,
  show_col_types = FALSE
)

# Detect cohort ID column
cohort_id_col <- intersect(
  c("company_id", "cik"),
  names(cohort)
)

if (length(cohort_id_col) == 0) {
  stop(
    "Cannot find company_id or cik in cohort file. ",
    "Inspect names(cohort) and update the script."
  )
}

cohort_ids <- cohort %>%
  transmute(
    company_id = str_remove(
      str_trim(as.character(.data[[cohort_id_col[1]]])),
      "^0+"
    )
  ) %>%
  mutate(
    company_id = if_else(
      company_id == "",
      "0",
      company_id
    )
  ) %>%
  distinct()

cat("\n====================================================\n")
cat("PHASE 2O: IMPROVED FACT EXTRACTION\n")
cat("====================================================\n")
cat("Companies in cohort:", nrow(cohort_ids), "\n")

# ------------------------------------------------------------
# 2. EXTENDED TAG MAP
# ------------------------------------------------------------
# These tags are candidates, not guaranteed equivalents.
# We keep a priority so the preferred standard tag is selected
# when multiple candidates exist.

tag_map <- tribble(
  ~variable,              ~tag,                                                   ~priority, ~fact_type,
  "revenue",              "RevenueFromContractWithCustomerExcludingAssessedTax",  1,         "flow",
  "revenue",              "SalesRevenueNet",                                      2,         "flow",
  "revenue",              "Revenues",                                             3,         "flow",
  "revenue",              "SalesRevenueGoodsNet",                                 4,         "flow",
  "revenue",              "SalesRevenueServicesNet",                              5,         "flow",
  "revenue",              "RegulatedAndUnregulatedOperatingRevenue",              6,         "flow",
  "net_income",           "ProfitLoss",                                           1,         "flow",
  "net_income",           "NetIncomeLoss",                                        2,         "flow",
  "net_income",           "ProfitLossAttributableToParent",                       3,         "flow",
  "net_income",           "ProfitLossIncludingPortionAttributableToNoncontrollingInterest", 4, "flow",
  "operating_income",     "OperatingIncomeLoss",                                  1,         "flow",
  "total_assets",         "Assets",                                               1,         "instant",
  "total_liabilities",    "Liabilities",                                          1,         "instant",
  "total_equity",         "StockholdersEquity",                                   1,         "instant",
  "total_equity",         "StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest", 2, "instant",
  "current_assets",       "AssetsCurrent",                                        1,         "instant",
  "current_liabilities",  "LiabilitiesCurrent",                                   1,         "instant",
  "operating_cash_flow",  "NetCashProvidedByUsedInOperatingActivities",            1,         "flow",
  "investing_cash_flow",  "NetCashProvidedByUsedInInvestingActivities",             1,         "flow",
  "financing_cash_flow",  "NetCashProvidedByUsedInFinancingActivities",            1,         "flow",
  "rd_expense",           "ResearchAndDevelopmentExpense",                        1,         "flow"
)

# ------------------------------------------------------------
# 3. READ ONE SEC ARCHIVE
# ------------------------------------------------------------

read_sec_archive <- function(archive_year) {
  
  folder <- file.path(
    raw_dir,
    paste0("sec_", archive_year, "q4")
  )
  
  sub_file <- file.path(folder, "sub.txt")
  num_file <- file.path(folder, "num.txt")
  
  if (!file.exists(sub_file) || !file.exists(num_file)) {
    warning(
      "Missing sub.txt or num.txt for archive ",
      archive_year
    )
    return(NULL)
  }
  
  cat("\nReading SEC archive:", archive_year, "\n")
  
  sub <- read_tsv(
    sub_file,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
  
  num <- read_tsv(
    num_file,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
  
  # Required SEC fields
  sub_required <- c(
    "adsh", "cik", "name", "form", "fy", "fp",
    "period", "filed"
  )
  
  num_required <- c(
    "adsh", "tag", "version", "ddate",
    "qtrs", "uom", "value", "coreg", "segments"
  )
  
  if (!all(sub_required %in% names(sub))) {
    stop(
      "Required SUB columns missing in archive ",
      archive_year, ": ",
      paste(setdiff(sub_required, names(sub)), collapse = ", ")
    )
  }
  
  if (!all(num_required %in% names(num))) {
    stop(
      "Required NUM columns missing in archive ",
      archive_year, ": ",
      paste(setdiff(num_required, names(num)), collapse = ", ")
    )
  }
  
  sub <- sub %>%
    mutate(
      cik = str_remove(str_trim(cik), "^0+"),
      cik = if_else(cik == "", "0", cik),
      fy = suppressWarnings(as.integer(fy)),
      period = as.character(period),
      filed = as.character(filed),
      archive_year = archive_year
    ) %>%
    filter(
      form == "10-K",
      fp == "FY",
      fy >= 2018,
      fy <= 2025,
      cik %in% cohort_ids$company_id
    ) %>%
    transmute(
      company_id = cik,
      company_name = name,
      fiscal_year = fy,
      adsh,
      period,
      filed,
      archive_year
    )
  
  if (nrow(sub) == 0) {
    return(NULL)
  }
  
  # Keep one filing for each company and fiscal year.
  # Prefer the latest filed original 10-K in the archive set.
  sub <- sub %>%
    arrange(company_id, fiscal_year, desc(filed)) %>%
    distinct(company_id, fiscal_year, .keep_all = TRUE)
  
  num <- num %>%
    mutate(
      ddate = as.character(ddate),
      qtrs = suppressWarnings(as.integer(qtrs)),
      value = suppressWarnings(as.numeric(value)),
      uom = as.character(uom),
      coreg = coalesce(as.character(coreg), ""),
      segments = coalesce(as.character(segments), "")
    )
  
  matched <- num %>%
    inner_join(
      sub %>% select(adsh, company_id, company_name,
                     fiscal_year, period, filed),
      by = "adsh"
    ) %>%
    inner_join(
      tag_map,
      by = "tag"
    ) %>%
    filter(
      is.na(coreg) | coreg == "",
      is.na(segments) | segments == "",
      !is.na(value)
    ) %>%
    mutate(
      # Annual flows should have four quarters.
      # Instant facts should have qtrs = 0.
      valid_duration = case_when(
        fact_type == "flow" ~ qtrs == 4,
        fact_type == "instant" ~ qtrs == 0,
        TRUE ~ FALSE
      ),
      # For annual filings, period should normally be the
      # reporting date for the fiscal year.
      matches_period = ddate == period
    ) %>%
    filter(valid_duration) %>%
    mutate(
      archive_year = archive_year
    )
  
  matched
}

# ------------------------------------------------------------
# 4. PROCESS ALL ARCHIVES
# ------------------------------------------------------------

archives <- 2018:2025
fact_list <- list()

for (yr in archives) {
  result <- read_sec_archive(yr)
  
  if (!is.null(result) && nrow(result) > 0) {
    fact_list[[as.character(yr)]] <- result
    cat(
      "Candidate facts retained for archive",
      yr, ":", nrow(result), "\n"
    )
  }
}

if (length(fact_list) == 0) {
  stop(
    "No facts were extracted. Check the SEC archive paths, ",
    "cohort IDs, and SEC column names."
  )
}

facts <- bind_rows(fact_list)

# ------------------------------------------------------------
# 5. SAVE DIAGNOSTIC FACTS
# ------------------------------------------------------------

write_csv(
  facts,
  file.path(output_dir, "phase2o_candidate_facts.csv")
)

# Report candidate units before selecting USD.
cat("\n--- Candidate facts by unit ---\n")

unit_report <- facts %>%
  count(uom, name = "fact_rows") %>%
  arrange(desc(fact_rows))

print(unit_report, n = Inf)

write_csv(
  unit_report,
  file.path(output_dir, "phase2o_unit_report.csv")
)

# This initial modeling dataset is restricted to USD.
# Non-USD observations are not converted or combined.
facts_usd <- facts %>%
  filter(uom == "USD")

# ------------------------------------------------------------
# 6. AUDIT PERIOD MATCHING
# ------------------------------------------------------------

cat("\n--- Fiscal-period date matching ---\n")

period_report <- facts_usd %>%
  group_by(variable) %>%
  summarise(
    candidate_rows = n(),
    matching_period_rows = sum(matches_period, na.rm = TRUE),
    nonmatching_period_rows = sum(!matches_period, na.rm = TRUE),
    period_match_pct = round(
      100 * mean(matches_period, na.rm = TRUE),
      2
    ),
    .groups = "drop"
  )

print(period_report, n = Inf)

write_csv(
  period_report,
  file.path(output_dir, "phase2o_period_match_report.csv")
)

# Only use facts whose ddate matches the filing's period.
# This avoids choosing comparative values from earlier periods.
facts_period_matched <- facts_usd %>%
  filter(matches_period)

# ------------------------------------------------------------
# 7. SELECT ONE FACT PER COMPANY-YEAR-VARIABLE
# ------------------------------------------------------------

selected_facts <- facts_period_matched %>%
  arrange(
    company_id,
    fiscal_year,
    variable,
    priority,
    desc(filed),
    desc(ddate)
  ) %>%
  distinct(
    company_id,
    fiscal_year,
    variable,
    .keep_all = TRUE
  )

write_csv(
  selected_facts,
  file.path(output_dir, "phase2o_selected_facts.csv")
)

# ------------------------------------------------------------
# 8. PIVOT TO COMPANY-YEAR DATASET
# ------------------------------------------------------------

financial_wide <- selected_facts %>%
  select(
    company_id,
    fiscal_year,
    company_name,
    variable,
    value
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  rename(year = fiscal_year)

# Build the expected 178 x 8 grid.
# Missing values remain NA.
company_names <- cohort %>%
  transmute(
    company_id = str_remove(
      str_trim(as.character(.data[[cohort_id_col[1]]])),
      "^0+"
    ),
    company_name = coalesce(
      as.character(company_name),
      as.character(company_id)
    )
  ) %>%
  mutate(
    company_id = if_else(company_id == "", "0", company_id)
  ) %>%
  distinct(company_id, .keep_all = TRUE)

expected_grid <- crossing(
  company_id = cohort_ids$company_id,
  year = 2018:2025
) %>%
  left_join(
    company_names,
    by = "company_id"
  )

improved_dataset <- expected_grid %>%
  left_join(
    financial_wide,
    by = c("company_id", "year")
  ) %>%
  mutate(
    company_name = coalesce(
      as.character(company_name.y),
      as.character(company_name.x)
    )
  ) %>%
  select(
    company_id,
    year,
    company_name,
    everything(),
    -any_of(c("company_name.x", "company_name.y"))
  )

# ------------------------------------------------------------
# 9. QUALITY AND COVERAGE REPORTS
# ------------------------------------------------------------

numeric_variables <- intersect(
  tag_map$variable,
  names(improved_dataset)
) %>%
  unique()

coverage_report <- tibble(
  variable = numeric_variables
) %>%
  mutate(
    available_company_years = map_int(
      variable,
      ~ sum(!is.na(improved_dataset[[.x]]))
    ),
    missing_company_years = nrow(improved_dataset) -
      available_company_years,
    coverage_pct = round(
      100 * available_company_years /
        nrow(improved_dataset),
      2
    )
  ) %>%
  arrange(desc(coverage_pct))

cat("\n--- Improved financial fact coverage ---\n")
print(coverage_report, n = Inf)

write_csv(
  coverage_report,
  file.path(output_dir, "phase2o_coverage_report.csv")
)

year_coverage <- improved_dataset %>%
  group_by(year) %>%
  summarise(
    company_year_rows = n(),
    revenue_available = if ("revenue" %in% names(.)) {
      sum(!is.na(revenue))
    } else {
      0L
    },
    net_income_available = if ("net_income" %in% names(.)) {
      sum(!is.na(net_income))
    } else {
      0L
    },
    .groups = "drop"
  ) %>%
  arrange(year)

cat("\n--- Coverage by year ---\n")
print(year_coverage, n = Inf)

write_csv(
  year_coverage,
  file.path(output_dir, "phase2o_year_coverage.csv")
)

# ------------------------------------------------------------
# 10. SAVE IMPROVED DATASET SEPARATELY
# ------------------------------------------------------------

improved_file <- file.path(
  processed_dir,
  "financial_facts_company_year_improved.csv"
)

write_csv(
  improved_dataset,
  improved_file
)

# ------------------------------------------------------------
# 11. FINAL STATUS
# ------------------------------------------------------------

cat("\n====================================================\n")
cat("PHASE 2O COMPLETED\n")
cat("====================================================\n")

cat("Companies:", n_distinct(improved_dataset$company_id), "\n")
cat("Rows:", nrow(improved_dataset), "\n")
cat("Years:", min(improved_dataset$year),
    "to", max(improved_dataset$year), "\n")

cat("\nImproved dataset saved to:\n")
cat(improved_file, "\n")

cat("\nDiagnostic reports saved to:\n")
cat(output_dir, "\n")

cat(
  "\nIMPORTANT: Compare coverage against the original dataset.\n",
  "Do not proceed to model training until period alignment, ",
  "tag semantics, and company-year coverage are validated.\n",
  sep = ""
)

cat("====================================================\n")
