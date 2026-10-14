
# ============================================================
# R-InsightRAG - Phase 2J
# Select Companies with Historical Filing Coverage
# ============================================================

library(tidyverse)

project_path <- getwd()
raw_path <- file.path(project_path, "data", "raw")
output_path <- file.path(project_path, "data", "processed")

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

years <- 2018:2025
filing_list <- list()

for (yr in years) {
  
  sec_path <- file.path(raw_path, paste0("sec_", yr, "q4"))
  sub_file <- file.path(sec_path, "sub.txt")
  
  if (!file.exists(sub_file)) {
    warning("Missing SUB file for archive ", yr)
    next
  }
  
  sub_data <- read.delim(
    sub_file,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = ""
  )
  
  annual <- sub_data %>%
    filter(
      form == "10-K",
      fp == "FY",
      !is.na(cik),
      !is.na(fy)
    ) %>%
    transmute(
      archive_year = yr,
      company_id = as.character(cik),
      company_name = name,
      fiscal_year = as.integer(fy),
      filing_date = filed,
      report_period = period
    ) %>%
    filter(
      fiscal_year >= 2018,
      fiscal_year <= 2025
    ) %>%
    distinct(company_id, fiscal_year, .keep_all = TRUE)
  
  filing_list[[as.character(yr)]] <- annual
}

all_filings <- bind_rows(filing_list)

# A company can have multiple archive appearances.
# Count distinct fiscal years, not archive appearances.
company_coverage <- all_filings %>%
  group_by(company_id) %>%
  summarise(
    company_name = first(company_name),
    years_available = n_distinct(fiscal_year),
    first_fiscal_year = min(fiscal_year),
    last_fiscal_year = max(fiscal_year),
    fiscal_years = paste(
      sort(unique(fiscal_year)),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(years_available), company_name)

write_csv(
  company_coverage,
  file.path(output_path, "company_fiscal_year_coverage.csv")
)

# Initial screening: at least 5 distinct fiscal years
eligible_companies <- company_coverage %>%
  filter(years_available >= 5)

write_csv(
  eligible_companies,
  file.path(output_path, "eligible_companies_5plus_years.csv")
)

cat("\n===== COVERAGE SUMMARY =====\n")
print(
  company_coverage %>%
    count(years_available, name = "companies") %>%
    arrange(desc(years_available))
)

cat("\nCompanies with at least 5 fiscal years:\n")
cat(nrow(eligible_companies), "\n")

cat("\nCoverage files saved under data/processed/.\n")
