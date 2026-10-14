
# ============================================================
# R-InsightRAG
# PHASE 4F: INVESTIGATE SOURCE REVENUE FACTS
# ============================================================

library(dplyr)
library(readr)

cat("=========================================\n")
cat("PHASE 4F: SOURCE REVENUE FACT INVESTIGATION\n")
cat("=========================================\n")

root <- getwd()

facts_path <- file.path(
  root, "data", "processed", "financial_facts_long.csv"
)

analytics_path <- file.path(
  root, "data", "processed", "financial_analytics_company_year.csv"
)

output_dir <- file.path(root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(facts_path)) {
  stop(
    "Could not find financial_facts_long.csv at: ",
    facts_path
  )
}

if (!file.exists(analytics_path)) {
  stop(
    "Could not find financial_analytics_company_year.csv at: ",
    analytics_path
  )
}

# 1. Read the source facts and analytics
facts <- read_csv(facts_path, show_col_types = FALSE)
analytics <- read_csv(analytics_path, show_col_types = FALSE)

required_fact_columns <- c(
  "company_id", "company_name", "fiscal_year",
  "adsh", "variable", "tag", "ddate",
  "qtrs", "uom", "value"
)

missing_columns <- setdiff(required_fact_columns, names(facts))

if (length(missing_columns) > 0) {
  stop(
    "Missing columns in financial_facts_long.csv: ",
    paste(missing_columns, collapse = ", ")
  )
}

cat("\nSource fact rows:", nrow(facts), "\n")
cat("Analytics rows:", nrow(analytics), "\n")

# 2. Companies with the largest revenue changes
focus_companies <- c(
  "ARROWHEAD PHARMACEUTICALS",
  "TIANCI INTERNATIONAL",
  "SEMILEDS",
  "HARTFORD GREAT HEALTH",
  "CONCRETE LEVELING SYSTEMS",
  "CANNABICS PHARMACEUTICALS",
  "RAFAEL HOLDINGS"
)

# 3. Filter source facts to relevant companies and years
revenue_facts <- facts %>%
  filter(
    variable == "revenue",
    grepl(
      paste(focus_companies, collapse = "|"),
      company_name,
      ignore.case = TRUE
    ),
    fiscal_year >= 2018,
    fiscal_year <= 2025
  ) %>%
  arrange(company_name, fiscal_year, tag, ddate)

if (nrow(revenue_facts) == 0) {
  stop(
    "No matching revenue facts found. ",
    "Check the company names and variable values in the source file."
  )
}

# 4. Display source tags, dates, units and accession identifiers
cat("\n--- Revenue source facts ---\n")

print(
  revenue_facts %>%
    select(
      company_id, company_name, fiscal_year,
      adsh, tag, ddate, qtrs, uom, value
    ),
  n = 100
)

# 5. Count source tags by company and fiscal year
tag_summary <- revenue_facts %>%
  group_by(company_id, company_name, fiscal_year, tag, uom, qtrs) %>%
  summarise(
    fact_count = n(),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(company_name, fiscal_year, tag)

cat("\n--- Source tag summary ---\n")
print(tag_summary, n = 100)

# 6. Save outputs for review
write_csv(
  revenue_facts,
  file.path(output_dir, "phase4f_revenue_source_facts.csv")
)

write_csv(
  tag_summary,
  file.path(output_dir, "phase4f_revenue_tag_summary.csv")
)

cat("\nFiles saved:\n")
cat("outputs/phase4f_revenue_source_facts.csv\n")
cat("outputs/phase4f_revenue_tag_summary.csv\n")
cat("\nPHASE 4F COMPLETED\n")