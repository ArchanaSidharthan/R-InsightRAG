
# ============================================================
# R-InsightRAG
# PHASE 4G: REVENUE FILING PROVENANCE AUDIT
# ============================================================

library(dplyr)
library(readr)

root <- getwd()

facts_path <- file.path(
  root, "data", "processed", "financial_facts_all_units.csv"
)

selected_path <- file.path(
  root, "data", "processed", "financial_facts_long.csv"
)

if (!file.exists(facts_path) || !file.exists(selected_path)) {
  stop("Required SEC fact files were not found.")
}

all_facts <- read_csv(facts_path, show_col_types = FALSE)
selected <- read_csv(selected_path, show_col_types = FALSE)

# Focus on the most extreme revenue cases
focus_ids <- c(
  879407, 1557798, 1333822, 1482554,
  1414382, 1343009, 1713863
)

revenue_tags <- c(
  "RevenueFromContractWithCustomerExcludingAssessedTax",
  "Revenues",
  "SalesRevenueNet",
  "SalesRevenueGoodsNet",
  "SalesRevenueServicesNet",
  "RegulatedAndUnregulatedOperatingRevenue"
)

# Inspect all matching revenue tags and filing identifiers
audit <- all_facts %>%
  filter(
    company_id %in% focus_ids,
    variable == "revenue",
    tag %in% revenue_tags,
    fiscal_year >= 2018,
    fiscal_year <= 2025,
    uom == "USD",
    qtrs == 4
  ) %>%
  select(
    company_id, company_name, fiscal_year,
    adsh, tag, ddate, qtrs, uom, value
  ) %>%
  distinct() %>%
  arrange(company_id, fiscal_year, tag, adsh, ddate)

cat("Source revenue facts found:", nrow(audit), "\n")

# Compare source candidates with selected long facts
selected_revenue <- selected %>%
  filter(
    company_id %in% focus_ids,
    variable == "revenue",
    fiscal_year >= 2018,
    fiscal_year <= 2025
  ) %>%
  select(
    company_id, fiscal_year,
    selected_adsh = adsh,
    selected_tag = tag,
    selected_ddate = ddate,
    selected_value = value
  ) %>%
  distinct()

audit <- audit %>%
  left_join(
    selected_revenue,
    by = c("company_id", "fiscal_year")
  ) %>%
  mutate(
    matches_selected_value =
      !is.na(selected_value) &
      near(value, selected_value, tol = 0.01),
    matches_selected_tag = tag == selected_tag,
    matches_selected_filing = adsh == selected_adsh
  )

cat("\n--- Candidate facts and selected fact comparison ---\n")
print(
  audit %>%
    select(
      company_name, fiscal_year, adsh, tag,
      ddate, value, selected_tag, selected_value,
      matches_selected_value, matches_selected_tag,
      matches_selected_filing
    ),
  n = 150
)

out_path <- file.path(
  root, "outputs", "phase4g_revenue_filing_provenance.csv"
)

write_csv(audit, out_path)

cat("\nSaved:", out_path, "\n")
cat("PHASE 4G COMPLETED\n")
