
# ============================================================
# R-InsightRAG
# PHASE 4H: REVENUE SELECTION VALIDATION
# ============================================================

library(dplyr)
library(readr)

root <- getwd()

all_path <- file.path(
  root, "data", "processed", "financial_facts_all_units.csv"
)

selected_path <- file.path(
  root, "data", "processed", "financial_facts_long.csv"
)

if (!file.exists(all_path) || !file.exists(selected_path)) {
  stop("Required SEC fact files are missing.")
}

all_facts <- read_csv(all_path, show_col_types = FALSE)
selected <- read_csv(selected_path, show_col_types = FALSE)

# Selected annual revenue facts
selected_revenue <- selected %>%
  filter(
    variable == "revenue",
    uom == "USD",
    qtrs == 4
  ) %>%
  select(
    company_id, company_name, fiscal_year,
    selected_adsh = adsh,
    selected_tag = tag,
    selected_ddate = ddate,
    selected_value = value
  ) %>%
  distinct()

# All candidate annual revenue facts
candidate_revenue <- all_facts %>%
  filter(
    variable == "revenue",
    uom == "USD",
    qtrs == 4
  ) %>%
  select(
    company_id, fiscal_year,
    candidate_adsh = adsh,
    candidate_tag = tag,
    candidate_ddate = ddate,
    candidate_value = value
  ) %>%
  distinct()

# Check whether the selected fact exists in the source data
validation <- selected_revenue %>%
  left_join(
    candidate_revenue,
    by = c("company_id", "fiscal_year"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    same_filing = selected_adsh == candidate_adsh,
    same_tag = selected_tag == candidate_tag,
    same_date = selected_ddate == candidate_ddate,
    same_value = near(selected_value, candidate_value, tol = 0.01),
    exact_match =
      same_filing & same_tag & same_date & same_value
  )

# One row per selected revenue observation
summary_by_record <- validation %>%
  group_by(
    company_id, company_name, fiscal_year,
    selected_adsh, selected_tag, selected_ddate, selected_value
  ) %>%
  summarise(
    exact_source_match = any(exact_match, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Selected revenue fact matching ---\n")
cat("Selected annual revenue records:", nrow(summary_by_record), "\n")
cat(
  "Exact source matches:",
  sum(summary_by_record$exact_source_match), "\n"
)
cat(
  "No exact source match:",
  sum(!summary_by_record$exact_source_match), "\n"
)

# Save records without an exact match
unmatched <- summary_by_record %>%
  filter(!exact_source_match)

write_csv(
  summary_by_record,
  file.path(root, "outputs", "phase4h_revenue_match_summary.csv")
)

write_csv(
  unmatched,
  file.path(root, "outputs", "phase4h_unmatched_revenue_facts.csv")
)

cat("\n--- First unmatched records ---\n")
print(head(unmatched, 20), n = 20)

cat("\nSaved:\n")
cat("outputs/phase4h_revenue_match_summary.csv\n")
cat("outputs/phase4h_unmatched_revenue_facts.csv\n")
cat("\nPHASE 4H COMPLETED\n")
