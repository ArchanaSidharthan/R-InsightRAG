
# ============================================================
# PHASE 2U: INSPECT SIX REVENUE CASES
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 2U: REVENUE CASE INSPECTION\n")
cat("============================================\n")

cases <- tribble(
  ~company_id, ~company_name, ~year,
  "879407",  "ARROWHEAD PHARMACEUTICALS, INC.", 2018L,
  "863894",  "VERU INC.", 2018L,
  "1348362", "LEXARIA BIOSCIENCE CORP.", 2020L,
  "1482554", "HARTFORD GREAT HEALTH CORP.", 2019L,
  "1702744", "SIMPLY GOOD FOODS CO", 2019L,
  "3545",    "ALICO INC", 2018L
)

# Read SEC selected facts as character to avoid parsing
# unrelated text/Boolean-like metadata columns.
facts <- read_csv(
  "outputs/phase2o_selected_facts.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  mutate(
    company_id = as.character(company_id),
    fiscal_year = as.integer(fiscal_year),
    ddate = as.character(ddate),
    qtrs = suppressWarnings(as.integer(qtrs)),
    value = suppressWarnings(as.numeric(value))
  )

# Extract annual revenue facts for the six companies.
case_facts <- facts %>%
  filter(
    variable == "revenue",
    company_id %in% cases$company_id,
    fiscal_year %in% 2018:2021
  ) %>%
  select(
    company_id, company_name, fiscal_year,
    tag, priority, ddate, qtrs, uom,
    value, adsh
  ) %>%
  arrange(company_id, fiscal_year)

cat("\n--- Revenue facts for selected cases ---\n")
print(case_facts, n = Inf)

# Compare the revenue values in the company-year dataset.
financial_data <- read_csv(
  "data/processed/financial_facts_company_year_improved.csv",
  show_col_types = FALSE
) %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  )

case_values <- financial_data %>%
  semi_join(cases, by = "company_id") %>%
  filter(year %in% 2018:2021) %>%
  select(company_id, company_name, year, revenue) %>%
  arrange(company_id, year)

cat("\n--- Company-year revenue values ---\n")
print(case_values, n = Inf)

# Export both review tables.
write_csv(
  case_facts,
  "outputs/phase2u_six_case_sec_facts.csv"
)

write_csv(
  case_values,
  "outputs/phase2u_six_case_revenue_values.csv"
)

cat("\nSaved:\n")
cat("outputs/phase2u_six_case_sec_facts.csv\n")
cat("outputs/phase2u_six_case_revenue_values.csv\n")
cat("\nNo data was modified or removed.\n")
cat("PHASE 2U COMPLETED\n")
cat("============================================\n")
