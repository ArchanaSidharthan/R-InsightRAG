
# ============================================================
# PHASE 2S: REVENUE CONSISTENCY AND TARGET READINESS AUDIT
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 2S: REVENUE CONSISTENCY AUDIT\n")
cat("============================================\n")

# Load the improved company-year dataset
financial_data <- read_csv(
  "data/processed/financial_facts_company_year_improved.csv",
  show_col_types = FALSE
)

# Load selected facts to inspect the revenue tag used
selected_facts <- read_csv(
  "outputs/phase2o_selected_facts.csv",
  show_col_types = FALSE
)

# Standardise keys
financial_data <- financial_data %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  ) %>%
  arrange(company_id, year)

selected_facts <- selected_facts %>%
  mutate(
    company_id = as.character(company_id),
    fiscal_year = as.integer(fiscal_year)
  )

# Confirm required columns
required_data_cols <- c(
  "company_id", "year", "company_name", "revenue"
)

missing_data_cols <- setdiff(
  required_data_cols, names(financial_data)
)

if (length(missing_data_cols) > 0) {
  stop(
    "Missing columns in financial dataset: ",
    paste(missing_data_cols, collapse = ", ")
  )
}

required_fact_cols <- c(
  "company_id", "fiscal_year", "variable", "tag"
)

missing_fact_cols <- setdiff(
  required_fact_cols, names(selected_facts)
)

if (length(missing_fact_cols) > 0) {
  stop(
    "Missing columns in selected facts: ",
    paste(missing_fact_cols, collapse = ", ")
  )
}

# 1. Extract revenue tag provenance
revenue_tags <- selected_facts %>%
  filter(variable == "revenue") %>%
  select(
    company_id, fiscal_year, tag,
    any_of(c("priority", "value", "adsh", "ddate", "qtrs", "uom"))
  ) %>%
  distinct()

# Check that there is only one selected revenue tag per company-year
tag_duplicates <- revenue_tags %>%
  count(company_id, fiscal_year) %>%
  filter(n > 1)

cat("\n--- Duplicate revenue-tag keys ---\n")
print(tag_duplicates, n = Inf)

# 2. Join tags to company-year revenue values
revenue_audit <- financial_data %>%
  left_join(
    revenue_tags,
    by = c("company_id" = "company_id",
           "year" = "fiscal_year")
  ) %>%
  arrange(company_id, year) %>%
  group_by(company_id) %>%
  mutate(
    previous_revenue = lag(revenue),
    previous_tag = lag(tag),
    previous_year = lag(year),
    
    consecutive_years = year == previous_year + 1,
    
    tag_changed = !is.na(tag) &
      !is.na(previous_tag) &
      tag != previous_tag,
    
    revenue_growth = if_else(
      !is.na(revenue) &
        !is.na(previous_revenue) &
        previous_revenue > 0 &
        revenue >= 0 &
        consecutive_years,
      revenue / previous_revenue - 1,
      NA_real_
    )
  ) %>%
  ungroup()

# 3. Summarise revenue tag usage
cat("\n--- Revenue tag counts ---\n")

print(
  revenue_audit %>%
    filter(!is.na(tag)) %>%
    count(tag, sort = TRUE)
)

# 4. Identify companies whose selected revenue tag changes
tag_changes <- revenue_audit %>%
  filter(tag_changed) %>%
  select(
    company_id, company_name, year,
    previous_year, previous_tag, tag,
    previous_revenue, revenue, revenue_growth
  )

cat("\n--- Revenue tag changes between consecutive years ---\n")
cat("Rows with a tag change:", nrow(tag_changes), "\n")
print(tag_changes, n = 50)

# 5. Assess valid next-year target availability
target_audit <- revenue_audit %>%
  group_by(company_id) %>%
  mutate(
    next_year = lead(year),
    next_revenue = lead(revenue),
    next_tag = lead(tag),
    next_year_consecutive = next_year == year + 1,
    
    valid_target = !is.na(revenue) &
      revenue > 0 &
      !is.na(next_revenue) &
      next_revenue >= 0 &
      next_year_consecutive,
    
    next_year_growth = if_else(
      valid_target,
      next_revenue / revenue - 1,
      NA_real_
    ),
    
    high_growth_next_year = if_else(
      valid_target,
      as.integer(next_year_growth >= 0.10),
      NA_integer_
    )
  ) %>%
  ungroup()

cat("\n--- Target readiness summary ---\n")

print(
  target_audit %>%
    summarise(
      total_company_year_rows = n(),
      valid_next_year_targets = sum(valid_target, na.rm = TRUE),
      missing_or_invalid_targets = sum(!valid_target, na.rm = TRUE),
      positive_class = sum(
        high_growth_next_year == 1,
        na.rm = TRUE
      ),
      negative_class = sum(
        high_growth_next_year == 0,
        na.rm = TRUE
      )
    )
)

# 6. Export audit files
write_csv(
  revenue_audit,
  "outputs/phase2s_revenue_consistency_audit.csv"
)

write_csv(
  tag_changes,
  "outputs/phase2s_revenue_tag_changes.csv"
)

write_csv(
  target_audit,
  "outputs/phase2s_target_readiness_audit.csv"
)

cat("\n--- Reports saved ---\n")
cat("outputs/phase2s_revenue_consistency_audit.csv\n")
cat("outputs/phase2s_revenue_tag_changes.csv\n")
cat("outputs/phase2s_target_readiness_audit.csv\n")

cat("\nNo financial observations were deleted or imputed.\n")
cat("PHASE 2S COMPLETED\n")
cat("============================================\n")
