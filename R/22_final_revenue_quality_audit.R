
# ============================================================
# PHASE 2V: FINAL REVENUE QUALITY AND TARGET READINESS AUDIT
# Project: R-InsightRAG
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 2V: FINAL REVENUE QUALITY AUDIT\n")
cat("============================================\n")

# 1. Load company-year financial data
financial_data <- read_csv(
  "data/processed/financial_facts_company_year_improved.csv",
  show_col_types = FALSE
) %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  ) %>%
  arrange(company_id, year)

# 2. Load selected revenue facts as character to avoid
# parsing unrelated SEC metadata columns.
selected_facts <- read_csv(
  "outputs/phase2o_selected_facts.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  mutate(
    company_id = as.character(company_id),
    fiscal_year = as.integer(fiscal_year)
  )

# 3. Get selected revenue tag per company-year
revenue_tags <- selected_facts %>%
  filter(variable == "revenue") %>%
  transmute(
    company_id,
    year = fiscal_year,
    tag
  ) %>%
  distinct(company_id, year, .keep_all = TRUE)

# 4. Attach tags and calculate next-year target
audit <- financial_data %>%
  left_join(
    revenue_tags,
    by = c("company_id", "year")
  ) %>%
  arrange(company_id, year) %>%
  group_by(company_id) %>%
  mutate(
    previous_year = lag(year),
    previous_revenue = lag(revenue),
    previous_tag = lag(tag),
    
    next_year = lead(year),
    next_revenue = lead(revenue),
    next_tag = lead(tag),
    
    consecutive_previous = year == previous_year + 1,
    consecutive_next = next_year == year + 1,
    
    current_revenue_valid =
      !is.na(revenue) & revenue > 0,
    
    next_revenue_valid =
      !is.na(next_revenue) & next_revenue >= 0,
    
    valid_target = current_revenue_valid &
      next_revenue_valid &
      consecutive_next,
    
    next_year_growth = if_else(
      valid_target,
      next_revenue / revenue - 1,
      NA_real_
    ),
    
    high_growth_next_year = if_else(
      valid_target,
      as.integer(next_year_growth >= 0.10),
      NA_integer_
    ),
    
    tag_available = !is.na(tag) & !is.na(next_tag),
    
    same_tag = tag_available & tag == next_tag,
    
    tag_changed = tag_available & tag != next_tag
  ) %>%
  ungroup()

# 5. Revenue quality summary
cat("\n--- Revenue data quality ---\n")

quality_summary <- audit %>%
  summarise(
    rows = n(),
    companies = n_distinct(company_id),
    missing_revenue = sum(is.na(revenue)),
    zero_revenue = sum(revenue == 0, na.rm = TRUE),
    negative_revenue = sum(revenue < 0, na.rm = TRUE),
    positive_revenue = sum(revenue > 0, na.rm = TRUE),
    missing_revenue_tag = sum(is.na(tag))
  )

print(quality_summary)

# 6. Target summary by fiscal year
cat("\n--- Target readiness by year ---\n")

target_by_year <- audit %>%
  group_by(year) %>%
  summarise(
    rows = n(),
    valid_targets = sum(valid_target, na.rm = TRUE),
    invalid_targets = sum(!valid_target, na.rm = TRUE),
    positive_labels = sum(
      high_growth_next_year == 1,
      na.rm = TRUE
    ),
    negative_labels = sum(
      high_growth_next_year == 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(target_by_year, n = Inf)

# 7. Compare labels for unchanged and changed revenue tags
cat("\n--- Target distribution by tag consistency ---\n")

tag_summary <- audit %>%
  filter(valid_target) %>%
  mutate(
    tag_status = case_when(
      !tag_available ~ "Missing tag",
      same_tag ~ "Same tag",
      tag_changed ~ "Changed tag",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(tag_status) %>%
  summarise(
    rows = n(),
    positive_labels = sum(high_growth_next_year == 1),
    negative_labels = sum(high_growth_next_year == 0),
    median_growth = median(next_year_growth, na.rm = TRUE),
    .groups = "drop"
  )

print(tag_summary, n = Inf)

# 8. Growth distribution and extreme observations
cat("\n--- Growth distribution ---\n")

growth_summary <- audit %>%
  filter(valid_target) %>%
  summarise(
    valid_growth_rows = n(),
    median_growth = median(next_year_growth),
    minimum_growth = min(next_year_growth),
    maximum_growth = max(next_year_growth),
    growth_above_100pct = sum(next_year_growth > 1),
    growth_below_minus_50pct = sum(next_year_growth < -0.50)
  )

print(growth_summary)

# 9. Create a separate diagnostic table of extreme targets
extreme_targets <- audit %>%
  filter(
    valid_target,
    next_year_growth > 1 |
      next_year_growth < -0.50
  ) %>%
  select(
    company_id, company_name, year, next_year,
    revenue, next_revenue, tag, next_tag,
    next_year_growth, high_growth_next_year
  ) %>%
  arrange(desc(abs(next_year_growth)))

# 10. Save reports
write_csv(
  audit,
  "outputs/phase2v_final_revenue_quality_audit.csv"
)

write_csv(
  target_by_year,
  "outputs/phase2v_target_readiness_by_year.csv"
)

write_csv(
  tag_summary,
  "outputs/phase2v_target_by_tag_consistency.csv"
)

write_csv(
  extreme_targets,
  "outputs/phase2v_extreme_target_growth.csv"
)

cat("\n--- Reports saved ---\n")
cat("outputs/phase2v_final_revenue_quality_audit.csv\n")
cat("outputs/phase2v_target_readiness_by_year.csv\n")
cat("outputs/phase2v_target_by_tag_consistency.csv\n")
cat("outputs/phase2v_extreme_target_growth.csv\n")

cat("\nNo observations were deleted or imputed.\n")
cat("No financial values were modified.\n")
cat("PHASE 2V COMPLETED\n")
cat("============================================\n")
