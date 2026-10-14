
# ============================================================
# PHASE 2T: REVENUE TAG CONSISTENCY SENSITIVITY TEST
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 2T: REVENUE TAG SENSITIVITY\n")
cat("============================================\n")

# 1. Read company-year financial data
financial_data <- read_csv(
  "data/processed/financial_facts_company_year_improved.csv",
  show_col_types = FALSE
) %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year)
  ) %>%
  arrange(company_id, year)

# 2. Read selected facts as character to avoid guessing
#    text/Boolean types in unrelated SEC metadata columns
selected_facts <- read_csv(
  "outputs/phase2o_selected_facts.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# Convert only the required columns
revenue_tags <- selected_facts %>%
  filter(variable == "revenue") %>%
  transmute(
    company_id = as.character(company_id),
    year = as.integer(fiscal_year),
    tag = tag
  ) %>%
  distinct(company_id, year, .keep_all = TRUE)

# 3. Join tags to the revenue dataset
audit <- financial_data %>%
  left_join(
    revenue_tags,
    by = c("company_id", "year")
  ) %>%
  arrange(company_id, year) %>%
  group_by(company_id) %>%
  mutate(
    next_year = lead(year),
    next_revenue = lead(revenue),
    next_tag = lead(tag),
    
    consecutive_next_year = next_year == year + 1,
    
    valid_target = !is.na(revenue) &
      revenue > 0 &
      !is.na(next_revenue) &
      next_revenue >= 0 &
      consecutive_next_year,
    
    tag_available_both_years =
      !is.na(tag) & !is.na(next_tag),
    
    same_tag = tag_available_both_years &
      tag == next_tag,
    
    tag_changed = tag_available_both_years &
      tag != next_tag,
    
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

# 4. Compare tag consistency for valid targets
cat("\n--- Target consistency summary ---\n")

target_summary <- audit %>%
  summarise(
    all_valid_targets = sum(valid_target, na.rm = TRUE),
    
    valid_targets_same_tag = sum(
      valid_target & same_tag,
      na.rm = TRUE
    ),
    
    valid_targets_different_tag = sum(
      valid_target & tag_changed,
      na.rm = TRUE
    ),
    
    valid_targets_missing_tag = sum(
      valid_target & !tag_available_both_years,
      na.rm = TRUE
    ),
    
    same_tag_positive_labels = sum(
      valid_target & same_tag &
        high_growth_next_year == 1,
      na.rm = TRUE
    ),
    
    same_tag_negative_labels = sum(
      valid_target & same_tag &
        high_growth_next_year == 0,
      na.rm = TRUE
    )
  )

print(target_summary)

# 5. Inspect the changes in revenue-tag pairs
cat("\n--- Tag transitions ---\n")

tag_transitions <- audit %>%
  filter(
    valid_target,
    tag_available_both_years,
    tag_changed
  ) %>%
  count(tag, next_tag, sort = TRUE)

print(tag_transitions, n = Inf)

# 6. Review tag changes with large growth movements
cat("\n--- Tag changes with large revenue movements ---\n")

tag_change_review <- audit %>%
  filter(
    valid_target,
    tag_changed,
    abs(next_year_growth) > 0.50
  ) %>%
  select(
    company_id, company_name, year, next_year,
    revenue, next_revenue, tag, next_tag,
    next_year_growth, high_growth_next_year
  ) %>%
  arrange(desc(abs(next_year_growth)))

print(tag_change_review, n = 50)

# 7. Save diagnostics
write_csv(
  audit,
  "outputs/phase2t_revenue_tag_sensitivity.csv"
)

write_csv(
  tag_change_review,
  "outputs/phase2t_large_changes_with_tag_switch.csv"
)

write_csv(
  tag_transitions,
  "outputs/phase2t_tag_transitions.csv"
)

cat("\nReports saved:\n")
cat("outputs/phase2t_revenue_tag_sensitivity.csv\n")
cat("outputs/phase2t_large_changes_with_tag_switch.csv\n")
cat("outputs/phase2t_tag_transitions.csv\n")

cat("\nNo observations deleted or imputed.\n")
cat("PHASE 2T COMPLETED\n")
cat("============================================\n")
