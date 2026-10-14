
# ============================================================
# PHASE 2R: VALIDATE REVENUE ANOMALIES
# Project: R-InsightRAG
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 2R: REVENUE ANOMALY VALIDATION\n")
cat("============================================\n")

# 1. Load the reports created in Phase 2Q
growth_file <- "outputs/phase2q_unusual_growth.csv"
facts_file  <- "outputs/phase2q_unusual_growth_fact_details.csv"
zero_file   <- "outputs/phase2q_nonpositive_revenue.csv"

growth <- read_csv(growth_file, show_col_types = FALSE)
facts  <- read_csv(facts_file, show_col_types = FALSE)
zero   <- read_csv(zero_file, show_col_types = FALSE)

# 2. Inspect the column names
cat("\n--- Growth report columns ---\n")
print(names(growth))

cat("\n--- Fact details columns ---\n")
print(names(facts))

# 3. Check extreme positive growth
cat("\n--- Growth greater than 100% ---\n")

extreme_growth <- growth %>%
  filter(revenue_growth > 1) %>%
  arrange(desc(revenue_growth))

print(
  extreme_growth %>%
    select(
      company_id, company_name, year,
      previous_year, previous_revenue,
      revenue, revenue_growth
    ),
  n = Inf
)

# 4. Check severe revenue declines
cat("\n--- Growth below -50% ---\n")

severe_declines <- growth %>%
  filter(revenue_growth < -0.50) %>%
  arrange(revenue_growth)

print(
  severe_declines %>%
    select(
      company_id, company_name, year,
      previous_year, previous_revenue,
      revenue, revenue_growth
    ),
  n = Inf
)

# 5. Summarise zero-revenue cases
cat("\n--- Zero or negative revenue ---\n")

print(zero, n = Inf)

# 6. Summarise the selected revenue tags
cat("\n--- Selected revenue tags ---\n")

if ("tag" %in% names(facts)) {
  print(
    facts %>%
      count(tag, sort = TRUE)
  )
}

# 7. Summarise units and annual-period indicators
cat("\n--- Units of measure ---\n")

if ("uom" %in% names(facts)) {
  print(facts %>% count(uom, sort = TRUE))
}

cat("\n--- qtrs values ---\n")

if ("qtrs" %in% names(facts)) {
  print(facts %>% count(qtrs, sort = TRUE))
}

# 8. Create a focused review table
review_columns <- intersect(
  c(
    "company_id", "company_name", "fiscal_year",
    "year", "revenue_growth", "previous_revenue",
    "revenue", "tag", "priority", "ddate",
    "qtrs", "uom", "value", "adsh"
  ),
  names(facts)
)

fact_review <- facts %>%
  select(all_of(review_columns)) %>%
  arrange(desc(abs(revenue_growth)))

write_csv(
  fact_review,
  "outputs/phase2r_revenue_fact_review.csv"
)

write_csv(
  extreme_growth,
  "outputs/phase2r_extreme_growth.csv"
)

write_csv(
  severe_declines,
  "outputs/phase2r_severe_declines.csv"
)

cat("\n--- Files saved ---\n")
cat("outputs/phase2r_revenue_fact_review.csv\n")
cat("outputs/phase2r_extreme_growth.csv\n")
cat("outputs/phase2r_severe_declines.csv\n")

cat("\nPHASE 2R CHECKS COMPLETED\n")
cat("No rows were deleted or imputed.\n")
cat("============================================\n")
