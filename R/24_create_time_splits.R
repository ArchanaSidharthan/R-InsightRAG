
# ============================================================
# PHASE 3B: CHRONOLOGICAL TRAIN / VALIDATION / TEST SPLIT
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 3B: TIME-BASED DATA SPLITS\n")
cat("============================================\n")

data <- read_csv(
  "data/processed/modeling_dataset.csv",
  show_col_types = FALSE
) %>%
  mutate(
    company_id = as.character(company_id),
    year = as.integer(year),
    target_year = year + 1L
  ) %>%
  arrange(year, company_id)

# Confirm the target is valid
if (any(is.na(data$high_growth_next_year))) {
  stop("Missing target values found in the modelling dataset.")
}

if (!all(data$high_growth_next_year %in% c(0L, 1L))) {
  stop("Target must contain only 0 and 1.")
}

# Define chronological splits using FEATURE year
train <- data %>%
  filter(year >= 2018, year <= 2021)

validation <- data %>%
  filter(year == 2022)

test <- data %>%
  filter(year >= 2023, year <= 2024)

# Check each split
if (nrow(train) == 0 || nrow(validation) == 0 ||
    nrow(test) == 0) {
  stop("At least one split is empty.")
}

if (max(train$year) >= min(validation$year)) {
  stop("Training and validation years overlap.")
}

if (max(validation$year) >= min(test$year)) {
  stop("Validation and test years overlap.")
}

# Remove target-derived helper fields from saved modelling splits.
# target_year is retained for clarity; it is not a predictor.
# next_revenue and next_year_growth are not in modeling_dataset.
write_csv(
  train,
  "data/processed/train_data.csv"
)

write_csv(
  validation,
  "data/processed/validation_data.csv"
)

write_csv(
  test,
  "data/processed/test_data.csv"
)

# Summarise class balance and year coverage
summarise_split <- function(df, split_name) {
  df %>%
    summarise(
      split = split_name,
      rows = n(),
      companies = n_distinct(company_id),
      first_feature_year = min(year),
      last_feature_year = max(year),
      positive_labels = sum(high_growth_next_year == 1),
      negative_labels = sum(high_growth_next_year == 0),
      positive_percentage = round(
        100 * mean(high_growth_next_year == 1),
        2
      )
    )
}

split_summary <- bind_rows(
  summarise_split(train, "Training"),
  summarise_split(validation, "Validation"),
  summarise_split(test, "Test")
)

cat("\n--- Split summary ---\n")
print(split_summary)

cat("\n--- Training labels by year ---\n")
print(
  train %>%
    count(year, high_growth_next_year) %>%
    arrange(year, high_growth_next_year),
  n = Inf
)

cat("\n--- Validation labels ---\n")
print(
  validation %>%
    count(high_growth_next_year)
)

cat("\n--- Test labels by year ---\n")
print(
  test %>%
    count(year, high_growth_next_year) %>%
    arrange(year, high_growth_next_year),
  n = Inf
)

write_csv(
  split_summary,
  "outputs/phase3b_split_summary.csv"
)

cat("\nSaved files:\n")
cat("data/processed/train_data.csv\n")
cat("data/processed/validation_data.csv\n")
cat("data/processed/test_data.csv\n")
cat("outputs/phase3b_split_summary.csv\n")

cat("\nNo rows were randomly shuffled.\n")
cat("No missing values were imputed.\n")
cat("PHASE 3B COMPLETED\n")
cat("============================================\n")
