
# ============================================================
# PHASE 3C: LEAKAGE-SAFE PREPROCESSING
# Baseline excludes highly incomplete R&D features.
# ============================================================

library(readr)
library(dplyr)

cat("\n============================================\n")
cat("PHASE 3C: PREPARE MODEL FEATURES\n")
cat("============================================\n")

# Load chronological splits
train <- read_csv(
  "data/processed/train_data.csv",
  show_col_types = FALSE
)

validation <- read_csv(
  "data/processed/validation_data.csv",
  show_col_types = FALSE
)

test <- read_csv(
  "data/processed/test_data.csv",
  show_col_types = FALSE
)

target <- "high_growth_next_year"

# Exclude identifiers, dates used only for tracking,
# and the target from the predictor matrix.
excluded <- c(
  "company_id",
  "company_name",
  "year",
  "target_year",
  "previous_year",
  "previous_revenue",
  "next_year",
  "next_revenue",
  "valid_target",
  "next_year_growth",
  target,
  "rd_expense",
  "rd_to_revenue"
)

features <- setdiff(
  names(train),
  excluded
)

# Retain numeric features only for this baseline.
features <- features[
  vapply(train[features], is.numeric, logical(1))
]

if (length(features) == 0) {
  stop("No numeric predictors found.")
}

# Ensure all splits use identical columns and order.
x_train <- as.data.frame(train[, features, drop = FALSE])
x_validation <- as.data.frame(
  validation[, features, drop = FALSE]
)
x_test <- as.data.frame(test[, features, drop = FALSE])

# Fit median imputation values on training data only.
imputation_values <- vapply(
  x_train,
  function(x) {
    value <- median(x, na.rm = TRUE)
    if (!is.finite(value)) NA_real_ else value
  },
  numeric(1)
)

# Remove predictors that are entirely missing in training.
keep <- is.finite(imputation_values)

if (any(!keep)) {
  cat("\nRemoving all-missing training predictors:\n")
  print(names(imputation_values)[!keep])
}

features <- features[keep]
imputation_values <- imputation_values[keep]

x_train <- x_train[, features, drop = FALSE]
x_validation <- x_validation[, features, drop = FALSE]
x_test <- x_test[, features, drop = FALSE]

# Apply training-derived medians to all splits.
for (feature in features) {
  x_train[[feature]][is.na(x_train[[feature]])] <-
    imputation_values[[feature]]
  
  x_validation[[feature]][is.na(x_validation[[feature]])] <-
    imputation_values[[feature]]
  
  x_test[[feature]][is.na(x_test[[feature]])] <-
    imputation_values[[feature]]
}

# Fit scaling parameters on training data only.
scaling_means <- vapply(x_train, mean, numeric(1))
scaling_sds <- vapply(x_train, sd, numeric(1))

# Remove constant predictors to avoid division by zero.
nonconstant <- is.finite(scaling_sds) & scaling_sds > 0

if (any(!nonconstant)) {
  cat("\nRemoving constant predictors:\n")
  print(names(scaling_sds)[!nonconstant])
}

features <- features[nonconstant]
scaling_means <- scaling_means[nonconstant]
scaling_sds <- scaling_sds[nonconstant]

x_train <- x_train[, features, drop = FALSE]
x_validation <- x_validation[, features, drop = FALSE]
x_test <- x_test[, features, drop = FALSE]

# Apply training-derived standardisation parameters.
for (feature in features) {
  x_train[[feature]] <-
    (x_train[[feature]] - scaling_means[[feature]]) /
    scaling_sds[[feature]]
  
  x_validation[[feature]] <-
    (x_validation[[feature]] - scaling_means[[feature]]) /
    scaling_sds[[feature]]
  
  x_test[[feature]] <-
    (x_test[[feature]] - scaling_means[[feature]]) /
    scaling_sds[[feature]]
}

# Attach target labels after preprocessing.
prepared_train <- data.frame(
  x_train,
  high_growth_next_year = train[[target]]
)

prepared_validation <- data.frame(
  x_validation,
  high_growth_next_year = validation[[target]]
)

prepared_test <- data.frame(
  x_test,
  high_growth_next_year = test[[target]]
)

# Check that preprocessing did not introduce missing values.
stopifnot(!anyNA(prepared_train))
stopifnot(!anyNA(prepared_validation))
stopifnot(!anyNA(prepared_test))

# Save prepared datasets and training-only parameters.
write_csv(
  prepared_train,
  "data/processed/train_features_scaled.csv"
)

write_csv(
  prepared_validation,
  "data/processed/validation_features_scaled.csv"
)

write_csv(
  prepared_test,
  "data/processed/test_features_scaled.csv"
)

write_csv(
  data.frame(
    feature = features,
    training_median = imputation_values[features],
    training_mean = scaling_means,
    training_sd = scaling_sds
  ),
  "outputs/phase3c_preprocessing_parameters.csv"
)

cat("\n--- Preprocessing summary ---\n")
cat("Training rows:", nrow(prepared_train), "\n")
cat("Validation rows:", nrow(prepared_validation), "\n")
cat("Test rows:", nrow(prepared_test), "\n")
cat("Predictors:", length(features), "\n")
cat("Missing values after preprocessing:",
    sum(is.na(prepared_train)) +
      sum(is.na(prepared_validation)) +
      sum(is.na(prepared_test)), "\n")

cat("\nPredictors used:\n")
print(features)

cat("\nPHASE 3C COMPLETED\n")
cat("============================================\n")
