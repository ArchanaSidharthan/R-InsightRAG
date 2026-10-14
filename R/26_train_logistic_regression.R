
# ============================================================
# PHASE 3D: LOGISTIC REGRESSION BASELINE
# ============================================================

library(readr)
library(dplyr)
library(pROC)

cat("\n============================================\n")
cat("PHASE 3D: LOGISTIC REGRESSION BASELINE\n")
cat("============================================\n")

train <- read_csv(
  "data/processed/train_features_scaled.csv",
  show_col_types = FALSE
)

validation <- read_csv(
  "data/processed/validation_features_scaled.csv",
  show_col_types = FALSE
)

test <- read_csv(
  "data/processed/test_features_scaled.csv",
  show_col_types = FALSE
)

target <- "high_growth_next_year"

# Ensure the outcome has both classes in training
if (length(unique(train[[target]])) != 2) {
  stop("Training data must contain both target classes.")
}

# Build the formula using all prepared predictors
predictors <- setdiff(names(train), target)

model_formula <- reformulate(
  predictors,
  response = target
)

# Train Logistic Regression
model <- glm(
  model_formula,
  data = train,
  family = binomial(link = "logit")
)

cat("\n--- Model summary ---\n")
print(summary(model))

# Predicted probabilities
validation_prob <- predict(
  model,
  newdata = validation,
  type = "response"
)

test_prob <- predict(
  model,
  newdata = test,
  type = "response"
)

# Choose threshold on validation data only
thresholds <- seq(0.20, 0.80, by = 0.01)

validation_metrics <- lapply(thresholds, function(threshold) {
  predicted <- as.integer(validation_prob >= threshold)
  actual <- validation[[target]]
  
  tp <- sum(predicted == 1 & actual == 1)
  tn <- sum(predicted == 0 & actual == 0)
  fp <- sum(predicted == 1 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)
  
  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  f1 <- if ((precision + recall) == 0) {
    0
  } else {
    2 * precision * recall / (precision + recall)
  }
  
  data.frame(
    threshold = threshold,
    accuracy = (tp + tn) / length(actual),
    precision = precision,
    recall = recall,
    f1 = f1
  )
})

validation_metrics <- bind_rows(validation_metrics)

# Select threshold that maximises validation F1
best_row <- validation_metrics %>%
  arrange(desc(f1), desc(precision)) %>%
  slice(1)

best_threshold <- best_row$threshold

cat("\n--- Best validation threshold ---\n")
print(best_row)

# Evaluation function
evaluate_model <- function(actual, probability, threshold) {
  predicted <- as.integer(probability >= threshold)
  
  tp <- sum(predicted == 1 & actual == 1)
  tn <- sum(predicted == 0 & actual == 0)
  fp <- sum(predicted == 1 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)
  
  precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
  recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
  f1 <- if ((precision + recall) == 0) {
    0
  } else {
    2 * precision * recall / (precision + recall)
  }
  
  roc <- pROC::roc(
    actual,
    probability,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  
  data.frame(
    threshold = threshold,
    accuracy = (tp + tn) / length(actual),
    precision = precision,
    recall = recall,
    f1 = f1,
    roc_auc = as.numeric(pROC::auc(roc)),
    true_positive = tp,
    true_negative = tn,
    false_positive = fp,
    false_negative = fn
  )
}

# Evaluate on validation and test.
# The test result is an initial benchmark only;
# do not use it to tune the model.
validation_result <- evaluate_model(
  validation[[target]],
  validation_prob,
  best_threshold
)

test_result <- evaluate_model(
  test[[target]],
  test_prob,
  best_threshold
)

cat("\n--- Validation performance ---\n")
print(validation_result)

cat("\n--- Test performance (initial benchmark) ---\n")
print(test_result)

# Save results
write_csv(
  validation_metrics,
  "outputs/phase3d_validation_threshold_search.csv"
)

write_csv(
  validation_result,
  "outputs/phase3d_logistic_validation_metrics.csv"
)

write_csv(
  test_result,
  "outputs/phase3d_logistic_test_metrics_initial.csv"
)

saveRDS(
  model,
  "models/logistic_regression_baseline.rds"
)

cat("\nSaved model: models/logistic_regression_baseline.rds\n")
cat("PHASE 3D COMPLETED\n")
cat("============================================\n")
