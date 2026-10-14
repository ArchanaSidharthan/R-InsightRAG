
# ============================================================
# R-InsightRAG
# PHASE 3E: LOGISTIC REGRESSION DIAGNOSTICS
# ============================================================

cat("\n=========================================\n")
cat("PHASE 3E: LOGISTIC REGRESSION DIAGNOSTICS\n")
cat("=========================================\n")

# ------------------------------------------------------------
# 1. Load validation data and saved model
# ------------------------------------------------------------

validation <- read.csv(
  "data/processed/validation_features_scaled.csv"
)

model <- readRDS(
  "models/logistic_regression_baseline.rds"
)

target_col <- "high_growth_next_year"

if (!target_col %in% names(validation)) {
  stop("Target column not found in validation data.")
}

# Ensure target is numeric 0/1
y <- as.numeric(as.character(validation[[target_col]]))

if (anyNA(y) || !all(y %in% c(0, 1))) {
  stop("Target must contain only 0 and 1, without missing values.")
}

# ------------------------------------------------------------
# 2. Generate validation probabilities
# ------------------------------------------------------------

prob <- predict(
  model,
  newdata = validation,
  type = "response"
)

if (anyNA(prob) || any(!is.finite(prob))) {
  stop("Predicted probabilities contain missing or invalid values.")
}

cat("\n--- Validation data ---\n")
cat("Rows:", nrow(validation), "\n")
cat("Actual positive cases:", sum(y == 1), "\n")
cat("Actual negative cases:", sum(y == 0), "\n")

cat("\n--- Predicted probability summary ---\n")
print(summary(prob))

# ------------------------------------------------------------
# 3. Define metric calculation function
# ------------------------------------------------------------

calculate_metrics <- function(actual, predicted, threshold) {
  
  tp <- sum(actual == 1 & predicted == 1)
  tn <- sum(actual == 0 & predicted == 0)
  fp <- sum(actual == 0 & predicted == 1)
  fn <- sum(actual == 1 & predicted == 0)
  
  accuracy <- (tp + tn) / length(actual)
  
  precision <- if ((tp + fp) > 0) {
    tp / (tp + fp)
  } else {
    0
  }
  
  recall <- if ((tp + fn) > 0) {
    tp / (tp + fn)
  } else {
    0
  }
  
  specificity <- if ((tn + fp) > 0) {
    tn / (tn + fp)
  } else {
    0
  }
  
  f1 <- if ((precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    0
  }
  
  data.frame(
    threshold = threshold,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = f1,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn
  )
}

# ------------------------------------------------------------
# 4. Evaluate thresholds using validation data only
# ------------------------------------------------------------

thresholds <- seq(0.30, 0.70, by = 0.01)

threshold_results <- do.call(
  rbind,
  lapply(thresholds, function(threshold) {
    
    predicted <- ifelse(prob >= threshold, 1, 0)
    
    calculate_metrics(
      actual = y,
      predicted = predicted,
      threshold = threshold
    )
  })
)

threshold_results <- threshold_results[
  order(-threshold_results$f1),
]

cat("\n--- Top 10 validation thresholds by F1 ---\n")
print(head(threshold_results, 10), row.names = FALSE)

# ------------------------------------------------------------
# 5. Majority-class baseline on validation data
# ------------------------------------------------------------

majority_class <- as.numeric(
  names(which.max(table(y)))
)

majority_predictions <- rep(
  majority_class,
  length(y)
)

baseline_metrics <- calculate_metrics(
  actual = y,
  predicted = majority_predictions,
  threshold = NA_real_
)

cat("\n--- Validation majority-class baseline ---\n")
print(baseline_metrics, row.names = FALSE)

# ------------------------------------------------------------
# 6. Compare default threshold 0.50
# ------------------------------------------------------------

default_predictions <- ifelse(prob >= 0.50, 1, 0)

default_metrics <- calculate_metrics(
  actual = y,
  predicted = default_predictions,
  threshold = 0.50
)

cat("\n--- Validation performance at threshold 0.50 ---\n")
print(default_metrics, row.names = FALSE)

# ------------------------------------------------------------
# 7. Save outputs
# ------------------------------------------------------------

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

write.csv(
  threshold_results,
  "outputs/phase3e_threshold_diagnostics.csv",
  row.names = FALSE
)

write.csv(
  baseline_metrics,
  "outputs/phase3e_majority_baseline.csv",
  row.names = FALSE
)

write.csv(
  default_metrics,
  "outputs/phase3e_logistic_threshold_050.csv",
  row.names = FALSE
)

cat("\nSaved files:\n")
cat("- outputs/phase3e_threshold_diagnostics.csv\n")
cat("- outputs/phase3e_majority_baseline.csv\n")
cat("- outputs/phase3e_logistic_threshold_050.csv\n")

cat("\nPHASE 3E COMPLETED\n")
cat("=========================================\n")
