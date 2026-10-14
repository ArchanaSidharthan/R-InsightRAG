
# ============================================================
# R-InsightRAG
# PHASE 3I: MODEL COMPARISON & FINAL TEST EVALUATION
# ============================================================

cat("\n=========================================\n")
cat("PHASE 3I: FINAL MODEL EVALUATION\n")
cat("=========================================\n")

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages(
    "pROC",
    repos = "https://cloud.r-project.org"
  )
}

library(pROC)

set.seed(42)

# ------------------------------------------------------------
# 1. Load the untouched test dataset
# ------------------------------------------------------------

test <- read.csv(
  "data/processed/test_features_scaled.csv"
)

target_col <- "high_growth_next_year"

if (!target_col %in% names(test)) {
  stop("Target column missing from test dataset.")
}

actual <- as.numeric(as.character(test[[target_col]]))

if (anyNA(actual) || !all(actual %in% c(0, 1))) {
  stop("Test target must contain only 0 and 1.")
}

cat("\nTest rows:", nrow(test), "\n")
cat("Actual positive cases:", sum(actual == 1), "\n")
cat("Actual negative cases:", sum(actual == 0), "\n")

# ------------------------------------------------------------
# 2. Load models and their validation-selected thresholds
# ------------------------------------------------------------

model_paths <- list(
  "Logistic Regression" = "models/logistic_regression_baseline.rds",
  "Random Forest" = "models/random_forest_baseline.rds",
  "SVM" = "models/svm_baseline.rds",
  "XGBoost" = "models/xgboost_baseline.rds"
)

metric_paths <- list(
  "Logistic Regression" =
    "outputs/phase3d_logistic_validation_metrics.csv",
  "Random Forest" =
    "outputs/phase3f_random_forest_validation_metrics.csv",
  "SVM" =
    "outputs/phase3g_svm_validation_metrics.csv",
  "XGBoost" =
    "outputs/phase3h_xgboost_validation_metrics.csv"
)

for (model_name in names(model_paths)) {
  if (!file.exists(model_paths[[model_name]])) {
    stop(paste("Model file not found:", model_paths[[model_name]]))
  }
  
  if (!file.exists(metric_paths[[model_name]])) {
    stop(paste(
      "Validation metric file not found:",
      metric_paths[[model_name]]
    ))
  }
}

models <- lapply(model_paths, readRDS)

validation_metrics <- lapply(
  metric_paths,
  read.csv
)

thresholds <- vapply(
  validation_metrics,
  function(result) {
    if (!"threshold" %in% names(result) ||
        nrow(result) < 1 ||
        !is.finite(result$threshold[1])) {
      stop("Could not retrieve a valid validation threshold.")
    }
    
    result$threshold[1]
  },
  numeric(1)
)

cat("\n--- Thresholds selected on validation data ---\n")
print(data.frame(
  model = names(thresholds),
  threshold = as.numeric(thresholds)
), row.names = FALSE)

# ------------------------------------------------------------
# 3. Prepare predictors
# ------------------------------------------------------------

predictors <- setdiff(names(test), target_col)

if (anyNA(test[, predictors, drop = FALSE])) {
  stop("Test predictors contain missing values.")
}

if (!all(vapply(test[, predictors, drop = FALSE], is.numeric, logical(1)))) {
  stop("All test predictors must be numeric.")
}

# ------------------------------------------------------------
# 4. Define evaluation metrics
# ------------------------------------------------------------

evaluate_predictions <- function(actual, probability, threshold) {
  
  predicted <- ifelse(probability >= threshold, 1, 0)
  
  tp <- sum(actual == 1 & predicted == 1)
  tn <- sum(actual == 0 & predicted == 0)
  fp <- sum(actual == 0 & predicted == 1)
  fn <- sum(actual == 1 & predicted == 0)
  
  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  recall <- if (tp + fn > 0) tp / (tp + fn) else 0
  specificity <- if (tn + fp > 0) tn / (tn + fp) else 0
  
  f1 <- if (precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    0
  }
  
  auc_value <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = actual,
        predictor = probability,
        levels = c(0, 1),
        direction = "<",
        quiet = TRUE
      )
    )
  )
  
  list(
    metrics = data.frame(
      threshold = threshold,
      accuracy = (tp + tn) / length(actual),
      precision = precision,
      recall = recall,
      specificity = specificity,
      f1 = f1,
      roc_auc = auc_value,
      TP = tp,
      TN = tn,
      FP = fp,
      FN = fn
    ),
    confusion = data.frame(
      actual = c("Actual 0", "Actual 1"),
      predicted_0 = c(tn, fn),
      predicted_1 = c(fp, tp)
    )
  )
}

# ------------------------------------------------------------
# 5. Generate test probabilities for all models
# ------------------------------------------------------------

test_probabilities <- list()

# Logistic Regression
test_probabilities[["Logistic Regression"]] <- as.numeric(
  predict(
    models[["Logistic Regression"]],
    newdata = test,
    type = "response"
  )
)

# Random Forest
rf_prob <- predict(
  models[["Random Forest"]],
  newdata = test[, predictors, drop = FALSE],
  type = "prob"
)

test_probabilities[["Random Forest"]] <- rf_prob[, "1"]

# SVM
svm_prediction <- predict(
  models[["SVM"]],
  newdata = test[, predictors, drop = FALSE],
  probability = TRUE
)

svm_probability_matrix <- attr(
  svm_prediction,
  "probabilities"
)

if (is.null(svm_probability_matrix) ||
    !"1" %in% colnames(svm_probability_matrix)) {
  stop("Could not retrieve SVM probabilities for class 1.")
}

test_probabilities[["SVM"]] <- svm_probability_matrix[, "1"]

# XGBoost
x_test <- as.matrix(test[, predictors, drop = FALSE])

dtest <- xgboost::xgb.DMatrix(data = x_test)

test_probabilities[["XGBoost"]] <- as.numeric(
  predict(models[["XGBoost"]], dtest)
)

# Validate probability outputs
for (model_name in names(test_probabilities)) {
  probability <- test_probabilities[[model_name]]
  
  if (length(probability) != length(actual) ||
      anyNA(probability) ||
      any(!is.finite(probability)) ||
      any(probability < 0 | probability > 1)) {
    stop(paste("Invalid probabilities for:", model_name))
  }
}

# ------------------------------------------------------------
# 6. Evaluate all models ONCE on the test set
# ------------------------------------------------------------

test_results <- list()
confusion_results <- list()

for (model_name in names(test_probabilities)) {
  
  result <- evaluate_predictions(
    actual = actual,
    probability = test_probabilities[[model_name]],
    threshold = thresholds[[model_name]]
  )
  
  result$metrics$model <- model_name
  result$metrics <- result$metrics[
    , c("model", setdiff(names(result$metrics), "model"))
  ]
  
  result$confusion$model <- model_name
  result$confusion <- result$confusion[
    , c("model", setdiff(names(result$confusion), "model"))
  ]
  
  test_results[[model_name]] <- result$metrics
  confusion_results[[model_name]] <- result$confusion
}

test_comparison <- do.call(rbind, test_results)
confusion_matrices <- do.call(rbind, confusion_results)

rownames(test_comparison) <- NULL
rownames(confusion_matrices) <- NULL

cat("\n--- FINAL TEST PERFORMANCE ---\n")
print(test_comparison, row.names = FALSE)

cat("\n--- TEST CONFUSION MATRICES ---\n")
print(confusion_matrices, row.names = FALSE)

# ------------------------------------------------------------
# 7. Majority-class test baseline
# ------------------------------------------------------------

majority_class <- as.numeric(
  names(which.max(table(actual)))
)

baseline_prediction <- rep(majority_class, length(actual))

baseline_accuracy <- mean(baseline_prediction == actual)

baseline <- data.frame(
  model = "Majority-class baseline",
  accuracy = baseline_accuracy,
  majority_class = majority_class,
  test_rows = length(actual)
)

cat("\n--- MAJORITY-CLASS BASELINE ---\n")
print(baseline, row.names = FALSE)

# ------------------------------------------------------------
# 8. Combine validation and test ROC-AUC results
# ------------------------------------------------------------

validation_auc_values <- vapply(
  validation_metrics,
  function(result) {
    if (!"roc_auc" %in% names(result)) {
      return(NA_real_)
    }
    as.numeric(result$roc_auc[1])
  },
  numeric(1)
)

comparison_summary <- data.frame(
  model = names(test_probabilities),
  validation_roc_auc = as.numeric(
    validation_auc_values[names(test_probabilities)]
  ),
  test_roc_auc = vapply(
    names(test_probabilities),
    function(model_name) {
      test_comparison$roc_auc[
        match(model_name, test_comparison$model)
      ]
    },
    numeric(1)
  ),
  validation_threshold = as.numeric(
    thresholds[names(test_probabilities)]
  )
)

cat("\n--- VALIDATION VS TEST ROC-AUC ---\n")
print(comparison_summary, row.names = FALSE)

# ------------------------------------------------------------
# 9. Save reports
# ------------------------------------------------------------

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

write.csv(
  test_comparison,
  "outputs/phase3i_final_test_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  confusion_matrices,
  "outputs/phase3i_test_confusion_matrices.csv",
  row.names = FALSE
)

write.csv(
  baseline,
  "outputs/phase3i_majority_class_baseline.csv",
  row.names = FALSE
)

write.csv(
  comparison_summary,
  "outputs/phase3i_validation_vs_test_auc.csv",
  row.names = FALSE
)

cat("\nSaved reports:\n")
cat("- outputs/phase3i_final_test_model_comparison.csv\n")
cat("- outputs/phase3i_test_confusion_matrices.csv\n")
cat("- outputs/phase3i_majority_class_baseline.csv\n")
cat("- outputs/phase3i_validation_vs_test_auc.csv\n")

cat("\nPHASE 3I COMPLETED\n")
cat("=========================================\n")
