
# ============================================================
# R-InsightRAG
# PHASE 3H: XGBOOST CLASSIFICATION
# ============================================================

cat("\n=========================================\n")
cat("PHASE 3H: XGBOOST CLASSIFICATION\n")
cat("=========================================\n")

if (!requireNamespace("xgboost", quietly = TRUE)) {
  install.packages(
    "xgboost",
    repos = "https://cloud.r-project.org"
  )
}

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages(
    "pROC",
    repos = "https://cloud.r-project.org"
  )
}

library(xgboost)
library(pROC)

set.seed(42)

# ------------------------------------------------------------
# 1. Load training and validation data only
# ------------------------------------------------------------

train <- read.csv(
  "data/processed/train_features_scaled.csv"
)

validation <- read.csv(
  "data/processed/validation_features_scaled.csv"
)

target_col <- "high_growth_next_year"

predictors <- setdiff(names(train), target_col)

if (!target_col %in% names(validation)) {
  stop("Target column missing from validation data.")
}

if (!setequal(
  predictors,
  setdiff(names(validation), target_col)
)) {
  stop("Training and validation predictors do not match.")
}

validation <- validation[, c(predictors, target_col)]

y_train <- as.numeric(as.character(train[[target_col]]))
y_validation <- as.numeric(as.character(validation[[target_col]]))

if (anyNA(y_train) || anyNA(y_validation) ||
    !all(y_train %in% c(0, 1)) ||
    !all(y_validation %in% c(0, 1))) {
  stop("Targets must contain only 0 and 1.")
}

x_train <- as.matrix(train[, predictors, drop = FALSE])
x_validation <- as.matrix(
  validation[, predictors, drop = FALSE]
)

if (anyNA(x_train) || anyNA(x_validation) ||
    any(!is.finite(x_train)) ||
    any(!is.finite(x_validation))) {
  stop("Predictors contain missing or non-finite values.")
}

cat("\nTraining rows:", nrow(x_train), "\n")
cat("Validation rows:", nrow(x_validation), "\n")
cat("Predictors:", length(predictors), "\n")

dtrain <- xgboost::xgb.DMatrix(
  data = x_train,
  label = y_train
)

dvalidation <- xgboost::xgb.DMatrix(
  data = x_validation,
  label = y_validation
)

# ------------------------------------------------------------
# 2. Define parameter configurations
# ------------------------------------------------------------

parameter_grid <- data.frame(
  max_depth = c(2, 2, 3, 3, 4, 4),
  eta = c(0.03, 0.10, 0.03, 0.10, 0.03, 0.10),
  min_child_weight = c(5, 5, 5, 5, 5, 5),
  subsample = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8),
  colsample_bytree = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
)

tuning_results <- data.frame()

# ------------------------------------------------------------
# 3. Train and compare configurations
# ------------------------------------------------------------

for (i in seq_len(nrow(parameter_grid))) {
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = parameter_grid$max_depth[i],
    eta = parameter_grid$eta[i],
    min_child_weight = parameter_grid$min_child_weight[i],
    subsample = parameter_grid$subsample[i],
    colsample_bytree = parameter_grid$colsample_bytree[i],
    lambda = 1,
    alpha = 0
  )
  
  cat(
    "\nConfiguration", i, "of", nrow(parameter_grid),
    "| depth =", params$max_depth,
    "| eta =", params$eta, "\n"
  )
  
  set.seed(42)
  
  fit <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = 150,
    verbose = 0
  )
  
  probabilities <- predict(fit, dvalidation)
  
  auc_value <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = y_validation,
        predictor = probabilities,
        levels = c(0, 1),
        direction = "<",
        quiet = TRUE
      )
    )
  )
  
  tuning_results <- rbind(
    tuning_results,
    data.frame(
      configuration = i,
      max_depth = params$max_depth,
      eta = params$eta,
      min_child_weight = params$min_child_weight,
      subsample = params$subsample,
      colsample_bytree = params$colsample_bytree,
      validation_roc_auc = auc_value
    )
  )
}

tuning_results <- tuning_results[
  order(-tuning_results$validation_roc_auc),
]

cat("\n--- XGBoost tuning results ---\n")
print(tuning_results, row.names = FALSE)

# ------------------------------------------------------------
# 4. Refit selected configuration
# ------------------------------------------------------------

best <- tuning_results[1, ]

best_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = best$max_depth,
  eta = best$eta,
  min_child_weight = best$min_child_weight,
  subsample = best$subsample,
  colsample_bytree = best$colsample_bytree,
  lambda = 1,
  alpha = 0
)

set.seed(42)

best_model <- xgboost::xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 150,
  verbose = 0
)

validation_prob <- predict(best_model, dvalidation)

validation_auc <- as.numeric(
  pROC::auc(
    pROC::roc(
      response = y_validation,
      predictor = validation_prob,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
  )
)

# ------------------------------------------------------------
# 5. Evaluate thresholds on validation data only
# ------------------------------------------------------------

calculate_metrics <- function(actual, predicted, threshold) {
  
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
  
  data.frame(
    threshold = threshold,
    accuracy = (tp + tn) / length(actual),
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

thresholds <- seq(0.20, 0.80, by = 0.01)

threshold_results <- do.call(
  rbind,
  lapply(thresholds, function(threshold) {
    calculate_metrics(
      y_validation,
      ifelse(validation_prob >= threshold, 1, 0),
      threshold
    )
  })
)

threshold_results <- threshold_results[
  order(-threshold_results$f1, -threshold_results$precision),
]

best_threshold <- threshold_results$threshold[1]

validation_metrics <- calculate_metrics(
  y_validation,
  ifelse(validation_prob >= best_threshold, 1, 0),
  best_threshold
)

validation_metrics$roc_auc <- validation_auc
validation_metrics$max_depth <- best$max_depth
validation_metrics$eta <- best$eta

cat("\n--- XGBoost validation performance ---\n")
print(validation_metrics, row.names = FALSE)

cat("\n--- Top 10 thresholds by F1 ---\n")
print(head(threshold_results, 10), row.names = FALSE)

# ------------------------------------------------------------
# 6. Feature importance
# ------------------------------------------------------------

importance_table <- xgboost::xgb.importance(
  feature_names = predictors,
  model = best_model
)

cat("\n--- Top 10 important features ---\n")
print(head(importance_table, 10))

# ------------------------------------------------------------
# 7. Save model and outputs
# ------------------------------------------------------------

dir.create("models", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  best_model,
  "models/xgboost_baseline.rds"
)

write.csv(
  tuning_results,
  "outputs/phase3h_xgboost_tuning.csv",
  row.names = FALSE
)

write.csv(
  validation_metrics,
  "outputs/phase3h_xgboost_validation_metrics.csv",
  row.names = FALSE
)

write.csv(
  threshold_results,
  "outputs/phase3h_xgboost_thresholds.csv",
  row.names = FALSE
)

write.csv(
  importance_table,
  "outputs/phase3h_xgboost_feature_importance.csv",
  row.names = FALSE
)

cat("\nSaved model: models/xgboost_baseline.rds\n")
cat("Saved tuning, validation, and importance outputs.\n")
cat("\nPHASE 3H COMPLETED\n")
cat("=========================================\n")
