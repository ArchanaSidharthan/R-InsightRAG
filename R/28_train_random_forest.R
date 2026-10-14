
# ============================================================
# R-InsightRAG
# PHASE 3F: RANDOM FOREST MODEL
# ============================================================

cat("\n=========================================\n")
cat("PHASE 3F: RANDOM FOREST MODEL\n")
cat("=========================================\n")

# Install once if needed
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest",
                   repos = "https://cloud.r-project.org")
}

library(randomForest)
library(pROC)

set.seed(42)

# ------------------------------------------------------------
# 1. Load training and validation datasets ONLY
# ------------------------------------------------------------

train <- read.csv(
  "data/processed/train_features_scaled.csv"
)

validation <- read.csv(
  "data/processed/validation_features_scaled.csv"
)

target_col <- "high_growth_next_year"

if (!target_col %in% names(train) ||
    !target_col %in% names(validation)) {
  stop("Target column is missing.")
}

# Use identical predictors in both datasets
predictors <- setdiff(names(train), target_col)

if (!setequal(predictors, setdiff(names(validation), target_col))) {
  stop("Training and validation predictor columns do not match.")
}

validation <- validation[, c(predictors, target_col)]

# Convert target to a factor with consistent class ordering
train[[target_col]] <- factor(
  as.character(train[[target_col]]),
  levels = c("0", "1")
)

validation[[target_col]] <- factor(
  as.character(validation[[target_col]]),
  levels = c("0", "1")
)

if (anyNA(train[[target_col]]) ||
    anyNA(validation[[target_col]])) {
  stop("Target contains missing or unexpected values.")
}

# Ensure predictors are numeric and contain no missing values
for (column in predictors) {
  if (!is.numeric(train[[column]]) ||
      !is.numeric(validation[[column]])) {
    stop(paste("Non-numeric predictor:", column))
  }
  
  if (anyNA(train[[column]]) ||
      anyNA(validation[[column]])) {
    stop(paste("Missing predictor values in:", column))
  }
}

cat("\nTraining rows:", nrow(train), "\n")
cat("Validation rows:", nrow(validation), "\n")
cat("Number of predictors:", length(predictors), "\n")

# ------------------------------------------------------------
# 2. Tune mtry using validation ROC-AUC
# ------------------------------------------------------------

# mtry = number of predictors randomly considered per split
mtry_values <- unique(
  pmax(
    1,
    pmin(
      length(predictors),
      c(2, 4, 6, 8, floor(sqrt(length(predictors))))
    )
  )
)

tuning_results <- data.frame()

for (m in mtry_values) {
  
  cat("\nTraining Random Forest with mtry =", m, "\n")
  
  rf_model <- randomForest(
    x = train[, predictors, drop = FALSE],
    y = train[[target_col]],
    ntree = 500,
    mtry = m,
    importance = TRUE
  )
  
  validation_prob <- predict(
    rf_model,
    newdata = validation[, predictors, drop = FALSE],
    type = "prob"
  )[, "1"]
  
  validation_auc <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = as.numeric(as.character(
          validation[[target_col]]
        )),
        predictor = validation_prob,
        levels = c(0, 1),
        direction = "<",
        quiet = TRUE
      )
    )
  )
  
  tuning_results <- rbind(
    tuning_results,
    data.frame(
      mtry = m,
      validation_roc_auc = validation_auc
    )
  )
}

tuning_results <- tuning_results[
  order(-tuning_results$validation_roc_auc),
]

cat("\n--- Random Forest tuning results ---\n")
print(tuning_results, row.names = FALSE)

best_mtry <- tuning_results$mtry[1]

cat("\nBest mtry:", best_mtry, "\n")

# ------------------------------------------------------------
# 3. Train final Random Forest using selected mtry
# ------------------------------------------------------------

set.seed(42)

final_model <- randomForest(
  x = train[, predictors, drop = FALSE],
  y = train[[target_col]],
  ntree = 500,
  mtry = best_mtry,
  importance = TRUE
)

# Validation probabilities only; do not evaluate test here
validation_prob <- predict(
  final_model,
  newdata = validation[, predictors, drop = FALSE],
  type = "prob"
)[, "1"]

actual <- as.numeric(as.character(validation[[target_col]]))

# ------------------------------------------------------------
# 4. Calculate validation metrics
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

validation_auc <- as.numeric(
  pROC::auc(
    pROC::roc(
      response = actual,
      predictor = validation_prob,
      levels = c(0, 1),
      direction = "<",
      quiet = TRUE
    )
  )
)

thresholds <- seq(0.20, 0.80, by = 0.01)

threshold_results <- do.call(
  rbind,
  lapply(thresholds, function(threshold) {
    calculate_metrics(
      actual,
      ifelse(validation_prob >= threshold, 1, 0),
      threshold
    )
  })
)

# Select the best F1 threshold; use precision to break ties
threshold_results <- threshold_results[
  order(
    -threshold_results$f1,
    -threshold_results$precision
  ),
]

best_threshold <- threshold_results$threshold[1]

validation_metrics <- calculate_metrics(
  actual,
  ifelse(validation_prob >= best_threshold, 1, 0),
  best_threshold
)

validation_metrics$roc_auc <- validation_auc
validation_metrics$mtry <- best_mtry

cat("\n--- Random Forest validation performance ---\n")
print(validation_metrics, row.names = FALSE)

cat("\n--- Top 10 thresholds by F1 ---\n")
print(head(threshold_results, 10), row.names = FALSE)

# ------------------------------------------------------------
# 5. Feature importance
# ------------------------------------------------------------

importance_table <- as.data.frame(
  randomForest::importance(final_model)
)

importance_table$feature <- rownames(importance_table)

importance_column <- if (
  "%IncMSE" %in% names(importance_table)
) {
  "%IncMSE"
} else {
  "MeanDecreaseGini"
}

importance_table <- importance_table[
  order(-importance_table[[importance_column]]),
]

cat("\n--- Top 10 important features ---\n")
print(head(importance_table, 10), row.names = FALSE)

# ------------------------------------------------------------
# 6. Save model and validation outputs
# ------------------------------------------------------------

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
dir.create("models", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  final_model,
  "models/random_forest_baseline.rds"
)

write.csv(
  tuning_results,
  "outputs/phase3f_random_forest_tuning.csv",
  row.names = FALSE
)

write.csv(
  validation_metrics,
  "outputs/phase3f_random_forest_validation_metrics.csv",
  row.names = FALSE
)

write.csv(
  threshold_results,
  "outputs/phase3f_random_forest_thresholds.csv",
  row.names = FALSE
)

write.csv(
  importance_table,
  "outputs/phase3f_random_forest_feature_importance.csv",
  row.names = FALSE
)

cat("\nSaved model: models/random_forest_baseline.rds\n")
cat("Saved validation metrics and feature importance.\n")
cat("\nPHASE 3F COMPLETED\n")
cat("=========================================\n")
