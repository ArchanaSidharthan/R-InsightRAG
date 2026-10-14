
# ============================================================
# R-InsightRAG
# PHASE 3G: SUPPORT VECTOR MACHINE (SVM)
# ============================================================

cat("\n=========================================\n")
cat("PHASE 3G: SUPPORT VECTOR MACHINE\n")
cat("=========================================\n")

# Install packages if needed
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071", repos = "https://cloud.r-project.org")
}

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC", repos = "https://cloud.r-project.org")
}

library(e1071)
library(pROC)

set.seed(42)

# ------------------------------------------------------------
# 1. Load training and validation datasets
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

predictors <- setdiff(names(train), target_col)

if (!setequal(predictors, setdiff(names(validation), target_col))) {
  stop("Training and validation predictor columns do not match.")
}

validation <- validation[, c(predictors, target_col)]

# Ensure target classes are consistently ordered
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

for (column in predictors) {
  if (!is.numeric(train[[column]]) ||
      !is.numeric(validation[[column]])) {
    stop(paste("Non-numeric predictor:", column))
  }
  
  if (anyNA(train[[column]]) ||
      anyNA(validation[[column]])) {
    stop(paste("Missing values in predictor:", column))
  }
}

cat("\nTraining rows:", nrow(train), "\n")
cat("Validation rows:", nrow(validation), "\n")
cat("Predictors:", length(predictors), "\n")

# ------------------------------------------------------------
# 2. Define validation ROC-AUC function
# ------------------------------------------------------------

actual <- as.numeric(as.character(validation[[target_col]]))

calculate_auc <- function(actual, probabilities) {
  
  roc_object <- pROC::roc(
    response = actual,
    predictor = probabilities,
    levels = c(0, 1),
    direction = "<",
    quiet = TRUE
  )
  
  as.numeric(pROC::auc(roc_object))
}

# ------------------------------------------------------------
# 3. Tune cost and gamma using validation ROC-AUC
# ------------------------------------------------------------

# A compact parameter grid suitable for this dataset
cost_values <- c(0.1, 1, 10, 100)
gamma_values <- c(0.01, 0.05, 0.1, 0.5)

tuning_results <- data.frame()

for (cost_value in cost_values) {
  for (gamma_value in gamma_values) {
    
    cat(
      "\nTraining SVM: cost =",
      cost_value,
      "| gamma =",
      gamma_value,
      "\n"
    )
    
    svm_model <- e1071::svm(
      x = train[, predictors, drop = FALSE],
      y = train[[target_col]],
      type = "C-classification",
      kernel = "radial",
      cost = cost_value,
      gamma = gamma_value,
      probability = TRUE,
      scale = FALSE
    )
    
    validation_prediction <- predict(
      svm_model,
      newdata = validation[, predictors, drop = FALSE],
      probability = TRUE
    )
    
    probability_matrix <- attr(
      validation_prediction,
      "probabilities"
    )
    
    positive_column <- which(
      colnames(probability_matrix) == "1"
    )
    
    if (length(positive_column) != 1) {
      stop("Could not identify the SVM probability for class 1.")
    }
    
    validation_prob <- probability_matrix[, positive_column]
    
    validation_auc <- calculate_auc(
      actual,
      validation_prob
    )
    
    tuning_results <- rbind(
      tuning_results,
      data.frame(
        cost = cost_value,
        gamma = gamma_value,
        validation_roc_auc = validation_auc
      )
    )
  }
}

tuning_results <- tuning_results[
  order(-tuning_results$validation_roc_auc),
]

cat("\n--- SVM tuning results ---\n")
print(tuning_results, row.names = FALSE)

best_cost <- tuning_results$cost[1]
best_gamma <- tuning_results$gamma[1]

cat("\nBest cost:", best_cost, "\n")
cat("Best gamma:", best_gamma, "\n")

# ------------------------------------------------------------
# 4. Train the selected SVM model
# ------------------------------------------------------------

set.seed(42)

best_model <- e1071::svm(
  x = train[, predictors, drop = FALSE],
  y = train[[target_col]],
  type = "C-classification",
  kernel = "radial",
  cost = best_cost,
  gamma = best_gamma,
  probability = TRUE,
  scale = FALSE
)

validation_prediction <- predict(
  best_model,
  newdata = validation[, predictors, drop = FALSE],
  probability = TRUE
)

probability_matrix <- attr(
  validation_prediction,
  "probabilities"
)

validation_prob <- probability_matrix[, "1"]

validation_auc <- calculate_auc(actual, validation_prob)

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
      actual,
      ifelse(validation_prob >= threshold, 1, 0),
      threshold
    )
  })
)

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
validation_metrics$cost <- best_cost
validation_metrics$gamma <- best_gamma

cat("\n--- SVM validation performance ---\n")
print(validation_metrics, row.names = FALSE)

cat("\n--- Top 10 thresholds by F1 ---\n")
print(head(threshold_results, 10), row.names = FALSE)

# ------------------------------------------------------------
# 6. Save model and outputs
# ------------------------------------------------------------

dir.create("models", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  best_model,
  "models/svm_baseline.rds"
)

write.csv(
  tuning_results,
  "outputs/phase3g_svm_tuning.csv",
  row.names = FALSE
)

write.csv(
  validation_metrics,
  "outputs/phase3g_svm_validation_metrics.csv",
  row.names = FALSE
)

write.csv(
  threshold_results,
  "outputs/phase3g_svm_thresholds.csv",
  row.names = FALSE
)

cat("\nSaved model: models/svm_baseline.rds\n")
cat("Saved SVM tuning and validation results.\n")
cat("\nPHASE 3G COMPLETED\n")
cat("=========================================\n")
