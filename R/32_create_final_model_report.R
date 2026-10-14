
# ============================================================
# R-InsightRAG
# PHASE 3J: FINAL MODEL AUDIT AND PROJECT REPORT
# ============================================================

cat("\n=========================================\n")
cat("PHASE 3J: FINAL MODEL AUDIT\n")
cat("=========================================\n")

# ------------------------------------------------------------
# 1. Define input and output paths
# ------------------------------------------------------------

comparison_path <-
  "outputs/phase3i_final_test_model_comparison.csv"

auc_path <-
  "outputs/phase3i_validation_vs_test_auc.csv"

confusion_path <-
  "outputs/phase3i_test_confusion_matrices.csv"

baseline_path <-
  "outputs/phase3i_majority_class_baseline.csv"

importance_path <-
  "outputs/phase3h_xgboost_feature_importance.csv"

required_files <- c(
  comparison_path,
  auc_path,
  confusion_path,
  baseline_path,
  importance_path,
  "data/processed/train_features_scaled.csv",
  "data/processed/validation_features_scaled.csv",
  "data/processed/test_features_scaled.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Required files are missing:",
      paste(missing_files, collapse = ", ")
    )
  )
}

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 2. Load evaluation results and split metadata
# ------------------------------------------------------------

test_metrics <- read.csv(comparison_path)
auc_comparison <- read.csv(auc_path)
confusion <- read.csv(confusion_path)
baseline <- read.csv(baseline_path)
importance <- read.csv(importance_path)

train <- read.csv(
  "data/processed/train_features_scaled.csv"
)

validation <- read.csv(
  "data/processed/validation_features_scaled.csv"
)

test <- read.csv(
  "data/processed/test_features_scaled.csv"
)

target_col <- "high_growth_next_year"

if (!target_col %in% names(train) ||
    !target_col %in% names(validation) ||
    !target_col %in% names(test)) {
  stop("Target column missing from one or more datasets.")
}

# ------------------------------------------------------------
# 3. Verify split sizes and target distributions
# ------------------------------------------------------------

split_summary <- data.frame(
  split = c("Train", "Validation", "Test"),
  rows = c(nrow(train), nrow(validation), nrow(test)),
  positive = c(
    sum(train[[target_col]] == 1),
    sum(validation[[target_col]] == 1),
    sum(test[[target_col]] == 1)
  ),
  negative = c(
    sum(train[[target_col]] == 0),
    sum(validation[[target_col]] == 0),
    sum(test[[target_col]] == 0)
  )
)

cat("\n--- Dataset split summary ---\n")
print(split_summary, row.names = FALSE)

# ------------------------------------------------------------
# 4. Check required evaluation columns
# ------------------------------------------------------------

required_metrics <- c(
  "model", "threshold", "accuracy", "precision",
  "recall", "specificity", "f1", "roc_auc",
  "TP", "TN", "FP", "FN"
)

if (!all(required_metrics %in% names(test_metrics))) {
  stop("The test metrics file does not contain expected columns.")
}

# ------------------------------------------------------------
# 5. Identify the leading model by test ROC-AUC
# ------------------------------------------------------------

# This is descriptive reporting only.
# Model selection should be based on validation data.
best_test_auc_row <- test_metrics[
  which.max(test_metrics$roc_auc),
]

best_f1_row <- test_metrics[
  which.max(test_metrics$f1),
]

cat("\n--- Highest test ROC-AUC (descriptive only) ---\n")
print(best_test_auc_row, row.names = FALSE)

cat("\n--- Highest test F1 (descriptive only) ---\n")
print(best_f1_row, row.names = FALSE)

# ------------------------------------------------------------
# 6. Build a compact comparison table
# ------------------------------------------------------------

comparison_table <- merge(
  auc_comparison,
  test_metrics,
  by = "model",
  suffixes = c("_auc", "_test")
)

comparison_table <- comparison_table[
  order(-comparison_table$validation_roc_auc),
]

cat("\n--- Validation and test comparison ---\n")
print(comparison_table, row.names = FALSE)

# ------------------------------------------------------------
# 7. Create audit notes
# ------------------------------------------------------------

audit_notes <- c(
  "R-InsightRAG: Final Model Audit",
  "================================",
  "",
  "Target definition:",
  paste(
    "high_growth_next_year = 1 when next fiscal year's",
    "revenue growth is at least 10%; otherwise 0."
  ),
  "",
  "Data split:",
  paste(
    "Training: ", nrow(train), " rows.",
    "Validation: ", nrow(validation), " rows.",
    "Test: ", nrow(test), " rows."
  ),
  "",
  "Leakage safeguards:",
  "- Time-based training, validation and test splits.",
  "- Preprocessing parameters estimated from training data.",
  "- Classification thresholds selected using validation data.",
  "- Test set used for final evaluation only.",
  "",
  "Evaluation interpretation:",
  "- ROC-AUC measures ranking discrimination.",
  "- Precision, recall, F1 and specificity depend on threshold.",
  "- Accuracy must be interpreted alongside the majority baseline.",
  "",
  "Limitations:",
  "- The dataset contains a relatively small number of companies.",
  "- SEC revenue tags can differ across issuers and years.",
  "- A single held-out period does not establish future robustness.",
  "- The validation set was used for model and threshold comparisons.",
  "- Test results should not be used for additional model tuning.",
  "",
  "Recommended future work:",
  "- Audit fiscal-year revenue tags and extreme revenue changes.",
  "- Run rolling-origin temporal validation.",
  "- Test probability calibration and threshold stability.",
  "- Consider company-held-out sensitivity analysis.",
  "- Assess feature importance stability across time periods.",
  "",
  "This report is a model evaluation summary, not investment advice."
)

writeLines(
  audit_notes,
  "outputs/phase3j_model_audit_notes.txt"
)

# ------------------------------------------------------------
# 8. Save consolidated outputs
# ------------------------------------------------------------

write.csv(
  split_summary,
  "outputs/phase3j_dataset_split_summary.csv",
  row.names = FALSE
)

write.csv(
  comparison_table,
  "outputs/phase3j_consolidated_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  best_test_auc_row,
  "outputs/phase3j_best_test_auc_model.csv",
  row.names = FALSE
)

write.csv(
  importance,
  "outputs/phase3j_xgboost_feature_importance.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 9. Generate a Markdown report
# ------------------------------------------------------------

report_path <- "outputs/phase3j_final_model_report.md"

report_lines <- c(
  "# R-InsightRAG: Final Model Evaluation",
  "",
  "## Objective",
  "",
  paste(
    "Predict whether a company's next fiscal-year revenue growth",
    "will reach or exceed 10% using current-year financial features."
  ),
  "",
  "## Dataset splits",
  "",
  "| Split | Rows | Positive | Negative |",
  "|---|---:|---:|---:|",
  paste0(
    "| ", split_summary$split,
    " | ", split_summary$rows,
    " | ", split_summary$positive,
    " | ", split_summary$negative, " |"
  ),
  "",
  "## Test performance",
  "",
  "| Model | Threshold | Accuracy | Precision | Recall | Specificity | F1 | ROC-AUC |",
  "|---|---:|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(test_metrics))) {
  r <- test_metrics[i, ]
  
  report_lines <- c(
    report_lines,
    sprintf(
      "| %s | %.2f | %.4f | %.4f | %.4f | %.4f | %.4f | %.4f |",
      r$model,
      r$threshold,
      r$accuracy,
      r$precision,
      r$recall,
      r$specificity,
      r$f1,
      r$roc_auc
    )
  )
}

baseline_accuracy <- baseline$accuracy[1]

report_lines <- c(
  report_lines,
  "",
  "## Majority-class baseline",
  "",
  sprintf(
    "The majority-class baseline achieved %.4f accuracy on the test set.",
    baseline_accuracy
  ),
  "",
  "## XGBoost feature importance",
  "",
  "The following table contains the importance values generated by XGBoost.",
  "",
  "| Feature | Gain | Cover | Frequency |",
  "|---|---:|---:|---:|"
)

if (all(c("Feature", "Gain", "Cover", "Frequency") %in% names(importance))) {
  top_importance <- head(importance, 10)
  
  for (i in seq_len(nrow(top_importance))) {
    r <- top_importance[i, ]
    
    report_lines <- c(
      report_lines,
      sprintf(
        "| %s | %.4f | %.4f | %.4f |",
        r$Feature, r$Gain, r$Cover, r$Frequency
      )
    )
  }
}

report_lines <- c(
  report_lines,
  "",
  "## Interpretation",
  "",
  paste(
    "Results should be interpreted across multiple metrics.",
    "A model can identify more high-growth cases while producing",
    "many false positives. The majority-class baseline may achieve",
    "higher accuracy while failing to identify any positive cases."
  ),
  "",
  "## Limitations and next steps",
  "",
  "- Investigate SEC revenue-tag consistency and extreme growth values.",
  "- Use rolling-origin temporal validation to assess stability.",
  "- Evaluate threshold stability and probability calibration.",
  "- Consider company-held-out sensitivity analysis.",
  "- Do not tune models using the final test set.",
  "",
  "This report is a retrospective model evaluation, not investment advice."
)

writeLines(report_lines, report_path)

cat("\n--- Files saved ---\n")
cat("outputs/phase3j_dataset_split_summary.csv\n")
cat("outputs/phase3j_consolidated_model_comparison.csv\n")
cat("outputs/phase3j_best_test_auc_model.csv\n")
cat("outputs/phase3j_xgboost_feature_importance.csv\n")
cat("outputs/phase3j_model_audit_notes.txt\n")
cat("outputs/phase3j_final_model_report.md\n")

cat("\nPHASE 3J COMPLETED\n")
cat("=========================================\n")
