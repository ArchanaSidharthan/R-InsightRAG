
# ============================================================
# R-InsightRAG - Phase 2G
# Historical SEC Dataset Inspection
# ============================================================

library(tidyverse)

project_path <- getwd()
sec_path <- file.path(project_path, "data", "raw", "sec_2018q4")
output_path <- file.path(project_path, "data", "processed")

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Read submission metadata
sub_data <- read.delim(
  file.path(sec_path, "sub.txt"),
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = ""
)

# Inspect filing years and types
cat("\n===== SUBMISSION SUMMARY =====\n")
cat("Total filings:", nrow(sub_data), "\n")
cat("Available filing types:\n")
print(sort(table(sub_data$form), decreasing = TRUE))

cat("\nFiscal years:\n")
print(sort(table(sub_data$fy), decreasing = TRUE))

cat("\nFiling periods:\n")
print(sort(table(sub_data$fp), decreasing = TRUE))

cat("\n===== ANNUAL 10-K FILINGS =====\n")

annual_filings <- sub_data %>%
  filter(form == "10-K", fp == "FY")

cat("Annual 10-K filings:", nrow(annual_filings), "\n")
cat("Unique companies (CIKs):", n_distinct(annual_filings$cik), "\n")

cat("\nSample annual filings:\n")
print(
  annual_filings %>%
    select(cik, name, form, fy, fp, filed, period) %>%
    head(10)
)

# Save inspection summary
summary_data <- tibble(
  metric = c(
    "total_filings",
    "annual_10k_filings",
    "unique_annual_10k_companies"
  ),
  value = c(
    nrow(sub_data),
    nrow(annual_filings),
    n_distinct(annual_filings$cik)
  )
)

write_csv(
  summary_data,
  file.path(output_path, "sec_2018q4_inspection.csv")
)

cat("\nPhase 2G inspection completed.\n")
