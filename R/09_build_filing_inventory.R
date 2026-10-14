
# ============================================================
# R-InsightRAG - Phase 2I
# Cross-Year SEC Filing Inventory
# ============================================================

library(tidyverse)

project_path <- getwd()
raw_path <- file.path(project_path, "data", "raw")
output_path <- file.path(project_path, "data", "processed")

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

years <- 2018:2025

inventory_list <- list()

for (yr in years) {
  
  folder_name <- paste0("sec_", yr, "q4")
  sec_path <- file.path(raw_path, folder_name)
  sub_file <- file.path(sec_path, "sub.txt")
  
  cat("\nInspecting:", folder_name, "\n")
  
  if (!file.exists(sub_file)) {
    warning("SUB file missing for ", yr, ". Skipping.")
    next
  }
  
  sub_data <- read.delim(
    sub_file,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = ""
  )
  
  annual <- sub_data %>%
    filter(
      form %in% c("10-K", "10-K/A"),
      fp == "FY"
    )
  
  inventory_list[[as.character(yr)]] <- tibble(
    archive_year = yr,
    total_filings = nrow(sub_data),
    annual_filings = nrow(annual),
    unique_annual_companies = n_distinct(annual$cik),
    annual_fy_min = if (nrow(annual) > 0) min(annual$fy, na.rm = TRUE) else NA_real_,
    annual_fy_max = if (nrow(annual) > 0) max(annual$fy, na.rm = TRUE) else NA_real_
  )
}

inventory <- bind_rows(inventory_list)

write_csv(
  inventory,
  file.path(output_path, "sec_cross_year_filing_inventory.csv")
)

cat("\n===== CROSS-YEAR INVENTORY =====\n")
print(inventory)

cat("\nSaved to:\n")
cat(file.path(output_path, "sec_cross_year_filing_inventory.csv"), "\n")
