# ============================================================
# R-InsightRAG
# Phase 2B - SEC Financial Dataset Inspection
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Project paths
# ------------------------------------------------------------

project_path <- getwd()

raw_path <- file.path(project_path, "data", "raw")
processed_path <- file.path(project_path, "data", "processed")

dir.create(raw_path, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_path, recursive = TRUE, showWarnings = FALSE)

cat("============================================\n")
cat("       R-InsightRAG - PHASE 2B\n")
cat("       SEC DATASET INSPECTION\n")
cat("============================================\n\n")

cat("Project path:\n")
cat(project_path, "\n\n")


# ------------------------------------------------------------
# 2. Locate SEC ZIP file
# ------------------------------------------------------------

zip_file <- file.path(
  raw_path,
  "2025q4.zip"
)

if (!file.exists(zip_file)) {
  
  stop(
    paste0(
      "\nSEC ZIP file not found.\n\n",
      "Expected location:\n",
      zip_file,
      "\n\n",
      "Download 2025 Q4 SEC dataset first."
    )
  )
  
}

cat("SEC ZIP file found:\n")
cat(zip_file, "\n\n")


# ------------------------------------------------------------
# 3. Display ZIP contents
# ------------------------------------------------------------

cat("============================================\n")
cat("ZIP CONTENTS\n")
cat("============================================\n\n")

zip_contents <- unzip(
  zip_file,
  list = TRUE
)

print(zip_contents)


# ------------------------------------------------------------
# 4. Create extraction directory
# ------------------------------------------------------------

extract_path <- file.path(
  raw_path,
  "sec_2025q4"
)

dir.create(
  extract_path,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 5. Extract SEC dataset
# ------------------------------------------------------------

cat("\n============================================\n")
cat("EXTRACTING SEC DATA\n")
cat("============================================\n\n")

unzip(
  zip_file,
  exdir = extract_path
)

cat("Extraction completed.\n\n")


# ------------------------------------------------------------
# 6. Check extracted files
# ------------------------------------------------------------

cat("============================================\n")
cat("EXTRACTED FILES\n")
cat("============================================\n\n")

extracted_files <- list.files(
  extract_path,
  recursive = TRUE,
  full.names = TRUE
)

print(extracted_files)


# ------------------------------------------------------------
# 7. Identify SEC files
# ------------------------------------------------------------

sub_file <- list.files(
  extract_path,
  pattern = "^sub\\.txt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

num_file <- list.files(
  extract_path,
  pattern = "^num\\.txt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

tag_file <- list.files(
  extract_path,
  pattern = "^tag\\.txt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

pre_file <- list.files(
  extract_path,
  pattern = "^pre\\.txt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


# ------------------------------------------------------------
# 8. Print locations
# ------------------------------------------------------------

cat("\n============================================\n")
cat("SEC FILE LOCATIONS\n")
cat("============================================\n\n")

cat("SUB:\n")
print(sub_file)

cat("\nNUM:\n")
print(num_file)

cat("\nTAG:\n")
print(tag_file)

cat("\nPRE:\n")
print(pre_file)


# ------------------------------------------------------------
# 9. Read SUB
# ------------------------------------------------------------

if (length(sub_file) > 0) {
  
  cat("\n============================================\n")
  cat("READING SUB FILE\n")
  cat("============================================\n\n")
  
  sub_data <- read.delim(
    sub_file[1],
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = ""
  )
  
  cat("Rows:", nrow(sub_data), "\n")
  cat("Columns:", ncol(sub_data), "\n\n")
  
  cat("Column names:\n")
  print(names(sub_data))
  
  cat("\nFirst 5 rows:\n")
  print(head(sub_data, 5))
  
}


# ------------------------------------------------------------
# 10. Read NUM
# ------------------------------------------------------------

if (length(num_file) > 0) {
  
  cat("\n============================================\n")
  cat("READING NUM FILE\n")
  cat("============================================\n\n")
  
  num_data <- read.delim(
    num_file[1],
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = ""
  )
  
  cat("Rows:", nrow(num_data), "\n")
  cat("Columns:", ncol(num_data), "\n\n")
  
  cat("Column names:\n")
  print(names(num_data))
  
  cat("\nFirst 5 rows:\n")
  print(head(num_data, 5))
  
}


# ------------------------------------------------------------
# 11. Read TAG
# ------------------------------------------------------------

if (length(tag_file) > 0) {
  
  cat("\n============================================\n")
  cat("READING TAG FILE\n")
  cat("============================================\n\n")
  
  tag_data <- read.delim(
    tag_file[1],
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = ""
  )
  
  cat("Rows:", nrow(tag_data), "\n")
  cat("Columns:", ncol(tag_data), "\n\n")
  
  cat("Column names:\n")
  print(names(tag_data))
  
}


# ------------------------------------------------------------
# 12. Read PRE
# ------------------------------------------------------------

if (length(pre_file) > 0) {
  
  cat("\n============================================\n")
  cat("READING PRE FILE\n")
  cat("============================================\n\n")
  
  pre_data <- read.delim(
    pre_file[1],
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = ""
  )
  
  cat("Rows:", nrow(pre_data), "\n")
  cat("Columns:", ncol(pre_data), "\n\n")
  
  cat("Column names:\n")
  print(names(pre_data))
  
}


# ------------------------------------------------------------
# 13. Save basic inspection information
# ------------------------------------------------------------

inspection <- tibble(
  file = c(
    "SUB",
    "NUM",
    "TAG",
    "PRE"
  ),
  available = c(
    length(sub_file) > 0,
    length(num_file) > 0,
    length(tag_file) > 0,
    length(pre_file) > 0
  )
)

write_csv(
  inspection,
  file.path(
    processed_path,
    "sec_2025q4_file_check.csv"
  )
)

cat("\n============================================\n")
cat("PHASE 2B COMPLETED\n")
cat("============================================\n\n")

cat(
  "Inspection report saved to:\n",
  file.path(
    processed_path,
    "sec_2025q4_file_check.csv"
  ),
  "\n"
)