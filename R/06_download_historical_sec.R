
# ============================================================
# R-InsightRAG
# Phase 2F - Historical SEC Dataset Collection
# ============================================================

library(tidyverse)

project_path <- getwd()

raw_path <- file.path(project_path, "data", "raw")
external_path <- file.path(project_path, "data", "external")

dir.create(raw_path, recursive = TRUE, showWarnings = FALSE)
dir.create(external_path, recursive = TRUE, showWarnings = FALSE)

# Start with ONE historical quarter to test the pipeline.
# We will expand to 2018-2025 after verification.
dataset_url <- paste0(
  "https://www.sec.gov/files/dera/data/",
  "financial-statement-data-sets/2018q4.zip"
)

zip_path <- file.path(raw_path, "2018q4.zip")
extract_path <- file.path(raw_path, "sec_2018q4")

cat("Downloading historical SEC dataset...\n")
cat("URL:", dataset_url, "\n")


download.file(
  url = dataset_url,
  destfile = zip_path,
  mode = "wb",
  method = "libcurl",
  headers = c(
    "User-Agent" = "R-InsightRAG your-real-email@example.com"
  ),
  quiet = FALSE
)


if (!file.exists(zip_path) || file.info(zip_path)$size == 0) {
  stop("Download failed or the ZIP file is empty.")
}

cat("Download completed.\n")
cat("File size (MB):",
    round(file.info(zip_path)$size / 1024^2, 2),
    "\n")

dir.create(
  extract_path,
  recursive = TRUE,
  showWarnings = FALSE
)

unzip(zip_path, exdir = extract_path)

required_files <- c(
  "sub.txt",
  "num.txt",
  "tag.txt",
  "pre.txt"
)

found_files <- file.exists(
  file.path(extract_path, required_files)
)

if (!all(found_files)) {
  stop("The extracted archive is missing required SEC files.")
}

cat("\nHistorical SEC archive verified.\n")
cat("Extracted folder:", extract_path, "\n")
cat("Required files found:\n")
print(required_files)

write_csv(
  tibble(
    year = 2018,
    quarter = "Q4",
    archive = zip_path,
    extracted_folder = extract_path,
    status = "verified"
  ),
  file.path(external_path, "historical_sec_download_log.csv")
)
