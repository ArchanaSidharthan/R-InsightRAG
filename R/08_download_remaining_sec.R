
# ============================================================
# R-InsightRAG - Phase 2H
# Download SEC Q4 archives for 2019-2024
# ============================================================

library(tidyverse)

project_path <- getwd()
raw_path <- file.path(project_path, "data", "raw")
log_path <- file.path(
  project_path,
  "data", "external",
  "historical_sec_download_log.csv"
)

dir.create(raw_path, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

# Replace with your real contact email
user_agent <- "R-InsightRAG your-real-email@example.com"

years <- 2019:2024
download_log <- list()

for (yr in years) {
  
  quarter <- paste0(yr, "q4")
  zip_path <- file.path(raw_path, paste0(quarter, ".zip"))
  extract_path <- file.path(raw_path, paste0("sec_", quarter))
  
  dataset_url <- paste0(
    "https://www.sec.gov/files/dera/data/",
    "financial-statement-data-sets/",
    quarter, ".zip"
  )
  
  cat("\n====================================\n")
  cat("Processing:", quarter, "\n")
  cat("====================================\n")
  
  status <- "failed"
  message_text <- ""
  
  tryCatch({
    
    # Download only if the ZIP does not already exist
    if (!file.exists(zip_path) ||
        file.info(zip_path)$size == 0) {
      
      download.file(
        url = dataset_url,
        destfile = zip_path,
        mode = "wb",
        method = "libcurl",
        headers = c("User-Agent" = user_agent),
        quiet = FALSE
      )
    }
    
    if (!file.exists(zip_path) ||
        file.info(zip_path)$size == 0) {
      stop("ZIP file is missing or empty.")
    }
    
    dir.create(
      extract_path,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    unzip(zip_path, exdir = extract_path)
    
    required_files <- c(
      "sub.txt", "num.txt", "tag.txt", "pre.txt"
    )
    
    if (!all(file.exists(
      file.path(extract_path, required_files)
    ))) {
      stop("One or more required SEC files are missing.")
    }
    
    status <- "verified"
    message_text <- "Download and extraction verified"
    
    cat("SUCCESS:", quarter, "\n")
    
  }, error = function(e) {
    
    message_text <<- conditionMessage(e)
    cat("FAILED:", quarter, "\n")
    cat("Reason:", message_text, "\n")
  })
  
  download_log[[length(download_log) + 1]] <- tibble(
    year = yr,
    quarter = "Q4",
    url = dataset_url,
    zip_path = zip_path,
    status = status,
    message = message_text
  )
}

log_df <- bind_rows(download_log)

write_csv(log_df, log_path)

cat("\n===== DOWNLOAD SUMMARY =====\n")
print(log_df %>% select(year, status, message))

cat("\nLog saved to:\n", log_path, "\n")
