
# ============================================================
# R-InsightRAG - Phase 5C
# Download SEC Annual Report Documents
# ============================================================

library(httr)
library(readr)
library(dplyr)
library(stringr)

# 1. Project configuration
project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

input_file <- "data/processed/sec_annual_report_documents.csv"

download_dir <- "documents/annual_reports"
manifest_file <- "data/processed/sec_download_manifest.csv"
error_file <- "outputs/phase5c_download_errors.csv"

dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

# Replace with your own contact email
sec_user_agent <- "R-InsightRAG research your-email@example.com"

# 2. Read and validate the inventory
if (!file.exists(input_file)) {
  stop("Annual-report inventory not found: ", input_file)
}

reports <- read_csv(input_file, show_col_types = FALSE)

required_columns <- c(
  "company_id",
  "company_name",
  "fiscal_year",
  "accession_number",
  "primary_document_url"
)

missing_columns <- setdiff(required_columns, names(reports))

if (length(missing_columns) > 0) {
  stop(
    "Missing inventory columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 3. Create safe local filenames
safe_text <- function(x) {
  x <- str_replace_all(x, "[^A-Za-z0-9_-]", "_")
  str_sub(x, 1, 70)
}

reports <- reports %>%
  mutate(
    local_filename = paste0(
      safe_text(as.character(company_id)), "_",
      safe_text(as.character(fiscal_year)), "_",
      str_remove_all(as.character(accession_number), "-"),
      ".html"
    ),
    local_path = file.path(download_dir, local_filename)
  )

# 4. Resume safely if a previous manifest exists
if (file.exists(manifest_file)) {
  
  old_manifest <- read_csv(
    manifest_file,
    show_col_types = FALSE
  )
  
  if (all(c("local_filename", "download_status") %in%
          names(old_manifest))) {
    
    reports <- reports %>%
      left_join(
        old_manifest %>%
          select(
            local_filename,
            previous_status = download_status
          ) %>%
          distinct(local_filename, .keep_all = TRUE),
        by = "local_filename"
      )
    
  } else {
    reports$previous_status <- NA_character_
  }
  
} else {
  reports$previous_status <- NA_character_
}

# Initialise status columns
reports$download_status <- "Pending"
reports$http_status <- NA_integer_
reports$file_size_bytes <- NA_real_
reports$downloaded_at <- NA_character_

# Keep any previously recorded status for existing files.
for (i in seq_len(nrow(reports))) {
  
  if (
    file.exists(reports$local_path[i]) &&
    !is.na(file.info(reports$local_path[i])$size) &&
    file.info(reports$local_path[i])$size > 0
  ) {
    reports$download_status[i] <- "Downloaded"
    reports$file_size_bytes[i] <- file.info(
      reports$local_path[i]
    )$size
  }
}

# 5. Download documents
ua <- user_agent(sec_user_agent)

cat("\n============================================\n")
cat("PHASE 5C: SEC DOCUMENT DOWNLOAD\n")
cat("============================================\n")
cat("Inventory records:", nrow(reports), "\n")
cat("Already downloaded:", sum(reports$download_status == "Downloaded"), "\n")
cat("Destination:", normalizePath(download_dir), "\n\n")

for (i in seq_len(nrow(reports))) {
  
  # Skip a valid local file
  if (reports$download_status[i] == "Downloaded") {
    next
  }
  
  url <- as.character(reports$primary_document_url[i])
  
  if (is.na(url) || !nzchar(url)) {
    reports$download_status[i] <- "Missing document URL"
    
  } else {
    
    response <- tryCatch(
      GET(
        url,
        ua,
        timeout(60),
        config(followlocation = TRUE)
      ),
      error = function(e) e
    )
    
    if (inherits(response, "error")) {
      
      reports$download_status[i] <- paste0(
        "Request error: ",
        conditionMessage(response)
      )
      
    } else {
      
      status <- status_code(response)
      reports$http_status[i] <- status
      
      if (status == 200) {
        
        file_bytes <- tryCatch(
          content(response, as = "raw"),
          error = function(e) raw(0)
        )
        
        if (length(file_bytes) > 0) {
          
          # Write to a temporary file first, then rename.
          temp_path <- paste0(reports$local_path[i], ".part")
          
          writeBin(file_bytes, temp_path)
          
          if (
            file.exists(temp_path) &&
            file.info(temp_path)$size > 0
          ) {
            
            if (file.exists(reports$local_path[i])) {
              file.remove(reports$local_path[i])
            }
            
            renamed <- file.rename(
              temp_path,
              reports$local_path[i]
            )
            
            if (renamed) {
              
              reports$download_status[i] <- "Downloaded"
              reports$file_size_bytes[i] <- length(file_bytes)
              reports$downloaded_at[i] <- format(
                Sys.time(),
                "%Y-%m-%d %H:%M:%S"
              )
              
            } else {
              reports$download_status[i] <- "Could not finalize file"
              if (file.exists(temp_path)) {
                file.remove(temp_path)
              }
            }
            
          } else {
            reports$download_status[i] <- "Empty download"
            if (file.exists(temp_path)) {
              file.remove(temp_path)
            }
          }
          
        } else {
          reports$download_status[i] <- "Empty response"
        }
        
      } else {
        reports$download_status[i] <- paste0("HTTP ", status)
      }
    }
  }
  
  # Print progress and save checkpoint every 25 records.
  if (i %% 25 == 0 || i == nrow(reports)) {
    
    cat(
      "Processed", i, "of", nrow(reports),
      "| downloaded:",
      sum(reports$download_status == "Downloaded"),
      "\n"
    )
    
    write_csv(
      reports %>% select(-previous_status),
      manifest_file,
      na = ""
    )
  }
  
  # Stay well below the SEC general request-rate limit.
  Sys.sleep(0.2)
}

# 6. Save final manifest
final_manifest <- reports %>% select(-previous_status)

write_csv(final_manifest, manifest_file, na = "")

# Save failures separately for review and retries.
failures <- final_manifest %>%
  filter(download_status != "Downloaded")

write_csv(failures, error_file, na = "")

# 7. Final summary
cat("\n============================================\n")
cat("PHASE 5C DOWNLOAD SUMMARY\n")
cat("============================================\n")

cat("Total inventory records:", nrow(final_manifest), "\n")
cat(
  "Successfully downloaded:",
  sum(final_manifest$download_status == "Downloaded"),
  "\n"
)
cat(
  "Not downloaded:",
  sum(final_manifest$download_status != "Downloaded"),
  "\n"
)

cat("\nDownload statuses:\n")
print(as.data.frame(
  final_manifest %>% count(download_status, sort = TRUE)
))

cat("\nManifest saved to:", manifest_file, "\n")
cat("Failure report saved to:", error_file, "\n")
cat("Report files stored in:", normalizePath(download_dir), "\n")
cat("\nPhase 5C completed.\n")
