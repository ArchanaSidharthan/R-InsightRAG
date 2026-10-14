
# ============================================================
# R-InsightRAG - Phase 5D
# Extract and validate SEC annual report text
# ============================================================

library(xml2)
library(rvest)
library(readr)
library(dplyr)
library(stringr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

manifest_file <- "data/processed/sec_download_manifest.csv"
text_dir <- "documents/extracted_text"
output_manifest <- "data/processed/sec_text_extraction_manifest.csv"
quality_report <- "outputs/phase5d_text_quality_summary.csv"

dir.create(text_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

if (!file.exists(manifest_file)) {
  stop("Download manifest not found: ", manifest_file)
}

manifest <- read_csv(manifest_file, show_col_types = FALSE)

required <- c(
  "company_id",
  "company_name",
  "fiscal_year",
  "accession_number",
  "local_path",
  "download_status"
)

missing_columns <- setdiff(required, names(manifest))

if (length(missing_columns) > 0) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "))
}

# Keep only successfully downloaded records.
manifest <- manifest %>%
  filter(download_status == "Downloaded")

# Extract visible text from HTML.
extract_visible_text <- function(path) {
  
  doc <- read_html(path, encoding = "UTF-8")
  
  # Remove scripts, styles, navigation, and other non-report content.
  removable <- c(
    "script", "style", "noscript", "svg",
    "iframe", "nav"
  )
  
  for (tag in removable) {
    nodes <- xml_find_all(doc, paste0("//", tag))
    if (length(nodes) > 0) {
      xml_remove(nodes)
    }
  }
  
  text <- html_text2(doc)
  
  # Normalize whitespace.
  text <- str_replace_all(text, "\u00a0", " ")
  text <- str_replace_all(text, "[ \t]+", " ")
  text <- str_replace_all(text, "\\n[ \t]+", "\n")
  text <- str_replace_all(text, "\\n{3,}", "\n\n")
  text <- str_trim(text)
  
  text
}

manifest$extracted_text_path <- NA_character_
manifest$extraction_status <- "Pending"
manifest$character_count <- NA_integer_
manifest$word_count <- NA_integer_
manifest$contains_financial_terms <- NA
manifest$extraction_error <- NA_character_

cat("\n============================================\n")
cat("PHASE 5D: TEXT EXTRACTION\n")
cat("============================================\n")
cat("Downloaded reports:", nrow(manifest), "\n\n")

for (i in seq_len(nrow(manifest))) {
  
  input_path <- as.character(manifest$local_path[i])
  
  safe_company <- str_replace_all(
    as.character(manifest$company_id[i]),
    "[^A-Za-z0-9_-]",
    "_"
  )
  
  safe_year <- str_replace_all(
    as.character(manifest$fiscal_year[i]),
    "[^A-Za-z0-9_-]",
    "_"
  )
  
  accession_safe <- str_remove_all(
    as.character(manifest$accession_number[i]),
    "-"
  )
  
  output_path <- file.path(
    text_dir,
    paste0(
      safe_company, "_",
      safe_year, "_",
      accession_safe,
      ".txt"
    )
  )
  
  manifest$extracted_text_path[i] <- output_path
  
  if (!file.exists(input_path)) {
    
    manifest$extraction_status[i] <- "Input file missing"
    manifest$extraction_error[i] <- "Downloaded HTML file not found"
    
  } else {
    
    result <- tryCatch(
      extract_visible_text(input_path),
      error = function(e) e
    )
    
    if (inherits(result, "error")) {
      
      manifest$extraction_status[i] <- "Extraction failed"
      manifest$extraction_error[i] <- conditionMessage(result)
      
    } else {
      
      chars <- nchar(result, type = "chars")
      words <- str_count(result, "\\S+")
      
      # Basic heuristic, not proof of complete or accurate report text.
      has_financial_terms <- str_detect(
        str_to_lower(result),
        "revenue|risk factors|management.s discussion|cash flows|net income"
      )
      
      writeLines(
        result,
        output_path,
        useBytes = TRUE
      )
      
      manifest$character_count[i] <- chars
      manifest$word_count[i] <- words
      manifest$contains_financial_terms[i] <- has_financial_terms
      
      if (chars < 1000) {
        manifest$extraction_status[i] <- "Suspiciously short text"
      } else if (!has_financial_terms) {
        manifest$extraction_status[i] <- "Review text content"
      } else {
        manifest$extraction_status[i] <- "Extracted"
      }
    }
  }
  
  # Checkpoint every 25 reports.
  if (i %% 25 == 0 || i == nrow(manifest)) {
    
    write_csv(manifest, output_manifest, na = "")
    
    cat(
      "Processed", i, "of", nrow(manifest),
      "| extracted:",
      sum(manifest$extraction_status == "Extracted"),
      "\n"
    )
  }
}

# Save final extraction manifest.
write_csv(manifest, output_manifest, na = "")

# Create quality summary.
quality <- manifest %>%
  group_by(extraction_status) %>%
  summarise(
    documents = n(),
    median_characters = median(character_count, na.rm = TRUE),
    median_words = median(word_count, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(quality, quality_report, na = "")

cat("\n============================================\n")
cat("PHASE 5D SUMMARY\n")
cat("============================================\n")

print(as.data.frame(
  manifest %>% count(extraction_status, sort = TRUE)
))

cat(
  "\nText files created:",
  sum(file.exists(manifest$extracted_text_path)),
  "\n"
)

cat("Extraction manifest:", output_manifest, "\n")
cat("Quality summary:", quality_report, "\n")
cat("Extracted text directory:", normalizePath(text_dir), "\n")

cat("\nPhase 5D completed.\n")
