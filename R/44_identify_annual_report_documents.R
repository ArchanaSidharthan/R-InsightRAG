
# ============================================================
# R-InsightRAG - Phase 5B
# Identify annual report forms and primary documents
# ============================================================

library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(stringr)

setwd("C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG")

input_file <- "data/processed/sec_document_inventory.csv"
output_file <- "data/processed/sec_annual_report_documents.csv"
error_file <- "outputs/phase5b_sec_download_errors.csv"

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

inventory <- read_csv(input_file, show_col_types = FALSE)

# SEC requests require a descriptive User-Agent.
# Replace this with your own contact email before running.
sec_user_agent <- "R-InsightRAG research contact@example.com"

# Supported annual report form types.
annual_forms <- c("10-K", "20-F", "40-F")

# Extract the first header field value from SEC submission text.
header_value <- function(txt, field) {
  pattern <- paste0("(?im)^", field, ":\\s*(.+?)\\s*$")
  result <- str_match(txt, pattern)
  
  if (is.na(result[1, 2])) {
    return(NA_character_)
  }
  
  str_trim(result[1, 2])
}

# Extract the primary annual-report document filename.
# SEC complete submissions contain multiple DOCUMENT blocks.
find_primary_document <- function(txt, form_type) {
  
  document_blocks <- str_extract_all(
    txt,
    regex(
      "<DOCUMENT>.*?</DOCUMENT>",
      dotall = TRUE
    )
  )[[1]]
  
  if (length(document_blocks) == 0) {
    return(NA_character_)
  }
  
  for (block in document_blocks) {
    
    block_type <- str_match(
      block,
      regex("<TYPE>\\s*([^\\r\\n<]+)", ignore_case = TRUE)
    )[, 2]
    
    block_file <- str_match(
      block,
      regex("<FILENAME>\\s*([^\\r\\n<]+)", ignore_case = TRUE)
    )[, 2]
    
    if (
      !is.na(block_type) &&
      !is.na(block_file) &&
      str_trim(block_type) == form_type
    ) {
      return(str_trim(block_file))
    }
  }
  
  NA_character_
}

results <- vector("list", nrow(inventory))
errors <- list()

cat("Total filings to inspect:", nrow(inventory), "\n")
cat("This process makes one SEC request per filing.\n")
cat("Please allow several minutes for completion.\n\n")

for (i in seq_len(nrow(inventory))) {
  
  cik <- str_remove(
    str_trim(as.character(inventory$cik_url[i])),
    "^0+"
  )
  
  accession <- str_trim(
    as.character(inventory$accession_number[i])
  )
  
  accession_compact <- str_remove_all(accession, "-")
  
  # Complete submission text URL
  submission_url <- paste0(
    "https://www.sec.gov/Archives/edgar/data/",
    cik, "/",
    accession_compact, "/",
    accession, ".txt"
  )
  
  response <- tryCatch(
    GET(
      submission_url,
      user_agent(sec_user_agent),
      timeout(30)
    ),
    error = function(e) e
  )
  
  status <- NA_integer_
  form_type <- NA_character_
  primary_document <- NA_character_
  document_url <- NA_character_
  report_status <- "Request failed"
  
  if (inherits(response, "response")) {
    
    status <- status_code(response)
    
    if (status == 200) {
      
      txt <- tryCatch(
        content(response, as = "text", encoding = "UTF-8"),
        error = function(e) NA_character_
      )
      
      if (!is.na(txt) && nchar(txt) > 0) {
        
        form_type <- header_value(
          txt,
          "CONFORMED SUBMISSION TYPE"
        )
        
        if (is.na(form_type)) {
          form_type <- header_value(txt, "SUBMISSION TYPE")
        }
        
        if (!is.na(form_type) && form_type %in% annual_forms) {
          
          primary_document <- find_primary_document(txt, form_type)
          
          if (!is.na(primary_document)) {
            document_url <- paste0(
              "https://www.sec.gov/Archives/edgar/data/",
              cik, "/",
              accession_compact, "/",
              primary_document
            )
            
            report_status <- "Annual report document identified"
          } else {
            report_status <- "Annual form found; primary document unresolved"
          }
          
        } else if (!is.na(form_type)) {
          report_status <- "Not a supported annual report form"
        } else {
          report_status <- "Form type not found in submission header"
        }
        
      } else {
        report_status <- "Empty or unreadable submission"
      }
      
    } else {
      report_status <- paste("HTTP", status)
    }
    
  } else {
    errors[[length(errors) + 1]] <- data.frame(
      row = i,
      accession_number = accession,
      error = as.character(response$message)
    )
  }
  
  results[[i]] <- data.frame(
    company_id = inventory$company_id[i],
    company_name = inventory$company_name[i],
    fiscal_year = inventory$fiscal_year[i],
    accession_number = accession,
    filing_date = inventory$filing_date[i],
    form_type = form_type,
    primary_document = primary_document,
    primary_document_url = document_url,
    http_status = status,
    document_status = report_status,
    stringsAsFactors = FALSE
  )
  
  if (i %% 25 == 0 || i == nrow(inventory)) {
    cat("Processed", i, "of", nrow(inventory), "filings\n")
  }
  
  # Stay well below the SEC's general request-rate limit.
  Sys.sleep(0.15)
}

report_inventory <- bind_rows(results)

write_csv(report_inventory, output_file, na = "")

if (length(errors) > 0) {
  write_csv(bind_rows(errors), error_file, na = "")
}

cat("\n============================================\n")
cat("PHASE 5B SUMMARY\n")
cat("============================================\n")

cat("Total records:", nrow(report_inventory), "\n")
cat(
  "Supported annual forms:",
  sum(report_inventory$form_type %in% annual_forms, na.rm = TRUE),
  "\n"
)
cat(
  "Primary document identified:",
  sum(!is.na(report_inventory$primary_document_url)),
  "\n"
)

cat("\nForm types:\n")
print(table(report_inventory$form_type, useNA = "ifany"))

cat("\nDocument statuses:\n")
print(table(report_inventory$document_status, useNA = "ifany"))

cat("\nSample identified reports:\n")
print(
  report_inventory %>%
    filter(!is.na(primary_document_url)) %>%
    select(
      company_name,
      fiscal_year,
      form_type,
      primary_document,
      primary_document_url
    ) %>%
    head(10),
  width = Inf
)

cat("\nSaved:", output_file, "\n")

if (length(errors) > 0) {
  cat("Request errors saved to:", error_file, "\n")
}

cat("\nPhase 5B completed.\n")
