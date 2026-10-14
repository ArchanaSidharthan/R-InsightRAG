
# ============================================================
# R-InsightRAG - Phase 5A
# Build SEC Annual Report Document Inventory
# ============================================================

library(readr)
library(dplyr)
library(stringr)

cat("\n============================================\n")
cat("PHASE 5A: SEC DOCUMENT INVENTORY\n")
cat("============================================\n")

# 1. Set project root
project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

input_file <- file.path(
  "data", "processed", "selected_annual_filings.csv"
)

output_file <- file.path(
  "data", "processed", "sec_document_inventory.csv"
)

# 2. Validate input
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

filings <- read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

names(filings) <- tolower(names(filings))

cat("\nInput rows:", nrow(filings), "\n")
cat("Available columns:\n")
print(names(filings))

# 3. Identify required columns
find_column <- function(candidates, available_names) {
  match <- candidates[candidates %in% available_names]
  
  if (length(match) == 0) {
    return(NA_character_)
  }
  
  match[1]
}

accession_col <- find_column(
  c("adsh", "accession_number", "accession_no", "accession"),
  names(filings)
)

cik_col <- find_column(
  c("cik", "company_id", "cik_number"),
  names(filings)
)

name_col <- find_column(
  c("company_name", "name", "company"),
  names(filings)
)

year_col <- find_column(
  c("fiscal_year", "fy", "year"),
  names(filings)
)

form_col <- find_column(
  c("form", "form_type", "filing_type"),
  names(filings)
)

filed_col <- find_column(
  c("filed", "filing_date", "filed_date"),
  names(filings)
)

period_col <- find_column(
  c("period", "period_end", "period_end_date"),
  names(filings)
)

# Accession number is essential for locating a filing.
if (is.na(accession_col)) {
  stop(
    "No accession-number column found. ",
    "Review the printed column names before proceeding."
  )
}

if (is.na(cik_col)) {
  stop(
    "No CIK/company identifier column found. ",
    "Review the printed column names before proceeding."
  )
}

# 4. Build a standardized inventory
get_column <- function(data, column_name, default = NA_character_) {
  if (is.na(column_name)) {
    return(rep(default, nrow(data)))
  }
  
  as.character(data[[column_name]])
}

inventory <- tibble(
  company_id = get_column(filings, cik_col),
  company_name = get_column(filings, name_col),
  fiscal_year = get_column(filings, year_col),
  accession_number = get_column(filings, accession_col),
  form_type = get_column(filings, form_col),
  filing_date = get_column(filings, filed_col),
  reporting_period = get_column(filings, period_col)
) %>%
  mutate(
    # Standardize SEC accession number
    accession_number = str_trim(accession_number),
    
    # SEC CIKs in URLs are numeric and have no leading zeros
    cik_url = str_remove(
      str_trim(company_id),
      "^0+"
    ),
    
    # Remove hyphens from accession number for archive URLs
    accession_folder = str_remove_all(
      accession_number,
      "-"
    ),
    
    # The filing index provides the list of documents in a filing.
    filing_index_url = paste0(
      "https://www.sec.gov/Archives/edgar/data/",
      cik_url,
      "/",
      accession_folder,
      "/index.json"
    ),
    
    # To be populated after identifying the primary filing document
    primary_document = NA_character_,
    primary_document_url = NA_character_,
    document_status = "Pending index inspection"
  ) %>%
  filter(
    !is.na(accession_number),
    accession_number != "",
    !is.na(cik_url),
    cik_url != ""
  ) %>%
  distinct(
    company_id,
    fiscal_year,
    accession_number,
    .keep_all = TRUE
  ) %>%
  arrange(company_name, fiscal_year)

# 5. Validate accession and CIK formats
inventory <- inventory %>%
  mutate(
    accession_valid = str_detect(
      accession_number,
      "^\\d{10}-\\d{2}-\\d{6}$"
    ),
    cik_valid = str_detect(cik_url, "^\\d+$")
  )

# 6. Save the inventory
write_csv(inventory, output_file, na = "")

# 7. Report results
cat("\n--------------------------------------------\n")
cat("INVENTORY SUMMARY\n")
cat("--------------------------------------------\n")

cat("Inventory rows:", nrow(inventory), "\n")
cat(
  "Unique companies:",
  n_distinct(inventory$company_id),
  "\n"
)
cat(
  "Unique accession numbers:",
  n_distinct(inventory$accession_number),
  "\n"
)
cat(
  "Valid accession formats:",
  sum(inventory$accession_valid, na.rm = TRUE),
  "\n"
)
cat(
  "Valid CIK formats:",
  sum(inventory$cik_valid, na.rm = TRUE),
  "\n"
)

cat("\nFiling forms:\n")
print(table(inventory$form_type, useNA = "ifany"))

cat("\nFiscal years:\n")
print(table(inventory$fiscal_year, useNA = "ifany"))

cat("\nSample filing inventory:\n")
print(
  inventory %>%
    select(
      company_name,
      fiscal_year,
      form_type,
      filing_date,
      accession_number,
      filing_index_url
    ) %>%
    head(10),
  width = Inf
)

cat("\nSaved inventory to:\n")
cat(normalizePath(output_file), "\n")

cat("\nPhase 5A completed.\n")
