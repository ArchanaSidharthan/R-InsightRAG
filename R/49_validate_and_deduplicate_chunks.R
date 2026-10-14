
# ============================================================
# R-InsightRAG - Phase 5F
# Validate and deduplicate document chunks
# ============================================================

library(readr)
library(dplyr)
library(stringr)

setwd("C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG")

input_file <- "data/processed/sec_document_chunks.csv"
output_file <- "data/processed/sec_document_chunks_unique.csv"
duplicate_file <- "outputs/phase5f_duplicate_chunk_audit.csv"
summary_file <- "outputs/phase5f_chunk_validation_summary.csv"

if (!file.exists(input_file)) {
  stop("Chunk dataset not found: ", input_file)
}

cat("Reading chunk dataset...\n")

chunks <- read_csv(
  input_file,
  show_col_types = FALSE
)

required <- c(
  "chunk_id", "company_id", "company_name",
  "fiscal_year", "accession_number",
  "source_url", "chunk_index", "chunk_text",
  "character_count", "word_count"
)

missing_columns <- setdiff(required, names(chunks))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Validate identifiers, content, and lengths.
chunks <- chunks %>%
  mutate(
    empty_text = is.na(chunk_text) | str_trim(chunk_text) == "",
    actual_characters = nchar(chunk_text, type = "chars"),
    duplicate_chunk_id = duplicated(chunk_id),
    normalized_text = str_squish(str_to_lower(chunk_text))
  )

# Exact duplicate text, regardless of company or report.
# Keep the first occurrence for the retrieval index, but retain
# a separate audit file so source provenance is not discarded.
duplicate_audit <- chunks %>%
  group_by(normalized_text) %>%
  mutate(
    text_occurrences = n(),
    kept_for_index = row_number() == 1
  ) %>%
  filter(text_occurrences > 1) %>%
  ungroup() %>%
  select(
    chunk_id,
    company_id,
    company_name,
    fiscal_year,
    accession_number,
    source_url,
    chunk_index,
    text_occurrences,
    kept_for_index,
    chunk_text
  )

# Deduplicate only exact normalized text.
# For retained chunks, preserve the original source metadata.
unique_chunks <- chunks %>%
  filter(!empty_text, !duplicate_chunk_id) %>%
  distinct(normalized_text, .keep_all = TRUE) %>%
  select(-normalized_text, -empty_text,
         -actual_characters, -duplicate_chunk_id)

# Audit summary.
summary <- data.frame(
  metric = c(
    "input_chunks",
    "empty_chunks",
    "duplicate_chunk_ids",
    "chunks_with_duplicate_text",
    "duplicate_text_groups",
    "unique_chunks_retained",
    "chunks_removed_from_index",
    "unique_companies_retained",
    "unique_reports_retained",
    "min_characters",
    "median_characters",
    "max_characters"
  ),
  value = c(
    nrow(chunks),
    sum(chunks$empty_text),
    sum(chunks$duplicate_chunk_id),
    nrow(duplicate_audit),
    n_distinct(duplicate_audit$normalized_text),
    nrow(unique_chunks),
    nrow(chunks) - nrow(unique_chunks),
    n_distinct(unique_chunks$company_id),
    n_distinct(unique_chunks$accession_number),
    min(unique_chunks$character_count),
    median(unique_chunks$character_count),
    max(unique_chunks$character_count)
  )
)

write_csv(unique_chunks, output_file, na = "")
write_csv(duplicate_audit, duplicate_file, na = "")
write_csv(summary, summary_file, na = "")

cat("\n============================================\n")
cat("PHASE 5F VALIDATION SUMMARY\n")
cat("============================================\n")
print(summary, row.names = FALSE)

cat("\nUnique chunks saved to:", output_file, "\n")
cat("Duplicate audit saved to:", duplicate_file, "\n")
cat("Summary saved to:", summary_file, "\n")

cat("\nPhase 5F completed.\n")
