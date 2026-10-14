
# ============================================================
# R-InsightRAG - Phase 7D
# Inspect Retrieved Passage Text
# ============================================================

library(readr)
library(dplyr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

results_file <- "outputs/phase7c_semantic_search/latest_search_results.csv"
chunks_file <- "data/processed/sec_document_chunks_unique.csv"
output_file <- "outputs/phase7d_retrieved_passages.csv"

cat("Loading search results...\n")
results <- read_csv(results_file, show_col_types = FALSE)

cat("Loading chunk text...\n")
chunks <- read_csv(chunks_file, show_col_types = FALSE)

# Validate the actual dataset schema
required_columns <- c("chunk_id", "chunk_text")

if (!all(required_columns %in% names(chunks))) {
  stop("Required columns chunk_id and chunk_text were not found.")
}

# Match retrieved chunks to their original passage text
passages <- results %>%
  left_join(
    chunks %>% select(chunk_id, chunk_text),
    by = "chunk_id"
  ) %>%
  mutate(
    passage_found = !is.na(chunk_text) & nzchar(chunk_text)
  )

cat("\n========================================\n")
cat("PHASE 7D: RETRIEVED PASSAGE INSPECTION\n")
cat("========================================\n")
cat("Retrieved results:", nrow(passages), "\n")
cat("Passages found:", sum(passages$passage_found), "\n")
cat("Passages missing:", sum(!passages$passage_found), "\n")

# Display each retrieved passage
for (i in seq_len(nrow(passages))) {
  
  cat("\n\n----------------------------------------\n")
  cat("RANK:", i, "\n")
  cat("COMPANY:", passages$company_name[i], "\n")
  cat("FISCAL YEAR:", passages$fiscal_year[i], "\n")
  cat("COSINE SIMILARITY:", round(passages$similarity[i], 4), "\n")
  cat("CHUNK ID:", passages$chunk_id[i], "\n")
  cat("ACCESSION:", passages$accession_number[i], "\n")
  cat("SEC SOURCE:", passages$source_url[i], "\n\n")
  
  if (passages$passage_found[i]) {
    cat("PASSAGE TEXT:\n")
    cat(substr(passages$chunk_text[i], 1, 1800), "\n")
  } else {
    cat("WARNING: Passage text was not found.\n")
  }
}

# Save results with their full passage text
write_csv(passages, output_file, na = "")

cat("\n\nInspection completed.\n")
cat("Saved to:", output_file, "\n")
