# ============================================================
# R-InsightRAG - Phase 7A
# Validate Embedding Batches
# ============================================================

library(readr)
library(dplyr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

input_file <- "data/processed/sec_document_chunks_unique.csv"
batch_dir <- "data/processed/embedding_batches"
output_file <- "outputs/phase7a_embedding_validation.csv"
summary_file <- "outputs/phase7a_validation_summary.csv"

expected_dimension <- 384L

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

cat("\nPHASE 7A: EMBEDDING VALIDATION\n")

# Load source chunk IDs
source_data <- read_csv(
  input_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

if (!"chunk_id" %in% names(source_data)) {
  stop("chunk_id column not found in source CSV.")
}

expected_ids <- as.character(source_data$chunk_id)

if (anyNA(expected_ids) ||
    any(expected_ids == "") ||
    anyDuplicated(expected_ids) > 0) {
  stop("Source chunk IDs are missing or duplicated.")
}

# Find batches in numerical order
batch_files <- list.files(
  batch_dir,
  pattern = "^embedding_batch_[0-9]+\\.rds$",
  full.names = TRUE
)

if (length(batch_files) == 0) {
  stop("No embedding batch files found.")
}

batch_numbers <- as.integer(
  sub(
    "^embedding_batch_([0-9]+)\\.rds$",
    "\\1",
    basename(batch_files)
  )
)

batch_files <- batch_files[order(batch_numbers)]

cat("Expected source chunks:", length(expected_ids), "\n")
cat("Batch files found:", length(batch_files), "\n")

# Validate each batch
audit <- vector("list", length(batch_files))
all_ids <- character(0)

for (i in seq_along(batch_files)) {
  
  current_file <- batch_files[i]
  current_name <- basename(current_file)
  
  result <- tryCatch({
    
    obj <- readRDS(current_file)
    
    if (is.null(obj$metadata) ||
        is.null(obj$embeddings)) {
      stop("Missing metadata or embeddings.")
    }
    
    metadata <- as.data.frame(obj$metadata)
    embeddings <- obj$embeddings
    
    if (!"chunk_id" %in% names(metadata)) {
      stop("chunk_id missing from batch metadata.")
    }
    
    ids <- as.character(metadata$chunk_id)
    
    if (!is.matrix(embeddings)) {
      stop("Embeddings are not a matrix.")
    }
    
    if (nrow(embeddings) != nrow(metadata)) {
      stop("Metadata and embedding row counts differ.")
    }
    
    if (ncol(embeddings) != expected_dimension) {
      stop("Incorrect embedding dimension.")
    }
    
    if (anyNA(ids) || any(ids == "")) {
      stop("Missing chunk IDs.")
    }
    
    if (any(!is.finite(embeddings))) {
      stop("Non-finite embedding values detected.")
    }
    
    list(
      status = "PASS",
      rows = nrow(metadata),
      dimensions = ncol(embeddings),
      ids = ids,
      message = "All checks passed"
    )
    
  }, error = function(e) {
    
    list(
      status = "FAIL",
      rows = NA_integer_,
      dimensions = NA_integer_,
      ids = character(0),
      message = conditionMessage(e)
    )
  })
  
  all_ids <- c(all_ids, result$ids)
  
  audit[[i]] <- data.frame(
    batch_file = current_name,
    status = result$status,
    rows = result$rows,
    dimensions = result$dimensions,
    message = result$message,
    stringsAsFactors = FALSE
  )
  
  cat(
    sprintf(
      "%d/%d | %s | %s\n",
      i, length(batch_files), current_name, result$status
    )
  )
}

audit_df <- bind_rows(audit)

# Check complete coverage
missing_ids <- setdiff(expected_ids, all_ids)
unexpected_ids <- setdiff(all_ids, expected_ids)
duplicate_ids <- sum(duplicated(all_ids))

failed_batches <- sum(audit_df$status == "FAIL")

coverage_pass <- (
  failed_batches == 0 &&
    duplicate_ids == 0 &&
    length(missing_ids) == 0 &&
    length(unexpected_ids) == 0 &&
    length(all_ids) == length(expected_ids)
)

# Save detailed audit
write_csv(audit_df, output_file)

# Save summary
summary_df <- data.frame(
  metric = c(
    "Expected source chunks",
    "Embedding rows",
    "Batch files",
    "Passed batches",
    "Failed batches",
    "Duplicate embedding IDs",
    "Missing source IDs",
    "Unexpected embedding IDs",
    "Coverage status"
  ),
  value = c(
    length(expected_ids),
    length(all_ids),
    length(batch_files),
    sum(audit_df$status == "PASS"),
    failed_batches,
    duplicate_ids,
    length(missing_ids),
    length(unexpected_ids),
    if (coverage_pass) "PASS" else "FAIL"
  )
)

write_csv(summary_df, summary_file)

cat("\nVALIDATION SUMMARY\n")
print(summary_df)

cat("\nDetailed audit:", output_file, "\n")
cat("Summary file:", summary_file, "\n")

if (coverage_pass) {
  cat("\nSUCCESS: Embedding coverage validated.\n")
  cat("Proceed to Phase 7B: retrieval index.\n")
} else {
  cat("\nVALIDATION FAILED. Review the audit before indexing.\n")
}