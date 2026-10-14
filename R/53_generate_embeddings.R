
# ============================================================
# R-InsightRAG - Phase 6C
# Resumable local CPU embedding pipeline
# ============================================================

library(reticulate)
library(readr)
library(dplyr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

input_file <- "data/processed/sec_document_chunks_unique.csv"
batch_dir <- "data/processed/embedding_batches"
log_file <- "outputs/phase6c_embedding_log.csv"

dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Input chunks file not found: ", input_file)
}

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

batch_size <- 2000L
embedding_dimension <- 384L

# ------------------------------------------------------------
# Load local model
# ------------------------------------------------------------

sentence_transformers <- import("sentence_transformers")

model <- sentence_transformers$SentenceTransformer(
  "sentence-transformers/all-MiniLM-L6-v2",
  device = "cpu"
)

cat("\n============================================\n")
cat("PHASE 6C: FULL EMBEDDING PIPELINE\n")
cat("============================================\n")
cat("Batch size:", batch_size, "\n")
cat("Embedding dimension:", embedding_dimension, "\n")
cat("Output folder:", normalizePath(batch_dir), "\n\n")

# ------------------------------------------------------------
# Callback processes one CSV batch at a time.
# read_csv_chunked avoids loading the entire corpus into RAM.
# ------------------------------------------------------------

log_entries <- list()

callback <- DataFrameCallback$new(function(data, pos) {
  
  if (nrow(data) == 0) {
    return(invisible(NULL))
  }
  
  # Position-based batch filename for resumability.
  batch_number <- floor((pos - 1) / batch_size) + 1L
  
  batch_file <- file.path(
    batch_dir,
    sprintf("embedding_batch_%04d.rds", batch_number)
  )
  
  # Skip batches that have already been successfully saved.
  if (file.exists(batch_file)) {
    cat(
      "Skipping existing batch", batch_number,
      "| input rows:", pos, "\n"
    )
    return(invisible(NULL))
  }
  
  if (!"chunk_text" %in% names(data)) {
    stop("The input CSV does not contain chunk_text.")
  }
  
  texts <- as.character(data$chunk_text)
  
  if (anyNA(texts) || any(!nzchar(texts))) {
    stop("Missing or empty chunk text in batch ", batch_number)
  }
  
  start_time <- Sys.time()
  
  vectors <- model$encode(
    texts,
    batch_size = 16L,
    show_progress_bar = FALSE,
    convert_to_numpy = TRUE,
    normalize_embeddings = TRUE
  )
  
  embedding_matrix <- as.matrix(vectors)
  
  if (nrow(embedding_matrix) != nrow(data)) {
    stop("Embedding row count mismatch in batch ", batch_number)
  }
  
  if (ncol(embedding_matrix) != embedding_dimension) {
    stop("Unexpected embedding dimension in batch ", batch_number)
  }
  
  if (any(!is.finite(embedding_matrix))) {
    stop("Non-finite embedding value in batch ", batch_number)
  }
  
  # Store only the required metadata and vectors.
  metadata_columns <- intersect(
    c(
      "chunk_id", "company_id", "company_name",
      "fiscal_year", "accession_number",
      "source_url", "source_text_path",
      "chunk_index", "character_count", "word_count"
    ),
    names(data)
  )
  
  batch_object <- list(
    batch_number = batch_number,
    input_position = pos,
    created_at = as.character(Sys.time()),
    dimension = ncol(embedding_matrix),
    metadata = data[, metadata_columns, drop = FALSE],
    embeddings = embedding_matrix
  )
  
  # Save atomically to avoid treating incomplete files as finished.
  temp_file <- paste0(batch_file, ".tmp")
  
  saveRDS(batch_object, temp_file, compress = FALSE)
  
  if (!file.rename(temp_file, batch_file)) {
    stop("Could not finalize batch file: ", batch_file)
  }
  
  elapsed <- as.numeric(
    difftime(Sys.time(), start_time, units = "secs")
  )
  
  log_entries[[length(log_entries) + 1L]] <<- data.frame(
    batch_number = batch_number,
    input_position = pos,
    rows = nrow(data),
    dimension = ncol(embedding_matrix),
    elapsed_seconds = elapsed,
    chunks_per_second = nrow(data) / elapsed,
    batch_file = batch_file,
    stringsAsFactors = FALSE
  )
  
  cat(
    "Saved batch", batch_number,
    "| rows:", nrow(data),
    "| seconds:", round(elapsed, 1),
    "| total input position:", pos,
    "\n"
  )
  
  # Persist progress after every successfully completed batch.
  existing_log <- if (file.exists(log_file)) {
    read_csv(log_file, show_col_types = FALSE)
  } else {
    data.frame()
  }
  
  new_log <- bind_rows(existing_log, bind_rows(log_entries))
  
  new_log <- new_log %>%
    distinct(batch_number, .keep_all = TRUE)
  
  write_csv(new_log, log_file, na = "")
  
  invisible(NULL)
})

# ------------------------------------------------------------
# Stream the source CSV in batches.
# The callback receives a data frame and its starting row.
# ------------------------------------------------------------

read_csv_chunked(
  input_file,
  callback = callback,
  chunk_size = batch_size,
  col_types = cols(.default = col_character()),
  progress = TRUE
)

# ------------------------------------------------------------
# Final batch inventory
# ------------------------------------------------------------

batch_files <- list.files(
  batch_dir,
  pattern = "^embedding_batch_[0-9]+\\.rds$",
  full.names = TRUE
)

cat("\n============================================\n")
cat("PHASE 6C SUMMARY\n")
cat("============================================\n")
cat("Batch files saved:", length(batch_files), "\n")
cat("Batch directory:", normalizePath(batch_dir), "\n")
cat("Progress log:", log_file, "\n")

cat(
  "\nNext step: validate batch coverage and create the retrieval index.\n"
)
