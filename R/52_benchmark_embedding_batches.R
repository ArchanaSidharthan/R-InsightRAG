
# ============================================================
# R-InsightRAG - Phase 6B
# Benchmark local embedding speed and output size
# ============================================================

library(reticulate)
library(readr)
library(dplyr)
library(jsonlite)

setwd("C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG")

input_file <- "data/processed/sec_document_chunks_unique.csv"
output_file <- "outputs/phase6b_embedding_benchmark.csv"

if (!file.exists(input_file)) {
  stop("Deduplicated chunk file not found.")
}

# Read only 100 rows for a small benchmark.
sample_data <- read_csv(
  input_file,
  n_max = 100,
  show_col_types = FALSE
)

if (nrow(sample_data) == 0) {
  stop("No chunks available for benchmarking.")
}

# Reuse the configured local Python environment.
sentence_transformers <- import("sentence_transformers")

model <- sentence_transformers$SentenceTransformer(
  "sentence-transformers/all-MiniLM-L6-v2",
  device = "cpu"
)

texts <- as.character(sample_data$chunk_text)

# Time the embedding operation.
start_time <- Sys.time()

vectors <- model$encode(
  texts,
  batch_size = 16L,
  show_progress_bar = TRUE,
  convert_to_numpy = TRUE,
  normalize_embeddings = TRUE
)

elapsed_seconds <- as.numeric(
  difftime(Sys.time(), start_time, units = "secs")
)

embedding_matrix <- as.matrix(vectors)

if (nrow(embedding_matrix) != nrow(sample_data)) {
  stop("Unexpected number of embeddings.")
}

if (any(!is.finite(embedding_matrix))) {
  stop("Embeddings contain non-finite values.")
}

# Save sample embeddings for inspection.
sample_data$embedding_json <- vapply(
  seq_len(nrow(embedding_matrix)),
  function(i) {
    as.character(toJSON(
      as.numeric(embedding_matrix[i, ]),
      auto_unbox = FALSE
    ))
  },
  character(1)
)

sample_data$embedding_dimension <- ncol(embedding_matrix)

write_csv(sample_data, output_file, na = "")

# Approximate total runtime from the observed benchmark.
estimated_total_hours <- (
  elapsed_seconds / nrow(sample_data) * 554160
) / 3600

cat("\n============================================\n")
cat("PHASE 6B BENCHMARK RESULTS\n")
cat("============================================\n")
cat("Chunks embedded:", nrow(sample_data), "\n")
cat("Embedding dimensions:", ncol(embedding_matrix), "\n")
cat("Elapsed seconds:", round(elapsed_seconds, 2), "\n")
cat(
  "Chunks per second:",
  round(nrow(sample_data) / elapsed_seconds, 3),
  "\n"
)
cat(
  "Estimated hours for 554,160 chunks:",
  round(estimated_total_hours, 2),
  "\n"
)
cat("Output:", output_file, "\n")
cat("\nBenchmark completed.\n")
