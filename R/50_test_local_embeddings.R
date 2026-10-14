
# ============================================================
# R-InsightRAG - Phase 6A
# Test local embeddings with Ollama
# ============================================================

library(httr)
library(jsonlite)
library(readr)
library(dplyr)

setwd("C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG")

input_file <- "data/processed/sec_document_chunks_unique.csv"
output_file <- "outputs/phase6a_embedding_test.csv"

if (!file.exists(input_file)) {
  stop("Deduplicated chunks file not found: ", input_file)
}

# Read only a small sample for the first test.
sample_chunks <- read_csv(
  input_file,
  n_max = 20,
  show_col_types = FALSE
)

if (nrow(sample_chunks) == 0) {
  stop("No chunks found in the input file.")
}

# Confirm that the local Ollama service is reachable.
base_url <- "http://localhost:11434"

health <- tryCatch(
  GET(paste0(base_url, "/api/tags"), timeout(10)),
  error = function(e) e
)

if (inherits(health, "error")) {
  stop(
    "Cannot reach Ollama. Ensure Ollama is installed and running. ",
    "Details: ", conditionMessage(health)
  )
}

if (status_code(health) != 200) {
  stop("Ollama is reachable but returned HTTP ", status_code(health))
}

# Verify that the model is available locally.
available_models <- content(
  health,
  as = "parsed",
  type = "application/json"
)$models

model_names <- vapply(
  available_models,
  function(x) as.character(x$name),
  character(1)
)

if (!any(grepl("^all-minilm(?::|$)", model_names))) {
  stop("Model all-minilm not found. Run: ollama pull all-minilm")
}

# Create embeddings locally.
response <- POST(
  paste0(base_url, "/api/embed"),
  content_type_json(),
  encode = "json",
  body = list(
    model = "all-minilm",
    input = as.character(sample_chunks$chunk_text)
  ),
  timeout(180)
)

if (status_code(response) != 200) {
  stop(
    "Embedding request failed with HTTP ",
    status_code(response), ": ",
    content(response, as = "text", encoding = "UTF-8")
  )
}

result <- content(
  response,
  as = "parsed",
  type = "application/json"
)

embeddings <- result$embeddings

if (is.null(embeddings) || length(embeddings) != nrow(sample_chunks)) {
  stop("Embedding count does not match the sample size.")
}

embedding_matrix <- do.call(rbind, embeddings)

if (any(!is.finite(embedding_matrix))) {
  stop("Embedding output contains non-finite values.")
}

# Store the test vectors as JSON for inspection.
sample_chunks$embedding_dimension <- ncol(embedding_matrix)
sample_chunks$embedding_json <- vapply(
  seq_len(nrow(embedding_matrix)),
  function(i) {
    as.character(toJSON(
      as.numeric(embedding_matrix[i, ]),
      auto_unbox = FALSE
    ))
  },
  character(1)
)

write_csv(sample_chunks, output_file, na = "")

cat("\n============================================\n")
cat("PHASE 6A EMBEDDING TEST\n")
cat("============================================\n")
cat("Sample chunks:", nrow(sample_chunks), "\n")
cat("Embedding dimensions:", ncol(embedding_matrix), "\n")
cat("All values finite:", all(is.finite(embedding_matrix)), "\n")
cat("Output:", output_file, "\n")
cat("\nLocal embedding test completed.\n")
