
# ============================================================
# R-InsightRAG - Phase 7C
# Semantic Search over SEC Annual Report Chunks
# Local CPU embeddings: all-MiniLM-L6-v2
# ============================================================

library(reticulate)
library(dplyr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

metadata_file <- "data/processed/retrieval_index/retrieval_metadata.rds"
embeddings_file <- "data/processed/retrieval_index/retrieval_embeddings.rds"
output_dir <- "outputs/phase7c_semantic_search"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nLoading retrieval metadata...\n")
index_metadata <- readRDS(metadata_file)

cat("Loading retrieval embeddings...\n")
index_embeddings <- readRDS(embeddings_file)

# ------------------------------------------------------------
# 1. Validate index
# ------------------------------------------------------------

if (nrow(index_metadata) != nrow(index_embeddings)) {
  stop("Metadata rows and embedding rows do not match.")
}

if (ncol(index_embeddings) != 384) {
  stop("Expected 384-dimensional embeddings. Check the saved index.")
}

if (anyDuplicated(index_metadata$chunk_id) > 0) {
  stop("Duplicate chunk IDs detected in metadata.")
}

cat("Index validation passed.\n")
cat("Indexed chunks:", nrow(index_metadata), "\n")
cat("Embedding dimensions:", ncol(index_embeddings), "\n")

# ------------------------------------------------------------
# 2. Load the same local embedding model
# ------------------------------------------------------------

cat("\nLoading sentence-transformers model...\n")

st <- reticulate::import("sentence_transformers")
model <- st$SentenceTransformer(
  "sentence-transformers/all-MiniLM-L6-v2",
  device = "cpu"
)

# ------------------------------------------------------------
# 3. Define semantic search function
# ------------------------------------------------------------

semantic_search <- function(
    question,
    top_k = 5,
    company = NULL,
    fiscal_year = NULL) {
  
  if (!is.character(question) || length(question) != 1 ||
      is.na(question) || !nzchar(trimws(question))) {
    stop("Provide a non-empty question.")
  }
  
  if (top_k < 1) {
    stop("top_k must be at least 1.")
  }
  
  cat("\nEmbedding query...\n")
  
  query_embedding <- model$encode(
    question,
    normalize_embeddings = TRUE,
    convert_to_numpy = TRUE,
    show_progress_bar = FALSE
  )
  
  query_vector <- as.numeric(query_embedding)
  
  if (length(query_vector) != ncol(index_embeddings)) {
    stop("Query embedding dimension does not match the index.")
  }
  
  # Restrict candidates before scoring when filters are supplied.
  candidates <- seq_len(nrow(index_metadata))
  
  if (!is.null(company) && nzchar(trimws(company))) {
    company_match <- grepl(
      company,
      index_metadata$company_name,
      ignore.case = TRUE,
      fixed = TRUE
    )
    
    candidates <- candidates[company_match[candidates]]
  }
  
  if (!is.null(fiscal_year) && !is.na(fiscal_year)) {
    candidates <- candidates[
      index_metadata$fiscal_year[candidates] == fiscal_year
    ]
  }
  
  if (length(candidates) == 0) {
    cat("No chunks matched the requested filters.\n")
    return(data.frame())
  }
  
  cat("Scoring", length(candidates), "candidate chunks...\n")
  
  # Embeddings are normalized, so dot product equals cosine similarity.
  # Process in blocks to limit temporary memory usage.
  block_size <- 25000L
  similarities <- numeric(length(candidates))
  
  starts <- seq.int(1L, length(candidates), by = block_size)
  
  for (start in starts) {
    end <- min(start + block_size - 1L, length(candidates))
    positions <- start:end
    rows <- candidates[positions]
    
    block <- index_embeddings[rows, , drop = FALSE]
    similarities[positions] <- as.numeric(block %*% query_vector)
    
    rm(block)
  }
  
  result_positions <- order(similarities, decreasing = TRUE)
  result_positions <- head(result_positions, min(top_k, length(result_positions)))
  
  selected_rows <- candidates[result_positions]
  
  results <- index_metadata[selected_rows, , drop = FALSE]
  results$similarity <- similarities[result_positions]
  
  results <- results %>%
    select(
      chunk_id,
      company_id,
      company_name,
      fiscal_year,
      accession_number,
      similarity,
      source_url,
      source_text_path,
      chunk_index,
      character_count,
      word_count
    ) %>%
    arrange(desc(similarity))
  
  # Save metadata and ranking; passage text is loaded in the next step.
  write.csv(
    results,
    file.path(output_dir, "latest_search_results.csv"),
    row.names = FALSE,
    na = ""
  )
  
  results
}

# ------------------------------------------------------------
# 4. Run an initial test query
# ------------------------------------------------------------

test_question <- paste(
  "What are the principal business risks, uncertainties,",
  "and risk factors disclosed by the company?"
)

cat("\nTest query:\n", test_question, "\n")

search_results <- semantic_search(
  question = test_question,
  top_k = 5
)

cat("\nTop semantic search results:\n")

if (nrow(search_results) > 0) {
  print(
    search_results %>%
      select(
        company_name,
        fiscal_year,
        similarity,
        accession_number,
        chunk_id
      ),
    row.names = FALSE
  )
  
  cat(
    "\nFull result metadata saved to:",
    file.path(output_dir, "latest_search_results.csv"),
    "\n"
  )
}

cat("\nPhase 7C initial search completed.\n")
