
# ============================================================
# R-InsightRAG - Phase 9A
# Build a reusable SEC passage retrieval module
# ============================================================

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

metadata_file <- file.path(
  project_root,
  "data/processed/retrieval_index/retrieval_metadata.rds"
)

embeddings_file <- file.path(
  project_root,
  "data/processed/retrieval_index/retrieval_embeddings.rds"
)

chunks_file <- file.path(
  project_root,
  "data/processed/sec_document_chunks_unique.csv"
)

output_dir <- file.path(project_root, "outputs/phase9a")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Check required files
# ------------------------------------------------------------

required_files <- c(
  metadata_file,
  embeddings_file,
  chunks_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Required files are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

cat("Required files found.\n")

# ------------------------------------------------------------
# 2. Load and validate the retrieval index
# ------------------------------------------------------------

cat("Loading retrieval metadata...\n")
index_metadata <- readRDS(metadata_file)

cat("Loading retrieval embeddings...\n")
index_embeddings <- readRDS(embeddings_file)

if (!is.data.frame(index_metadata)) {
  stop("Retrieval metadata must be a data frame.")
}

if (is.null(dim(index_embeddings)) ||
    length(dim(index_embeddings)) != 2) {
  stop("Retrieval embeddings must be a two-dimensional matrix.")
}

if (nrow(index_metadata) != nrow(index_embeddings)) {
  stop("Metadata and embedding row counts do not match.")
}

if (ncol(index_embeddings) != 384) {
  stop("Expected 384-dimensional embeddings.")
}

required_metadata_columns <- c(
  "chunk_id",
  "company_id",
  "company_name",
  "fiscal_year",
  "accession_number",
  "source_url",
  "source_text_path",
  "chunk_index"
)

if (!all(required_metadata_columns %in% names(index_metadata))) {
  stop(
    "Retrieval metadata is missing required columns: ",
    paste(
      setdiff(required_metadata_columns, names(index_metadata)),
      collapse = ", "
    )
  )
}

if (anyNA(index_metadata$chunk_id) ||
    anyDuplicated(index_metadata$chunk_id) > 0) {
  stop("Missing or duplicate chunk IDs detected in retrieval metadata.")
}

if (any(!is.finite(index_embeddings))) {
  stop("The embedding matrix contains non-finite values.")
}

cat("Index validation passed.\n")
cat("Chunks:", nrow(index_metadata), "\n")
cat("Embedding dimensions:", ncol(index_embeddings), "\n")

# ------------------------------------------------------------
# 3. Load passage text and match by chunk ID
# ------------------------------------------------------------

cat("Loading chunk IDs and passage text...\n")

chunks_text <- read.csv(
  chunks_file,
  header = TRUE,
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE,
  quote = "\"",
  comment.char = "",
  na.strings = character(),
  strip.white = FALSE
)

required_chunk_columns <- c("chunk_id", "chunk_text")

if (!all(required_chunk_columns %in% names(chunks_text))) {
  stop("Chunk CSV must contain chunk_id and chunk_text.")
}

chunks_text <- chunks_text[
  ,
  required_chunk_columns,
  drop = FALSE
]

if (anyNA(chunks_text$chunk_id) ||
    anyDuplicated(chunks_text$chunk_id) > 0) {
  stop("Missing or duplicate chunk IDs found in the chunk-text CSV.")
}

if (nrow(chunks_text) != nrow(index_metadata)) {
  stop(
    "Chunk-text and index row counts differ. ",
    "Check chunk IDs before continuing."
  )
}

# Match by ID rather than assuming row order is identical.
text_match <- match(
  index_metadata$chunk_id,
  chunks_text$chunk_id
)

if (anyNA(text_match)) {
  stop("Some index chunk IDs have no corresponding passage text.")
}

index_text <- chunks_text$chunk_text[text_match]

if (anyNA(index_text)) {
  stop("Missing passage text detected.")
}

rm(chunks_text, text_match)
invisible(gc())

cat("Passage text matched to index IDs.\n")

# ------------------------------------------------------------
# 4. Load the local sentence-transformers model
# ------------------------------------------------------------

if (!requireNamespace("reticulate", quietly = TRUE)) {
  stop("Install reticulate first: install.packages('reticulate')")
}

cat("Loading local embedding model...\n")

st <- reticulate::import("sentence_transformers")

embedding_model <- st$SentenceTransformer(
  "sentence-transformers/all-MiniLM-L6-v2",
  device = "cpu"
)

cat("Embedding model loaded.\n")

# ------------------------------------------------------------
# 5. Define the retrieval function
# ------------------------------------------------------------

retrieve_passages <- function(
    question,
    top_k = 5,
    company = NULL,
    fiscal_year = NULL,
    block_size = 25000L) {
  
  # Validate question.
  if (!is.character(question) ||
      length(question) != 1 ||
      is.na(question) ||
      !nzchar(trimws(question))) {
    stop("question must be one non-empty string.")
  }
  
  # Validate top_k.
  if (length(top_k) != 1 ||
      !is.numeric(top_k) ||
      is.na(top_k) ||
      !is.finite(top_k) ||
      top_k < 1) {
    stop("top_k must be a positive number.")
  }
  
  top_k <- as.integer(top_k)
  
  # Validate block_size.
  if (length(block_size) != 1 ||
      !is.numeric(block_size) ||
      is.na(block_size) ||
      !is.finite(block_size) ||
      block_size < 1 ||
      block_size != floor(block_size)) {
    stop("block_size must be a positive integer.")
  }
  
  block_size <- as.integer(block_size)
  
  # Encode the question using the same model as the index.
  query_embedding <- embedding_model$encode(
    question,
    normalize_embeddings = TRUE,
    convert_to_numpy = TRUE,
    show_progress_bar = FALSE
  )
  
  query_vector <- as.numeric(query_embedding)
  
  if (length(query_vector) != ncol(index_embeddings)) {
    stop("Query embedding dimension does not match the index.")
  }
  
  # Apply optional filters before calculating similarity.
  candidates <- seq_len(nrow(index_metadata))
  
  # FIXED COMPANY FILTER:
  # Case-insensitive literal substring matching.
  if (!is.null(company)) {
    
    if (!is.character(company) ||
        length(company) != 1 ||
        is.na(company)) {
      stop("company must be one non-missing string or NULL.")
    }
    
    company <- trimws(company)
    
    if (nzchar(company)) {
      
      company_names <- as.character(index_metadata$company_name)
      
      company_match <- rep(FALSE, length(company_names))
      
      valid_names <- !is.na(company_names)
      
      company_match[valid_names] <- grepl(
        pattern = tolower(company),
        x = tolower(company_names[valid_names]),
        fixed = TRUE
      )
      
      candidates <- candidates[
        company_match[candidates]
      ]
    }
  }
  
  # Optional fiscal-year filter.
  if (!is.null(fiscal_year)) {
    
    if (length(fiscal_year) != 1 ||
        is.na(fiscal_year)) {
      stop("fiscal_year must be one valid year or NULL.")
    }
    
    candidates <- candidates[
      !is.na(index_metadata$fiscal_year[candidates]) &
        as.character(index_metadata$fiscal_year[candidates]) ==
        as.character(fiscal_year)
    ]
  }
  
  if (length(candidates) == 0) {
    cat("No passages matched the supplied filters.\n")
    return(index_metadata[0, , drop = FALSE])
  }
  
  cat(
    "Scoring",
    length(candidates),
    "candidate passages...\n"
  )
  
  # Score candidates in blocks to limit temporary memory use.
  similarities <- numeric(length(candidates))
  
  starts <- seq.int(
    1L,
    length(candidates),
    by = block_size
  )
  
  for (start in starts) {
    
    end <- min(
      start + block_size - 1L,
      length(candidates)
    )
    
    positions <- start:end
    rows <- candidates[positions]
    
    embedding_block <- index_embeddings[
      rows,
      ,
      drop = FALSE
    ]
    
    similarities[positions] <- as.numeric(
      embedding_block %*% query_vector
    )
    
    rm(embedding_block)
  }
  
  # Select the highest cosine-similarity scores.
  ranked_positions <- order(
    similarities,
    decreasing = TRUE
  )
  
  ranked_positions <- head(
    ranked_positions,
    min(top_k, length(ranked_positions))
  )
  
  selected_rows <- candidates[ranked_positions]
  
  results <- index_metadata[
    selected_rows,
    ,
    drop = FALSE
  ]
  
  results$similarity <- similarities[ranked_positions]
  results$chunk_text <- index_text[selected_rows]
  
  results <- results[
    order(results$similarity, decreasing = TRUE),
    ,
    drop = FALSE
  ]
  
  rownames(results) <- NULL
  
  return(results)
}

cat("Retrieval function defined successfully.\n")

# ------------------------------------------------------------
# 6. Run a test query
# ------------------------------------------------------------

test_question <- paste(
  "What financial risks, liquidity pressures, debt obligations,",
  "and uncertainties does the company disclose?"
)

cat("\nRunning Phase 9A test query...\n")

test_results <- retrieve_passages(
  question = test_question,
  top_k = 5
)

if (nrow(test_results) == 0) {
  stop("The test query returned no passages.")
}

# Save complete results, including source links and text.
results_file <- file.path(
  output_dir,
  "phase9a_test_retrieval_results.csv"
)

write.csv(
  test_results,
  results_file,
  row.names = FALSE,
  na = ""
)

# Save a compact review file.
review_columns <- c(
  "chunk_id",
  "company_name",
  "fiscal_year",
  "similarity",
  "source_url",
  "chunk_text"
)

missing_review_columns <- setdiff(
  review_columns,
  names(test_results)
)

if (length(missing_review_columns) > 0) {
  stop(
    "Test results are missing required review columns: ",
    paste(missing_review_columns, collapse = ", ")
  )
}

review_results <- test_results[
  ,
  review_columns,
  drop = FALSE
]

review_file <- file.path(
  output_dir,
  "phase9a_test_retrieval_review.csv"
)

write.csv(
  review_results,
  review_file,
  row.names = FALSE,
  na = ""
)

cat("\nPhase 9A retrieval test completed.\n")
cat("Results saved to:\n", results_file, "\n")
cat("Review file saved to:\n", review_file, "\n")

cat("\nTop retrieved passages:\n")

for (i in seq_len(nrow(review_results))) {
  
  cat("\n----------------------------------------\n")
  cat("Rank:", i, "\n")
  cat("Company:", review_results$company_name[i], "\n")
  cat("Fiscal year:", review_results$fiscal_year[i], "\n")
  cat(
    "Similarity:",
    round(review_results$similarity[i], 4),
    "\n"
  )
  cat("SEC source:", review_results$source_url[i], "\n")
  cat(
    "Passage:",
    substr(review_results$chunk_text[i], 1, 700),
    "\n"
  )
}
