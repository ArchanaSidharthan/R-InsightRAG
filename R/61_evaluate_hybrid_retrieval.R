
# ============================================================
# R-InsightRAG — Phase 8D
# Compare semantic retrieval vs hybrid retrieval
# ============================================================

rm(list = intersect(
  c("phase8d_results", "phase8d_dir", "phase8d_cases"),
  ls(envir = .GlobalEnv)
))

# Ensure the working directory is the project root.
project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

# Output directory: use a dedicated variable to avoid
# conflicts with variables in scripts sourced later.
phase8d_dir <- file.path("evaluation", "phase8d")
dir.create(phase8d_dir, recursive = TRUE, showWarnings = FALSE)

# Load the existing semantic_search() function and index.
# This script also runs its demonstration query.
source(file.path("R", "56_test_semantic_search.R"))

# ------------------------------------------------------------
# 1. Evaluation queries
# ------------------------------------------------------------

phase8d_cases <- data.frame(
  query_id = sprintf("Q%02d", 1:8),
  question = c(
    "What are the principal business risks, uncertainties, and risk factors disclosed by the company?",
    "What factors affected the company's revenue and sales performance?",
    "What does the company disclose about operating cash flows?",
    "What are the company's major debt and liquidity risks?",
    "What factors affected operating profitability and profit margins?",
    "What does the company disclose about competition in its industry?",
    "What risks and challenges are associated with acquisitions and integration?",
    "How do economic conditions affect the company's business and financial performance?"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 2. Keywords for lexical reranking
# ------------------------------------------------------------

phase8d_keywords <- c(
  "debt", "liquidity", "borrowings", "borrowing",
  "credit facility", "credit facilities", "credit agreement",
  "refinancing", "covenant", "covenants", "maturities",
  "interest rate", "interest expense", "cash requirements",
  "financing", "financial obligations"
)

# Count distinct keyword phrases present in a passage.
phase8d_keyword_count <- function(txt, keywords) {
  if (is.na(txt) || !nzchar(txt)) return(0L)
  
  txt <- tolower(txt)
  
  sum(vapply(
    keywords,
    function(term) grepl(term, txt, fixed = TRUE),
    logical(1)
  ))
}

# ------------------------------------------------------------
# 3. Load chunk text for passage inspection
# ------------------------------------------------------------

chunk_file <- file.path(
  "data", "processed", "sec_document_chunks_unique.csv"
)

if (!file.exists(chunk_file)) {
  stop("Missing unique chunk file: ", chunk_file)
}

chunks <- readr::read_csv(
  chunk_file,
  show_col_types = FALSE,
  progress = TRUE
)

if (!all(c("chunk_id", "chunk_text") %in% names(chunks))) {
  stop("The chunk file must contain chunk_id and chunk_text.")
}

chunks <- chunks[
  !duplicated(chunks$chunk_id),
  c("chunk_id", "chunk_text")
]

# ------------------------------------------------------------
# 4. Run semantic and hybrid retrieval for every query
# ------------------------------------------------------------

phase8d_rows <- list()
row_counter <- 1L

for (i in seq_len(nrow(phase8d_cases))) {
  
  qid <- phase8d_cases$query_id[i]
  question <- phase8d_cases$question[i]
  
  message(
    "\nProcessing ", qid, " of ",
    nrow(phase8d_cases), ": ", question
  )
  
  # Retrieve one shared candidate pool for a fair comparison.
  candidates <- semantic_search(question, top_k = 100)
  
  if (is.null(candidates) || nrow(candidates) == 0) {
    warning("No candidates returned for ", qid)
    next
  }
  
  # Make sure the semantic score is available.
  if (!"semantic_score" %in% names(candidates)) {
    if ("similarity" %in% names(candidates)) {
      candidates$semantic_score <- candidates$similarity
    } else {
      stop(
        "Could not identify semantic score column for ",
        qid, ". Available columns: ",
        paste(names(candidates), collapse = ", ")
      )
    }
  }
  
  # Count keyword matches using passage text from the chunks file.
  if (!"chunk_text" %in% names(candidates)) {
    candidates <- dplyr::left_join(
      candidates,
      chunks,
      by = "chunk_id"
    )
  }
  
  candidates$keyword_count <- vapply(
    candidates$chunk_text,
    phase8d_keyword_count,
    integer(1),
    keywords = phase8d_keywords
  )
  
  max_keyword_count <- max(candidates$keyword_count, na.rm = TRUE)
  
  if (!is.finite(max_keyword_count) || max_keyword_count <= 0) {
    candidates$keyword_score <- 0
  } else {
    candidates$keyword_score <-
      candidates$keyword_count / max_keyword_count
  }
  
  # Keep the same experimental scoring formula as Phase 8C.
  candidates$hybrid_score <-
    0.70 * candidates$semantic_score +
    0.30 * candidates$keyword_score
  
  # Semantic baseline: top five by semantic similarity.
  semantic_top5 <- candidates[
    order(candidates$semantic_score, decreasing = TRUE),
  ]
  semantic_top5 <- head(semantic_top5, 5)
  
  # Hybrid result: rerank the same top-100 candidate pool.
  hybrid_top5 <- candidates[
    order(candidates$hybrid_score, decreasing = TRUE),
  ]
  hybrid_top5 <- head(hybrid_top5, 5)
  
  # Add query details and method labels.
  semantic_top5$query_id <- qid
  semantic_top5$question <- question
  semantic_top5$method <- "Semantic"
  semantic_top5$rank <- seq_len(nrow(semantic_top5))
  
  hybrid_top5$query_id <- qid
  hybrid_top5$question <- question
  hybrid_top5$method <- "Hybrid"
  hybrid_top5$rank <- seq_len(nrow(hybrid_top5))
  
  # Store results from both methods.
  phase8d_rows[[row_counter]] <- semantic_top5
  row_counter <- row_counter + 1L
  
  phase8d_rows[[row_counter]] <- hybrid_top5
  row_counter <- row_counter + 1L
  
  message(
    "Completed ", qid,
    " | Semantic results: ", nrow(semantic_top5),
    " | Hybrid results: ", nrow(hybrid_top5)
  )
}

if (length(phase8d_rows) == 0) {
  stop("No retrieval results were produced.")
}

phase8d_results <- dplyr::bind_rows(phase8d_rows)

# ------------------------------------------------------------
# 5. Join source metadata and prepare review fields
# ------------------------------------------------------------

metadata_file <- file.path(
  "data", "processed", "retrieval_index",
  "retrieval_metadata.rds"
)

if (!file.exists(metadata_file)) {
  stop("Missing retrieval metadata: ", metadata_file)
}

retrieval_metadata <- readRDS(metadata_file)

# Avoid duplicated columns when metadata was already returned.
metadata_cols <- intersect(
  c(
    "chunk_id", "company_id", "company_name", "fiscal_year",
    "accession_number", "source_url", "source_text_path",
    "chunk_index", "character_count", "word_count"
  ),
  names(retrieval_metadata)
)

metadata_small <- retrieval_metadata[, metadata_cols, drop = FALSE]

metadata_small <- metadata_small[
  !duplicated(metadata_small$chunk_id),
]

metadata_to_join <- setdiff(
  names(metadata_small),
  names(phase8d_results)
)

if (length(metadata_to_join) > 0) {
  phase8d_results <- dplyr::left_join(
    phase8d_results,
    metadata_small[, c("chunk_id", metadata_to_join), drop = FALSE],
    by = "chunk_id"
  )
}

# Preserve blank relevance labels for manual review.
phase8d_results$relevant <- NA_integer_
phase8d_results$review_notes <- ""

# Arrange for easier spreadsheet review.
phase8d_results <- phase8d_results[
  order(
    phase8d_results$query_id,
    phase8d_results$method,
    phase8d_results$rank
  ),
]

# Keep useful columns first, then retain any additional fields.
preferred_columns <- c(
  "query_id", "question", "method", "rank",
  "company_name", "fiscal_year", "chunk_id",
  "semantic_score", "keyword_count", "keyword_score",
  "hybrid_score", "relevant", "review_notes",
  "chunk_text", "source_url", "accession_number"
)

output_columns <- c(
  intersect(preferred_columns, names(phase8d_results)),
  setdiff(preferred_columns, names(phase8d_results)),
  setdiff(names(phase8d_results), preferred_columns)
)

phase8d_results <- phase8d_results[
  , unique(output_columns), drop = FALSE
]

# ------------------------------------------------------------
# 6. Save results and summary
# ------------------------------------------------------------

results_file <- file.path(
  phase8d_dir, "hybrid_top5_results.csv"
)

summary_file <- file.path(
  phase8d_dir, "hybrid_retrieval_run_summary.csv"
)

readr::write_csv(phase8d_results, results_file)

run_summary <- phase8d_results |>
  dplyr::group_by(query_id, question, method) |>
  dplyr::summarise(
    result_count = dplyr::n(),
    unique_companies = dplyr::n_distinct(company_name, na.rm = TRUE),
    mean_semantic_score = mean(semantic_score, na.rm = TRUE),
    mean_keyword_count = mean(keyword_count, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(run_summary, summary_file)

# ------------------------------------------------------------
# 7. Report
# ------------------------------------------------------------

cat("\n============================================\n")
cat("PHASE 8D — RETRIEVAL EVALUATION COMPLETE\n")
cat("============================================\n")
cat("Queries processed: ", dplyr::n_distinct(phase8d_results$query_id), "\n")
cat("Total result rows: ", nrow(phase8d_results), "\n")
cat("Expected maximum: 80 (8 queries x 2 methods x 5 results)\n")
cat("Results file: ", normalizePath(results_file), "\n")
cat("Summary file: ", normalizePath(summary_file), "\n")
cat("\nResults by method:\n")
print(
  phase8d_results |>
    dplyr::count(method, name = "passages")
)

cat("\nResults by query and method:\n")
print(
  run_summary |>
    dplyr::select(query_id, method, result_count, unique_companies)
)

cat("\nRelevance labels are intentionally blank.\n")
cat("Review the passages before calculating Precision@5 or MRR.\n")
