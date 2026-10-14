
# ============================================================
# R-InsightRAG - Phase 8C
# Hybrid Retrieval: Semantic + Keyword Matching
# ============================================================

library(readr)
library(dplyr)
library(stringr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

output_dir <- "evaluation/phase8c"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load the existing semantic search function and index.
# This also runs the original Phase 7C demonstration query.
source("R/56_test_semantic_search.R")

chunks <- read_csv(
  "data/processed/sec_document_chunks_unique.csv",
  show_col_types = FALSE
) %>%
  select(chunk_id, chunk_text)

# ------------------------------------------------------------
# 1. Define a hybrid search function
# ------------------------------------------------------------

hybrid_search <- function(question, top_k = 10) {
  
  # Semantic retrieval provides a broad candidate set.
  semantic_results <- semantic_search(
    question = question,
    top_k = 100
  )
  
  if (nrow(semantic_results) == 0) {
    return(data.frame())
  }
  
  # Retrieve the full text for semantic candidates.
  candidates <- semantic_results %>%
    left_join(chunks, by = "chunk_id")
  
  # Financial terms used for this experiment.
  keywords <- c(
    "debt",
    "liquidity",
    "borrowings",
    "borrowing",
    "credit facility",
    "credit facilities",
    "credit agreement",
    "refinancing",
    "covenant",
    "covenants",
    "maturities",
    "interest rate",
    "interest expense",
    "cash requirements",
    "financing",
    "financial obligations"
  )
  
  # Count distinct keyword matches in each passage.
  keyword_count <- vapply(
    candidates$chunk_text,
    function(txt) {
      if (is.na(txt) || !nzchar(txt)) return(0L)
      
      sum(vapply(
        keywords,
        function(term) {
          str_detect(
            str_to_lower(txt),
            fixed(str_to_lower(term))
          )
        },
        logical(1)
      ))
    },
    integer(1)
  )
  
  candidates$keyword_count <- keyword_count
  
  # Normalize keyword coverage to a 0-1 range.
  max_keywords <- max(candidates$keyword_count, 1L)
  
  candidates <- candidates %>%
    mutate(
      semantic_score = similarity,
      keyword_score = keyword_count / max_keywords,
      hybrid_score = 0.70 * semantic_score +
        0.30 * keyword_score
    ) %>%
    arrange(desc(hybrid_score)) %>%
    slice_head(n = top_k) %>%
    mutate(rank = row_number())
  
  candidates
}

# ------------------------------------------------------------
# 2. Compare original and expanded debt/liquidity queries
# ------------------------------------------------------------

queries <- data.frame(
  query_version = c("Original", "Expanded"),
  question = c(
    "What are the company's major debt and liquidity risks?",
    paste(
      "Discuss debt obligations, borrowings, credit facilities,",
      "debt maturities, interest expense, refinancing risk,",
      "liquidity, covenants, and access to financing."
    )
  ),
  stringsAsFactors = FALSE
)

all_results <- list()

for (i in seq_len(nrow(queries))) {
  
  cat("\nSearching:", queries$query_version[i], "\n")
  
  result <- hybrid_search(
    question = queries$question[i],
    top_k = 10
  )
  
  if (nrow(result) > 0) {
    result$query_version <- queries$query_version[i]
    result$query_text <- queries$question[i]
    all_results[[i]] <- result
  }
}

hybrid_results <- bind_rows(all_results)

# ------------------------------------------------------------
# 3. Save results and passage text
# ------------------------------------------------------------

output_file <- file.path(
  output_dir,
  "hybrid_debt_liquidity_results.csv"
)

write_csv(hybrid_results, output_file, na = "")

cat("\nTop five hybrid results by query:\n")

for (version in queries$query_version) {
  cat("\n", version, "\n")
  
  print(
    hybrid_results %>%
      filter(query_version == version) %>%
      slice_head(n = 5) %>%
      select(
        rank,
        company_name,
        fiscal_year,
        semantic_score,
        keyword_count,
        hybrid_score
      ),
    n = 5
  )
}

cat("\nSaved hybrid results to:", output_file, "\n")
cat("Phase 8C experiment completed.\n")
