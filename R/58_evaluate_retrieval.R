
# ============================================================
# R-InsightRAG - Phase 8A
# Financial Semantic Retrieval Evaluation
# ============================================================

library(readr)
library(dplyr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

output_dir <- "evaluation/phase8a"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Create a reproducible financial question test set
# ------------------------------------------------------------

test_cases <- data.frame(
  query_id = sprintf("Q%02d", 1:8),
  question = c(
    "What are the principal business risks and uncertainties?",
    "What factors affect the company's revenue and sales?",
    "What does the company disclose about operating cash flows?",
    "What are the company's major debt and liquidity risks?",
    "What factors affect operating profitability and margins?",
    "What competitive pressures does the company face?",
    "What risks arise from acquisitions and integration?",
    "What economic conditions could affect business performance?"
  ),
  expected_topic = c(
    "Risk factors",
    "Revenue",
    "Cash flow",
    "Debt and liquidity",
    "Profitability",
    "Competition",
    "Acquisition risk",
    "Macroeconomic risk"
  ),
  stringsAsFactors = FALSE
)

test_file <- file.path(output_dir, "retrieval_test_cases.csv")

write_csv(test_cases, test_file)

cat("Evaluation questions saved:", test_file, "\n")

# ------------------------------------------------------------
# 2. Load the existing semantic search function
# ------------------------------------------------------------

# Phase 7C script also runs its original test query when sourced.
# That extra test is harmless; the evaluation below runs separately.

source("R/56_test_semantic_search.R")

# ------------------------------------------------------------
# 3. Run top-five retrieval for each question
# ------------------------------------------------------------

all_results <- vector("list", nrow(test_cases))

for (i in seq_len(nrow(test_cases))) {
  
  query_id <- test_cases$query_id[i]
  question <- test_cases$question[i]
  topic <- test_cases$expected_topic[i]
  
  cat("\n========================================\n")
  cat("Query:", query_id, "\n")
  cat("Question:", question, "\n")
  cat("========================================\n")
  
  retrieved <- semantic_search(
    question = question,
    top_k = 5
  )
  
  if (nrow(retrieved) == 0) {
    cat("No results returned.\n")
    next
  }
  
  retrieved$rank <- seq_len(nrow(retrieved))
  retrieved$query_id <- query_id
  retrieved$question <- question
  retrieved$expected_topic <- topic
  
  all_results[[i]] <- retrieved %>%
    select(
      query_id,
      question,
      expected_topic,
      rank,
      chunk_id,
      company_name,
      fiscal_year,
      similarity,
      accession_number,
      source_url
    )
  
  print(
    retrieved %>%
      select(rank, company_name, fiscal_year, similarity),
    row.names = FALSE
  )
}

# ------------------------------------------------------------
# 4. Combine and save all retrieval results
# ------------------------------------------------------------

valid_results <- Filter(Negate(is.null), all_results)

if (length(valid_results) == 0) {
  stop("No retrieval results were returned for any question.")
}

evaluation_results <- bind_rows(valid_results)

results_file <- file.path(
  output_dir,
  "retrieval_top5_results.csv"
)

write_csv(evaluation_results, results_file)

# ------------------------------------------------------------
# 5. Attach actual passage text for human relevance review
# ------------------------------------------------------------

cat("\nLoading source chunk text for relevance review...\n")

chunks <- read_csv(
  "data/processed/sec_document_chunks_unique.csv",
  show_col_types = FALSE
) %>%
  select(chunk_id, chunk_text)

review_results <- evaluation_results %>%
  left_join(chunks, by = "chunk_id") %>%
  mutate(
    # Leave relevance unjudged until a human reviews the passage.
    relevant = NA
  ) %>%
  select(
    query_id,
    question,
    expected_topic,
    rank,
    company_name,
    fiscal_year,
    similarity,
    chunk_id,
    relevant,
    chunk_text,
    accession_number,
    source_url
  )

review_file <- file.path(
  output_dir,
  "retrieval_relevance_review.csv"
)

write_csv(review_results, review_file)

# ------------------------------------------------------------
# 6. Report basic retrieval coverage
# ------------------------------------------------------------

coverage <- evaluation_results %>%
  count(query_id, name = "results_returned") %>%
  right_join(
    test_cases %>% select(query_id, question, expected_topic),
    by = "query_id"
  ) %>%
  mutate(
    results_returned = coalesce(results_returned, 0L),
    expected_results = 5L,
    returned_five = results_returned == expected_results
  )

coverage_file <- file.path(output_dir, "retrieval_coverage.csv")
write_csv(coverage, coverage_file)

cat("\n========================================\n")
cat("PHASE 8A RETRIEVAL RUN COMPLETE\n")
cat("========================================\n")
cat("Questions:", nrow(test_cases), "\n")
cat("Questions with five results:",
    sum(coverage$returned_five), "\n")
cat("Total retrieved passages:", nrow(evaluation_results), "\n")
cat("\nSaved files:\n")
cat(results_file, "\n")
cat(review_file, "\n")
cat(coverage_file, "\n")
cat("\nNext: inspect and label the relevance review file.\n")
