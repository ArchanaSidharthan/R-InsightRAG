
# ============================================================
# R-InsightRAG - Phase 8B
# Debt and Liquidity Retrieval Improvement Experiment
# ============================================================

library(readr)
library(dplyr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

# Load existing search function and local embedding model
source("R/56_test_semantic_search.R")

# Use a separate output directory for this experiment
phase8b_dir <- "evaluation/phase8b"
dir.create(phase8b_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Define original and expanded questions
# ------------------------------------------------------------

original_question <- paste(
  "What are the company's major debt and liquidity risks?"
)

expanded_question <- paste(
  "Discuss the company's debt obligations, outstanding borrowings,",
  "short-term and long-term debt, credit facilities, debt maturities,",
  "interest expense, interest rates, debt covenants, refinancing risk,",
  "liquidity position, cash requirements, access to financing,",
  "ability to meet obligations, and sources of liquidity."
)

# ------------------------------------------------------------
# 2. Run both searches
# ------------------------------------------------------------

cat("\nSearching with the original question...\n")

original_results <- semantic_search(
  question = original_question,
  top_k = 10
)

original_results$query_version <- "Original"
original_results$query_text <- original_question

cat("\nSearching with the expanded question...\n")

expanded_results <- semantic_search(
  question = expanded_question,
  top_k = 10
)

expanded_results$query_version <- "Expanded"
expanded_results$query_text <- expanded_question

# ------------------------------------------------------------
# 3. Combine results
# ------------------------------------------------------------

comparison <- bind_rows(
  original_results,
  expanded_results
)

# Attach actual passage text
chunks <- read_csv(
  "data/processed/sec_document_chunks_unique.csv",
  show_col_types = FALSE
) %>%
  select(chunk_id, chunk_text)

comparison <- comparison %>%
  left_join(chunks, by = "chunk_id") %>%
  group_by(query_version) %>%
  arrange(desc(similarity), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(
    query_version,
    query_text,
    rank,
    company_name,
    fiscal_year,
    similarity,
    chunk_id,
    chunk_text,
    accession_number,
    source_url
  )

# ------------------------------------------------------------
# 4. Save the comparison
# ------------------------------------------------------------

comparison_file <- file.path(
  phase8b_dir,
  "debt_liquidity_retrieval_comparison.csv"
)

write_csv(comparison, comparison_file, na = "")

# Save a compact ranking summary
ranking_summary <- comparison %>%
  group_by(query_version) %>%
  summarise(
    results_returned = n(),
    unique_companies = n_distinct(company_name),
    mean_similarity = mean(similarity, na.rm = TRUE),
    .groups = "drop"
  )

summary_file <- file.path(
  phase8b_dir,
  "debt_liquidity_retrieval_summary.csv"
)

write_csv(ranking_summary, summary_file)

# ------------------------------------------------------------
# 5. Display top five for each version
# ------------------------------------------------------------

for (version in c("Original", "Expanded")) {
  
  cat("\n========================================\n")
  cat(version, "QUERY: TOP FIVE RESULTS\n")
  cat("========================================\n")
  
  displayed <- comparison %>%
    filter(query_version == version) %>%
    slice_head(n = 5) %>%
    select(rank, company_name, fiscal_year, similarity, chunk_id)
  
  print(displayed, n = 5, width = Inf)
}

cat("\nPhase 8B experiment completed.\n")
cat("Detailed passages:", comparison_file, "\n")
cat("Summary:", summary_file, "\n")
