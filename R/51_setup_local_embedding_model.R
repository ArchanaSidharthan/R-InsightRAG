
# ============================================================
# R-InsightRAG - Phase 6A
# Set up local CPU sentence embeddings
# ============================================================

library(reticulate)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

# Install required packages in a managed Python environment.
# This may take several minutes.
py_require(c(
  "sentence-transformers",
  "torch"
))

# Load Python package
sentence_transformers <- import(
  "sentence_transformers",
  delay_load = FALSE
)

# Load model locally.
# The first run downloads the model files.
model <- sentence_transformers$SentenceTransformer(
  "sentence-transformers/all-MiniLM-L6-v2",
  device = "cpu"
)

# Test a single financial sentence.
test_text <- c(
  "Revenue increased because of strong customer demand.",
  "Operating expenses increased during the financial year.",
  "The company identified liquidity and credit risks."
)

embeddings <- model$encode(
  test_text,
  convert_to_numpy = TRUE,
  normalize_embeddings = TRUE
)

cat("\n============================================\n")
cat("LOCAL EMBEDDING TEST\n")
cat("============================================\n")
cat("Sentences:", length(test_text), "\n")
cat("Embedding dimensions:", dim(embeddings)[2], "\n")
cat("All values finite:", all(is.finite(embeddings)), "\n")

if (dim(embeddings)[1] != length(test_text)) {
  stop("Embedding row count does not match the input.")
}

if (dim(embeddings)[2] != 384) {
  stop("Unexpected embedding dimension.")
}

if (!all(is.finite(embeddings))) {
  stop("Embedding matrix contains non-finite values.")
}

cat("\nLocal CPU embedding test passed.\n")
