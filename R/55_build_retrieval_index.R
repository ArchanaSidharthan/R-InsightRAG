# ============================================================
# R-InsightRAG - Phase 7B
# Build Local Retrieval Index
# ============================================================

library(readr)
library(dplyr)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

batch_dir <- "data/processed/embedding_batches"
index_dir <- "data/processed/retrieval_index"

dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================\n")
cat("PHASE 7B: BUILD RETRIEVAL INDEX\n")
cat("============================================\n")

batch_files <- list.files(
  batch_dir,
  pattern = "^embedding_batch_[0-9]+\\.rds$",
  full.names = TRUE
)

if (length(batch_files) != 278) {
  stop("Expected 278 validated batches. Found: ", length(batch_files))
}

batch_numbers <- as.integer(
  sub(
    "^embedding_batch_([0-9]+)\\.rds$",
    "\\1",
    basename(batch_files)
  )
)

batch_files <- batch_files[order(batch_numbers)]

metadata_parts <- vector("list", length(batch_files))
embedding_parts <- vector("list", length(batch_files))

for (i in seq_along(batch_files)) {
  
  cat(sprintf(
    "Loading batch %d of %d: %s\n",
    i, length(batch_files), basename(batch_files[i])
  ))
  
  obj <- readRDS(batch_files[i])
  
  metadata <- as.data.frame(obj$metadata)
  embeddings <- obj$embeddings
  
  if (!is.matrix(embeddings) ||
      nrow(embeddings) != nrow(metadata) ||
      ncol(embeddings) != 384 ||
      any(!is.finite(embeddings))) {
    stop("Invalid embedding batch: ", basename(batch_files[i]))
  }
  
  metadata_parts[[i]] <- metadata
  embedding_parts[[i]] <- embeddings
}

# Combine metadata and embeddings
index_metadata <- bind_rows(metadata_parts)
index_embeddings <- do.call(rbind, embedding_parts)

if (nrow(index_metadata) != 554160 ||
    nrow(index_embeddings) != 554160 ||
    ncol(index_embeddings) != 384) {
  stop("Combined index dimensions do not match expected values.")
}

if (anyDuplicated(index_metadata$chunk_id) > 0) {
  stop("Duplicate chunk IDs found in combined index.")
}

# Save index components separately
metadata_path <- file.path(index_dir, "retrieval_metadata.rds")
embedding_path <- file.path(index_dir, "retrieval_embeddings.rds")

saveRDS(index_metadata, metadata_path, compress = FALSE)
saveRDS(index_embeddings, embedding_path, compress = FALSE)

# Save index information
index_info <- data.frame(
  metric = c(
    "total_chunks",
    "embedding_dimension",
    "batch_count",
    "embedding_model",
    "similarity_metric"
  ),
  value = c(
    nrow(index_metadata),
    ncol(index_embeddings),
    length(batch_files),
    "sentence-transformers/all-MiniLM-L6-v2",
    "cosine_similarity"
  )
)

write_csv(
  index_info,
  file.path(index_dir, "retrieval_index_info.csv")
)

cat("\n============================================\n")
cat("PHASE 7B COMPLETE\n")
cat("============================================\n")
cat("Indexed chunks:", nrow(index_metadata), "\n")
cat("Embedding dimensions:", ncol(index_embeddings), "\n")
cat("Metadata:", metadata_path, "\n")
cat("Embeddings:", embedding_path, "\n")
cat("Index info:", file.path(index_dir, "retrieval_index_info.csv"), "\n")
