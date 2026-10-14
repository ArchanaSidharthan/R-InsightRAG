
# ============================================================
# R-InsightRAG - Phase 5E
# Clean and Chunk SEC Annual Report Text
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(tibble)

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"
setwd(project_root)

input_manifest <- "data/processed/sec_text_extraction_manifest.csv"
output_chunks <- "data/processed/sec_document_chunks.csv"
quality_output <- "outputs/phase5e_chunk_quality_summary.csv"

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_manifest)) {
  stop("Extraction manifest not found: ", input_manifest)
}

manifest <- read_csv(input_manifest, show_col_types = FALSE) %>%
  filter(extraction_status == "Extracted")

# Chunk settings: approximately 1,200 characters per chunk,
# with 200 characters of overlap between consecutive chunks.
chunk_size <- 1200L
chunk_overlap <- 200L
step_size <- chunk_size - chunk_overlap

if (chunk_overlap >= chunk_size || chunk_size <= 0) {
  stop("Invalid chunk size or overlap.")
}

# Clean extracted report text conservatively.
clean_report_text <- function(text) {
  
  text <- str_replace_all(text, "\u00a0", " ")
  text <- str_replace_all(text, "\r\n?", "\n")
  text <- str_replace_all(text, "[ \t]+", " ")
  text <- str_replace_all(text, "\n[ \t]+", "\n")
  text <- str_replace_all(text, "\n{3,}", "\n\n")
  
  str_trim(text)
}

# Split text into overlapping chunks, preferring a nearby
# sentence or paragraph boundary where possible.
make_chunks <- function(text, size = 1200L, overlap = 200L) {
  
  text <- clean_report_text(text)
  
  if (is.na(text) || !nzchar(text)) {
    return(character(0))
  }
  
  text_length <- nchar(text, type = "chars")
  step <- size - overlap
  chunks <- character(0)
  start <- 1L
  
  while (start <= text_length) {
    
    end <- min(start + size - 1L, text_length)
    
    if (end < text_length) {
      
      candidate <- substr(
        text,
        start,
        min(end + 1L, text_length)
      )
      
      # Prefer a paragraph break in the last part of the chunk.
      paragraph_breaks <- gregexpr("\n\n", candidate, fixed = TRUE)[[1]]
      
      if (paragraph_breaks[1] != -1) {
        
        valid_breaks <- paragraph_breaks[
          paragraph_breaks >= floor(size * 0.65)
        ]
        
        if (length(valid_breaks) > 0) {
          end <- start + valid_breaks[length(valid_breaks)] - 2L
        }
        
      } else {
        
        # Otherwise prefer a sentence boundary.
        sentence_ends <- gregexpr("[.!?]\\s", candidate, perl = TRUE)[[1]]
        
        if (sentence_ends[1] != -1) {
          
          valid_ends <- sentence_ends[
            sentence_ends >= floor(size * 0.65)
          ]
          
          if (length(valid_ends) > 0) {
            end <- start + valid_ends[length(valid_ends)] - 1L
          }
        }
      }
    }
    
    if (end < start) {
      end <- min(start + size - 1L, text_length)
    }
    
    chunk <- str_trim(substr(text, start, end))
    
    if (nzchar(chunk)) {
      chunks <- c(chunks, chunk)
    }
    
    if (end >= text_length) {
      break
    }
    
    # Move forward while retaining overlap.
    start <- max(start + 1L, end - overlap + 1L)
  }
  
  chunks
}

all_chunks <- vector("list", nrow(manifest))

cat("\n============================================\n")
cat("PHASE 5E: DOCUMENT CHUNKING\n")
cat("============================================\n")
cat("Reports to process:", nrow(manifest), "\n\n")

for (i in seq_len(nrow(manifest))) {
  
  text_path <- as.character(manifest$extracted_text_path[i])
  
  if (is.na(text_path) || !file.exists(text_path)) {
    all_chunks[[i]] <- tibble()
    next
  }
  
  report_text <- tryCatch(
    paste(readLines(text_path, warn = FALSE, encoding = "UTF-8"),
          collapse = "\n"),
    error = function(e) NA_character_
  )
  
  if (is.na(report_text) || !nzchar(report_text)) {
    all_chunks[[i]] <- tibble()
    next
  }
  
  chunks <- make_chunks(
    report_text,
    size = chunk_size,
    overlap = chunk_overlap
  )
  
  if (length(chunks) == 0) {
    all_chunks[[i]] <- tibble()
    next
  }
  
  all_chunks[[i]] <- tibble(
    company_id = as.character(manifest$company_id[i]),
    company_name = as.character(manifest$company_name[i]),
    fiscal_year = as.character(manifest$fiscal_year[i]),
    accession_number = as.character(manifest$accession_number[i]),
    source_url = as.character(manifest$primary_document_url[i]),
    source_text_path = text_path,
    chunk_index = seq_along(chunks),
    chunk_text = chunks,
    character_count = nchar(chunks, type = "chars"),
    word_count = str_count(chunks, "\\S+")
  )
  
  if (i %% 50 == 0 || i == nrow(manifest)) {
    cat(
      "Processed", i, "of", nrow(manifest),
      "| chunks created:",
      sum(vapply(all_chunks[seq_len(i)], nrow, integer(1))),
      "\n"
    )
  }
}

chunk_data <- bind_rows(all_chunks)

if (nrow(chunk_data) == 0) {
  stop("No chunks were created. Check the extracted text files.")
}

# Stable chunk identifier for later retrieval and citations.
chunk_data <- chunk_data %>%
  mutate(
    chunk_id = paste(
      company_id,
      fiscal_year,
      str_remove_all(accession_number, "-"),
      chunk_index,
      sep = "_"
    )
  ) %>%
  select(
    chunk_id,
    company_id,
    company_name,
    fiscal_year,
    accession_number,
    source_url,
    source_text_path,
    chunk_index,
    chunk_text,
    character_count,
    word_count
  )

write_csv(chunk_data, output_chunks, na = "")

quality <- chunk_data %>%
  summarise(
    total_chunks = n(),
    unique_companies = n_distinct(company_id),
    unique_reports = n_distinct(accession_number),
    min_characters = min(character_count),
    median_characters = median(character_count),
    max_characters = max(character_count),
    median_words = median(word_count),
    chunks_under_200_characters = sum(character_count < 200),
    chunks_over_1600_characters = sum(character_count > 1600),
    duplicate_chunk_texts = sum(duplicated(chunk_text))
  )

write_csv(quality, quality_output, na = "")

cat("\n============================================\n")
cat("PHASE 5E SUMMARY\n")
cat("============================================\n")
print(as.data.frame(quality))

cat("\nChunk dataset saved to:", output_chunks, "\n")
cat("Quality summary saved to:", quality_output, "\n")
cat("\nPhase 5E completed.\n")
