
# ============================================================
# R-InsightRAG - Phase 9B Alternative
# Evidence-only financial research report
# No OpenAI API or paid LLM required
# ============================================================

project_root <- "C:/Users/archa/OneDrive/Desktop/Projects/R-InsightRAG"

input_file <- file.path(
  project_root,
  "outputs",
  "phase9a",
  "phase9a_test_retrieval_review.csv"
)

output_dir <- file.path(
  project_root,
  "outputs",
  "phase9b_evidence_only"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 1. Validate and load existing retrieval results
# ------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    "Retrieval review file not found:\n",
    input_file,
    "\nRun R/63_build_rag_retrieval.R first."
  )
}

cat("Loading saved SEC retrieval results...\n")

results <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  colClasses = "character",
  na.strings = character()
)

required_columns <- c(
  "chunk_id",
  "company_name",
  "fiscal_year",
  "similarity",
  "source_url",
  "chunk_text"
)

missing_columns <- setdiff(required_columns, names(results))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

if (nrow(results) == 0) {
  stop("The retrieval results file contains no passages.")
}

# Convert similarity to numeric and validate it.
results$similarity <- suppressWarnings(
  as.numeric(results$similarity)
)

if (anyNA(results$similarity)) {
  stop("Some similarity scores are missing or invalid.")
}

if (any(!grepl("^https://www\\.sec\\.gov/", results$source_url))) {
  stop("A source URL is missing or is not an SEC HTTPS URL.")
}

if (anyNA(results$chunk_text) ||
    any(!nzchar(trimws(results$chunk_text)))) {
  stop("Some retrieved passages have missing text.")
}

# Preserve retrieval ranking, then label sources.
results$retrieval_rank <- seq_len(nrow(results))
results$source_id <- paste0("S", results$retrieval_rank)

cat("Passages loaded:", nrow(results), "\n")

# ------------------------------------------------------------
# 2. Add evidence-only descriptions
# ------------------------------------------------------------
# These labels describe the retrieved evidence.
# They are not AI-generated financial conclusions.

results$evidence_type <- vapply(
  results$chunk_text,
  function(txt) {
    txt_lower <- tolower(txt)
    
    categories <- c(
      "Debt and financing" =
        "debt|borrowing|credit facility|refinanc|covenant",
      "Liquidity and cash flow" =
        "liquidity|cash flow|working capital|cash requirements",
      "Business risks and uncertainties" =
        "risk|uncertaint|adverse effect|market risk",
      "Commitments and obligations" =
        "commitment|obligation|contractual|off-balance sheet",
      "Revenue and operations" =
        "revenue|net sales|sales growth|operating results"
    )
    
    matches <- names(categories)[
      vapply(
        categories,
        function(pattern) grepl(pattern, txt_lower),
        logical(1)
      )
    ]
    
    if (length(matches) == 0) {
      "Other retrieved evidence"
    } else {
      paste(matches, collapse = "; ")
    }
  },
  character(1)
)

# ------------------------------------------------------------
# 3. Export the evidence CSV
# ------------------------------------------------------------

export_columns <- c(
  "source_id",
  "retrieval_rank",
  "chunk_id",
  "company_name",
  "fiscal_year",
  "similarity",
  "evidence_type",
  "source_url",
  "chunk_text"
)

evidence_export <- results[
  ,
  export_columns,
  drop = FALSE
]

csv_file <- file.path(
  output_dir,
  "sec_evidence_report.csv"
)

write.csv(
  evidence_export,
  csv_file,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 4. HTML escaping helper
# ------------------------------------------------------------

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

# ------------------------------------------------------------
# 5. Build a readable HTML report
# ------------------------------------------------------------

generated_at <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S"
)

company_count <- length(unique(results$company_name))
year_count <- length(unique(results$fiscal_year))

html_parts <- c(
  "<!DOCTYPE html>",
  "<html lang='en'>",
  "<head>",
  "<meta charset='UTF-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>R-InsightRAG SEC Evidence Report</title>",
  "<style>",
  "body { font-family: Arial, sans-serif; max-width: 1100px; margin: 30px auto; padding: 0 18px; line-height: 1.6; color: #222; }",
  "h1, h2 { color: #17365D; }",
  ".notice { border: 1px solid #999; padding: 14px; margin: 20px 0; }",
  ".metadata { color: #555; }",
  ".passage { border: 1px solid #ddd; padding: 18px; margin: 18px 0; border-radius: 6px; }",
  ".score { font-weight: bold; }",
  ".text { white-space: pre-wrap; overflow-wrap: anywhere; }",
  "a { overflow-wrap: anywhere; }",
  "table { border-collapse: collapse; width: 100%; }",
  "th, td { border: 1px solid #ddd; padding: 9px; text-align: left; }",
  "th { background: #f2f5f8; }",
  "</style>",
  "</head>",
  "<body>",
  "<h1>R-InsightRAG: SEC Evidence Report</h1>",
  paste0("<p class='metadata'>Generated: ", html_escape(generated_at), "</p>"),
  "<div class='notice'>",
  "<strong>Evidence-only report.</strong> This report presents passages retrieved from SEC filings. It does not generate an AI financial conclusion or independently verify every statement in the filings. Similarity scores measure relevance to the search query, not truth, financial importance, or company performance.",
  "</div>",
  "<h2>Report overview</h2>",
  paste0("<p>Retrieved passages: ", nrow(results), "</p>"),
  paste0("<p>Distinct companies: ", company_count, "</p>"),
  paste0("<p>Distinct fiscal years represented: ", year_count, "</p>"),
  "<h2>Retrieved evidence</h2>"
)

for (i in seq_len(nrow(results))) {
  html_parts <- c(
    html_parts,
    "<section class='passage'>",
    paste0("<h3>", html_escape(results$source_id[i]),
           " — Rank ", results$retrieval_rank[i], "</h3>"),
    paste0("<p><strong>Company:</strong> ",
           html_escape(results$company_name[i]), "</p>"),
    paste0("<p><strong>Fiscal year:</strong> ",
           html_escape(results$fiscal_year[i]), "</p>"),
    paste0("<p><strong>Evidence category (keyword-based):</strong> ",
           html_escape(results$evidence_type[i]), "</p>"),
    paste0("<p class='score'>Cosine similarity: ",
           sprintf("%.4f", results$similarity[i]), "</p>"),
    paste0("<p><strong>SEC filing:</strong> <a href='",
           html_escape(results$source_url[i]),
           "' target='_blank' rel='noopener noreferrer'>Open source filing</a></p>"),
    "<h4>Retrieved passage</h4>",
    paste0("<div class='text'>",
           html_escape(results$chunk_text[i]),
           "</div>"),
    "</section>"
  )
}

html_parts <- c(
  html_parts,
  "<h2>Interpretation limits</h2>",
  "<ul>",
  "<li>Passages are selected by semantic similarity to the retrieval query.</li>",
  "<li>The evidence category is based on keyword matching and may be imperfect.</li>",
  "<li>Passages may be excerpts rather than complete sections of a filing.</li>",
  "<li>Check the original SEC filing for context before drawing conclusions.</li>",
  "</ul>",
  "</body>",
  "</html>"
)

html_file <- file.path(
  output_dir,
  "sec_evidence_report.html"
)

writeLines(
  html_parts,
  html_file,
  useBytes = TRUE
)

# ------------------------------------------------------------
# 6. Export a plain-text summary
# ------------------------------------------------------------

text_file <- file.path(
  output_dir,
  "sec_evidence_report.txt"
)

text_lines <- c(
  "R-InsightRAG — SEC EVIDENCE REPORT",
  paste("Generated:", generated_at),
  paste("Retrieved passages:", nrow(results)),
  paste("Distinct companies:", company_count),
  "",
  "IMPORTANT: Evidence only. No AI-generated conclusions.",
  "Similarity is a retrieval score, not a measure of truth.",
  ""
)

for (i in seq_len(nrow(results))) {
  text_lines <- c(
    text_lines,
    paste0("SOURCE ", results$source_id[i]),
    paste0("Rank: ", results$retrieval_rank[i]),
    paste0("Company: ", results$company_name[i]),
    paste0("Fiscal year: ", results$fiscal_year[i]),
    paste0("Similarity: ", sprintf("%.4f", results$similarity[i])),
    paste0("Evidence category: ", results$evidence_type[i]),
    paste0("SEC URL: ", results$source_url[i]),
    "",
    "PASSAGE:",
    results$chunk_text[i],
    "",
    "----------------------------------------",
    ""
  )
}

writeLines(
  text_lines,
  text_file,
  useBytes = TRUE
)

# ------------------------------------------------------------
# 7. Verify saved files
# ------------------------------------------------------------

cat("\n====================================\n")
cat("EVIDENCE REPORT COMPLETED\n")
cat("====================================\n")

output_files <- c(csv_file, html_file, text_file)

for (f in output_files) {
  cat(
    if (file.exists(f)) "[OK] " else "[MISSING] ",
    f,
    "\n"
  )
}

cat("\nRetrieved passages:", nrow(results), "\n")
cat("Distinct companies:", company_count, "\n")
cat("Output directory:", output_dir, "\n")
