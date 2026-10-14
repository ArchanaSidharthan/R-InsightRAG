# ============================================================
# R-InsightRAG
# Phase 2C - XBRL Financial Variable Inspection
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

project_path <- getwd()

sec_path <- file.path(
  project_path,
  "data",
  "raw",
  "sec_2025q4"
)

processed_path <- file.path(
  project_path,
  "data",
  "processed"
)

# ------------------------------------------------------------
# 2. File paths
# ------------------------------------------------------------

sub_file <- file.path(sec_path, "sub.txt")
num_file <- file.path(sec_path, "num.txt")
tag_file <- file.path(sec_path, "tag.txt")
pre_file <- file.path(sec_path, "pre.txt")

# ------------------------------------------------------------
# 3. Check files
# ------------------------------------------------------------

required_files <- c(
  sub_file,
  num_file,
  tag_file,
  pre_file
)

if (!all(file.exists(required_files))) {
  
  stop(
    "One or more SEC files are missing."
  )
  
}

cat("============================================\n")
cat(" R-InsightRAG - PHASE 2C\n")
cat(" XBRL VARIABLE INSPECTION\n")
cat("============================================\n\n")

# ------------------------------------------------------------
# 4. Read SUB
# ------------------------------------------------------------

cat("Reading SUB...\n")

sub_data <- read.delim(
  sub_file,
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = ""
)

cat(
  "SUB rows:",
  nrow(sub_data),
  "\n\n"
)

# ------------------------------------------------------------
# 5. Read NUM
# ------------------------------------------------------------

cat("Reading NUM...\n")

num_data <- read.delim(
  num_file,
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = ""
)

cat(
  "NUM rows:",
  nrow(num_data),
  "\n\n"
)

# ------------------------------------------------------------
# 6. Read TAG
# ------------------------------------------------------------

cat("Reading TAG...\n")

tag_data <- read.delim(
  tag_file,
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = ""
)

cat(
  "TAG rows:",
  nrow(tag_data),
  "\n\n"
)

# ------------------------------------------------------------
# 7. Read PRE
# ------------------------------------------------------------

cat("Reading PRE...\n")

pre_data <- read.delim(
  pre_file,
  sep = "\t",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = ""
)

cat(
  "PRE rows:",
  nrow(pre_data),
  "\n\n"
)

# ============================================================
# 8. Inspect filing types
# ============================================================

cat("============================================\n")
cat("FILING TYPES\n")
cat("============================================\n\n")

print(
  table(sub_data$form)
)

# ============================================================
# 9. Focus on annual filings
# ============================================================

annual_filings <- sub_data %>%
  filter(
    form == "10-K"
  )

cat("\nAnnual 10-K filings:\n")
cat(
  nrow(annual_filings),
  "\n\n"
)

# ============================================================
# 10. Inspect important XBRL tags
# ============================================================

important_keywords <- paste(
  c(
    "Revenue",
    "Sales",
    "NetIncome",
    "OperatingIncome",
    "Assets",
    "Liabilities",
    "StockholdersEquity",
    "Equity",
    "OperatingCashFlow",
    "CashProvided",
    "CashUsed",
    "OperatingExpenses",
    "ResearchAndDevelopment",
    "Employees"
  ),
  collapse = "|"
)

candidate_tags <- tag_data %>%
  filter(
    str_detect(
      tag,
      regex(
        important_keywords,
        ignore_case = TRUE
      )
    )
  )

cat("============================================\n")
cat("CANDIDATE XBRL TAGS\n")
cat("============================================\n\n")

print(
  candidate_tags %>%
    select(
      tag,
      version,
      custom,
      datatype,
      tlabel,
      doc
    ) %>%
    head(200)
)

# ============================================================
# 11. Revenue candidates
# ============================================================

revenue_tags <- tag_data %>%
  filter(
    str_detect(
      tag,
      regex(
        "Revenue|SalesRevenue",
        ignore_case = TRUE
      )
    )
  ) %>%
  select(
    tag,
    version,
    custom,
    tlabel,
    doc
  )

cat("\n============================================\n")
cat("REVENUE TAG CANDIDATES\n")
cat("============================================\n\n")

print(
  revenue_tags,
  n = 100
)

# ============================================================
# 12. Net income candidates
# ============================================================

net_income_tags <- tag_data %>%
  filter(
    str_detect(
      tag,
      regex(
        "NetIncome|ProfitLoss",
        ignore_case = TRUE
      )
    )
  ) %>%
  select(
    tag,
    version,
    custom,
    tlabel,
    doc
  )

cat("\n============================================\n")
cat("NET INCOME TAG CANDIDATES\n")
cat("============================================\n\n")

print(
  net_income_tags,
  n = 100
)

# ============================================================
# 13. Assets candidates
# ============================================================

asset_tags <- tag_data %>%
  filter(
    str_detect(
      tag,
      regex(
        "Assets",
        ignore_case = TRUE
      )
    )
  ) %>%
  select(
    tag,
    version,
    custom,
    tlabel,
    doc
  )

cat("\n============================================\n")
cat("ASSET TAG CANDIDATES\n")
cat("============================================\n\n")

print(
  asset_tags,
  n = 100
)

# ============================================================
# 14. Save candidate tags
# ============================================================

write_csv(
  candidate_tags,
  file.path(
    processed_path,
    "xbrl_candidate_tags.csv"
  )
)

write_csv(
  revenue_tags,
  file.path(
    processed_path,
    "xbrl_revenue_tags.csv"
  )
)

write_csv(
  net_income_tags,
  file.path(
    processed_path,
    "xbrl_net_income_tags.csv"
  )
)

write_csv(
  asset_tags,
  file.path(
    processed_path,
    "xbrl_asset_tags.csv"
  )
)

# ============================================================
# 15. Final message
# ============================================================

cat("\n============================================\n")
cat("PHASE 2C INSPECTION COMPLETED\n")
cat("============================================\n\n")

cat(
  "Candidate tags saved in:\n",
  processed_path,
  "\n"
)