# R-InsightRAG

## Hybrid Financial Intelligence and Predictive Analytics System

R-InsightRAG combines SEC financial data analysis, XGBoost modelling, SQLite analytics, semantic retrieval, evidence-grounded question answering, and an interactive Shiny dashboard.

## Dataset

- 1,424 company-year observations across 178 companies.
- Financial reporting period: 2018-2025.
- Approximately 1,424 annual reports processed.
- 587,079 text chunks before deduplication; 554,160 unique chunks.
- Local sentence-transformer embeddings with 384 dimensions.

## Architecture

1. SEC financial data ingestion and preprocessing.
2. Feature engineering and SQLite storage.
3. XGBoost predictive modelling.
4. Annual-report text chunking and local embedding generation.
5. Semantic retrieval and evidence export.
6. Interactive Shiny financial dashboard.

## Model Evaluation

- Test ROC-AUC: approximately 0.706.
- F1-score: approximately 0.473 at threshold 0.38.
- Accuracy: approximately 0.601.
- Majority-class baseline accuracy: approximately 0.760.

These are exploratory results. Accuracy is below the reported majority-class baseline, so the model is not production-ready.

## Technology Stack

R, SQLite, DBI, RSQLite, XGBoost, dplyr, ggplot2, Shiny, and sentence-transformers using all-MiniLM-L6-v2.

## Run the Dashboard

From the project root, run:

```r
shiny::runApp('shiny', launch.browser = TRUE)
```

The dashboard uses data/processed/r_insightrag.db.

## Evidence-Grounded QA

The QA pipeline is in R/66_evidence_grounded_qa.R and uses retrieval functions from R/63_build_rag_retrieval.R. Outputs are saved in outputs/phase9c/ as timestamped text and CSV files.

The current pipeline retrieves and ranks evidence; do not describe it as fully generative LLM question answering unless generation is separately implemented. Review evidence for relevance and completeness.

## Project Structure

- R/: data processing, modelling, retrieval, and QA scripts.
- shiny/app.R: interactive dashboard.
- data/processed/: processed data and SQLite database.
- data/processed/retrieval_index/: retrieval metadata and embeddings.
- outputs/phase9c/: QA text and evidence exports.
- sql/: SQL analytics.
- evaluation/ and models/: evaluation and model artifacts, where present.

## Limitations

- Retrieval may return irrelevant passages and needs human evaluation.
- Model performance is exploratory and requires further validation.
- Financial information should be independently verified.
- This project is for analytics demonstration, not financial advice.

## Reproducibility

Reuse the existing processed database and retrieval index. Avoid rebuilding the large embedding index unless necessary. Install required R packages and configure the local Python environment before rerunning the retrieval pipeline.

## Disclaimer

This project is an educational and analytical prototype. It does not provide investment recommendations.
