SELECT company_id, company_name, year,
       current_assets, current_liabilities, current_ratio
FROM financial_analytics
WHERE current_ratio IS NOT NULL
ORDER BY current_ratio DESC;
