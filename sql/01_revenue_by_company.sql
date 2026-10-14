SELECT company_id, company_name, year, revenue, revenue_growth
FROM financial_analytics
WHERE revenue IS NOT NULL
ORDER BY company_name, year;
