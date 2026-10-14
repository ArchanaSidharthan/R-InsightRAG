SELECT company_id, company_name, year, revenue
FROM financial_analytics
WHERE year = (SELECT MAX(year) FROM financial_analytics)
  AND revenue IS NOT NULL
ORDER BY revenue DESC
LIMIT 20;
