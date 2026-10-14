SELECT company_id, company_name, year,
       revenue, revenue_growth
FROM financial_analytics
WHERE revenue_growth >= 0.10
ORDER BY revenue_growth DESC;
