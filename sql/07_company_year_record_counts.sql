SELECT year, COUNT(*) AS company_records,
       COUNT(revenue) AS records_with_revenue,
       COUNT(net_income) AS records_with_net_income
FROM financial_analytics
GROUP BY year
ORDER BY year;
