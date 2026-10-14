SELECT company_id, company_name, year,
       revenue, operating_income, net_income,
       operating_margin, net_profit_margin
FROM financial_analytics
WHERE revenue IS NOT NULL
ORDER BY company_name, year;
