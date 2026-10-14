
# ============================================================
# R-InsightRAG
# PHASE 4K: INTERACTIVE FINANCIAL ANALYTICS DASHBOARD
# Framework: Shiny
# Data source: SQLite financial_analytics table
# ============================================================

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. FIND THE SQLITE DATABASE
# ------------------------------------------------------------

find_database <- function() {
  
  candidates <- c(
    file.path(
      getwd(), "..", "data", "processed",
      "r_insightrag.db"
    ),
    file.path(
      getwd(), "data", "processed",
      "r_insightrag.db"
    ),
    file.path(
      getwd(), "..", "..", "data", "processed",
      "r_insightrag.db"
    )
  )
  
  existing <- candidates[file.exists(candidates)]
  
  if (length(existing) == 0) {
    stop(
      "Database not found. Expected r_insightrag.db ",
      "inside data/processed."
    )
  }
  
  normalizePath(existing[1], winslash = "/", mustWork = TRUE)
}

db_path <- find_database()

# ------------------------------------------------------------
# 2. LOAD DATA
# ------------------------------------------------------------

con <- dbConnect(SQLite(), db_path)

if (!("financial_analytics" %in% dbListTables(con))) {
  dbDisconnect(con)
  stop("The financial_analytics table was not found.")
}

financial_data <- dbReadTable(
  con,
  "financial_analytics"
)

dbDisconnect(con)

financial_data <- as_tibble(financial_data) %>%
  mutate(
    year = as.integer(year),
    revenue_growth_pct = revenue_growth * 100,
    operating_margin_pct = operating_margin * 100,
    net_profit_margin_pct = net_profit_margin * 100
  )

available_years <- sort(
  unique(financial_data$year[!is.na(financial_data$year)])
)

company_choices <- sort(
  unique(financial_data$company_name)
)

# ------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ------------------------------------------------------------

format_money <- function(x) {
  
  if (length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("N/A")
  }
  
  if (abs(x) >= 1e12) {
    return(paste0("$", sprintf("%.2f", x / 1e12), "T"))
  }
  
  if (abs(x) >= 1e9) {
    return(paste0("$", sprintf("%.2f", x / 1e9), "B"))
  }
  
  if (abs(x) >= 1e6) {
    return(paste0("$", sprintf("%.2f", x / 1e6), "M"))
  }
  
  if (abs(x) >= 1e3) {
    return(paste0("$", format(round(x), big.mark = ",")))
  }
  
  paste0("$", format(round(x, 2), big.mark = ","))
}

format_percent <- function(x) {
  
  if (length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("N/A")
  }
  
  paste0(sprintf("%.2f", x * 100), "%")
}

# ------------------------------------------------------------
# 4. USER INTERFACE
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel(
    "R-InsightRAG | Financial Intelligence Dashboard"
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fb;
        font-family: Arial, sans-serif;
      }

      .well {
        background-color: white;
        border-radius: 8px;
        border: 1px solid #e0e5ed;
      }

      .kpi-card {
        background: white;
        border: 1px solid #e0e5ed;
        border-radius: 10px;
        padding: 18px;
        margin-bottom: 14px;
        min-height: 105px;
      }

      .kpi-label {
        font-size: 13px;
        color: #667085;
        margin-bottom: 8px;
      }

      .kpi-value {
        font-size: 25px;
        font-weight: bold;
        color: #172b4d;
      }

      .section-title {
        font-weight: bold;
        color: #172b4d;
        margin-top: 8px;
      }
    "))
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      selectInput(
        "year",
        "Fiscal year",
        choices = c("All years", available_years),
        selected = "All years"
      ),
      
      selectizeInput(
        "company",
        "Company",
        choices = company_choices,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Search and select companies"
        )
      ),
      
      checkboxInput(
        "exclude_missing_revenue",
        "Exclude records with missing revenue",
        value = TRUE
      ),
      
      hr(),
      
      p(
        "Source: SEC financial statement facts ",
        "collected for the selected company cohort."
      ),
      
      p(
        "Amounts are in USD. Growth and margin values ",
        "are calculated from reported financial data."
      )
    ),
    
    mainPanel(
      
      h3("Financial Overview"),
      
      fluidRow(
        column(
          3,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Companies"),
            textOutput("kpi_companies")
          )
        ),
        
        column(
          3,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Reported Revenue"),
            textOutput("kpi_revenue")
          )
        ),
        
        column(
          3,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Median Revenue Growth"),
            textOutput("kpi_growth")
          )
        ),
        
        column(
          3,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Median Operating Margin"),
            textOutput("kpi_margin")
          )
        )
      ),
      
      tabsetPanel(
        
        tabPanel(
          "Revenue Trends",
          
          h4(class = "section-title", "Revenue by Fiscal Year"),
          
          plotOutput(
            "revenue_trend_plot",
            height = "350px"
          ),
          
          h4("Yearly Summary"),
          
          tableOutput("year_summary")
        ),
        
        tabPanel(
          "Company Comparison",
          
          h4(class = "section-title", "Top Companies by Revenue"),
          
          plotOutput(
            "top_companies_plot",
            height = "450px"
          ),
          
          h4("Company Financial Records"),
          
          tableOutput("company_table")
        ),
        
        tabPanel(
          "Profitability",
          
          h4(
            class = "section-title",
            "Operating Margin vs Net Profit Margin"
          ),
          
          plotOutput(
            "profitability_plot",
            height = "400px"
          ),
          
          h4("Profitability Summary"),
          
          tableOutput("profitability_table")
        ),
        
        tabPanel(
          "Liquidity and Cash Flow",
          
          h4(class = "section-title", "Current Ratio"),
          
          plotOutput(
            "liquidity_plot",
            height = "350px"
          ),
          
          h4("Cash Flow Comparison"),
          
          plotOutput(
            "cashflow_plot",
            height = "400px"
          )
        ),
        
        tabPanel(
          "Growth Screening",
          
          h4(class = "section-title", "Revenue Growth Categories"),
          
          plotOutput(
            "growth_category_plot",
            height = "400px"
          ),
          
          h4("Records Requiring Data Review"),
          
          tableOutput("quality_table")
        ),
        
        tabPanel(
          "Data Explorer",
          
          h4(class = "section-title", "Filtered Financial Data"),
          
          downloadButton(
            "download_data",
            "Download filtered CSV"
          ),
          
          br(),
          br(),
          
          tableOutput("data_table")
        )
      )
    )
  )
)

# ------------------------------------------------------------
# 5. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Filtered records
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- financial_data
    
    if (
      !is.null(input$year) &&
      input$year != "All years"
    ) {
      data <- data %>%
        filter(year == as.integer(input$year))
    }
    
    if (
      !is.null(input$company) &&
      length(input$company) > 0
    ) {
      data <- data %>%
        filter(company_name %in% input$company)
    }
    
    if (isTRUE(input$exclude_missing_revenue)) {
      data <- data %>%
        filter(!is.na(revenue))
    }
    
    data
  })
  
  # ----------------------------------------------------------
  # KPI cards
  # ----------------------------------------------------------
  
  output$kpi_companies <- renderText({
    
    n_distinct(
      filtered_data()$company_id
    )
  })
  
  output$kpi_revenue <- renderText({
    
    data <- filtered_data()
    
    if (nrow(data) == 0 || all(is.na(data$revenue))) {
      return("N/A")
    }
    
    format_money(
      sum(data$revenue, na.rm = TRUE)
    )
  })
  
  output$kpi_growth <- renderText({
    
    x <- filtered_data()$revenue_growth
    x <- x[is.finite(x)]
    
    if (length(x) == 0) {
      return("N/A")
    }
    
    format_percent(median(x))
  })
  
  output$kpi_margin <- renderText({
    
    x <- filtered_data()$operating_margin
    x <- x[is.finite(x)]
    
    if (length(x) == 0) {
      return("N/A")
    }
    
    format_percent(median(x))
  })
  
  # ----------------------------------------------------------
  # Revenue trend chart
  # ----------------------------------------------------------
  
  output$revenue_trend_plot <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(year) %>%
      summarise(
        revenue = if (all(is.na(revenue))) {
          NA_real_
        } else {
          sum(revenue, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      filter(!is.na(revenue))
    
    validate(
      need(nrow(data) > 0, "No revenue data for these filters.")
    )
    
    ggplot(data, aes(x = year, y = revenue)) +
      geom_line(linewidth = 1.1, na.rm = TRUE) +
      geom_point(size = 2.5) +
      scale_y_continuous(
        labels = function(x) {
          ifelse(
            abs(x) >= 1e12,
            paste0(round(x / 1e12, 2), "T"),
            ifelse(
              abs(x) >= 1e9,
              paste0(round(x / 1e9, 1), "B"),
              paste0(round(x / 1e6, 0), "M")
            )
          )
        }
      ) +
      scale_x_continuous(breaks = sort(unique(data$year))) +
      labs(
        x = "Fiscal year",
        y = "Reported revenue (USD)",
        title = "Aggregate Reported Revenue"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$year_summary <- renderTable({
    
    filtered_data() %>%
      group_by(year) %>%
      summarise(
        company_count = n_distinct(company_id),
        companies_with_revenue = sum(!is.na(revenue)),
        total_revenue = if (all(is.na(revenue))) {
          NA_real_
        } else {
          sum(revenue, na.rm = TRUE)
        },
        median_growth = if (all(is.na(revenue_growth))) {
          NA_real_
        } else {
          median(revenue_growth, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      arrange(year) %>%
      mutate(
        total_revenue = round(total_revenue, 2),
        median_growth = round(median_growth * 100, 2)
      )
  })
  
  # ----------------------------------------------------------
  # Top companies
  # ----------------------------------------------------------
  
  output$top_companies_plot <- renderPlot({
    
    data <- filtered_data() %>%
      filter(!is.na(revenue)) %>%
      group_by(company_name) %>%
      summarise(
        revenue = max(revenue, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      slice_max(
        order_by = revenue,
        n = 10,
        with_ties = FALSE
      ) %>%
      arrange(revenue)
    
    validate(
      need(nrow(data) > 0, "No company revenue data available.")
    )
    
    ggplot(
      data,
      aes(
        x = reorder(company_name, revenue),
        y = revenue
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(
        labels = function(x) {
          ifelse(
            abs(x) >= 1e12,
            paste0(round(x / 1e12, 2), "T"),
            paste0(round(x / 1e9, 1), "B")
          )
        }
      ) +
      labs(
        x = NULL,
        y = "Revenue (USD)",
        title = "Top 10 Companies in the Filtered Data"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$company_table <- renderTable({
    
    filtered_data() %>%
      select(
        company_name,
        year,
        revenue,
        revenue_growth
      ) %>%
      arrange(desc(year), desc(revenue)) %>%
      head(100) %>%
      mutate(
        revenue = round(revenue, 2),
        revenue_growth = round(revenue_growth * 100, 2)
      )
  })
  
  # ----------------------------------------------------------
  # Profitability
  # ----------------------------------------------------------
  
  output$profitability_plot <- renderPlot({
    
    data <- filtered_data() %>%
      filter(
        is.finite(operating_margin),
        is.finite(net_profit_margin)
      )
    
    validate(
      need(nrow(data) > 0, "No complete profitability metrics available.")
    )
    
    ggplot(
      data,
      aes(
        x = operating_margin * 100,
        y = net_profit_margin * 100
      )
    ) +
      geom_point(alpha = 0.65, na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(
        x = "Operating margin (%)",
        y = "Net profit margin (%)",
        title = "Profitability Relationship"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$profitability_table <- renderTable({
    
    filtered_data() %>%
      group_by(year) %>%
      summarise(
        records = n(),
        median_operating_margin = if (
          all(is.na(operating_margin))
        ) {
          NA_real_
        } else {
          median(operating_margin, na.rm = TRUE) * 100
        },
        median_net_profit_margin = if (
          all(is.na(net_profit_margin))
        ) {
          NA_real_
        } else {
          median(net_profit_margin, na.rm = TRUE) * 100
        },
        .groups = "drop"
      ) %>%
      arrange(year)
  })
  
  # ----------------------------------------------------------
  # Liquidity
  # ----------------------------------------------------------
  
  output$liquidity_plot <- renderPlot({
    
    data <- filtered_data() %>%
      filter(
        !is.na(current_ratio),
        current_ratio >= 0
      )
    
    validate(
      need(nrow(data) > 0, "No liquidity data available.")
    )
    
    ggplot(
      data,
      aes(
        x = factor(year),
        y = current_ratio
      )
    ) +
      geom_boxplot(outlier.alpha = 0.35) +
      labs(
        x = "Fiscal year",
        y = "Current ratio",
        title = "Current Ratio Distribution by Year"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ----------------------------------------------------------
  # Cash flow
  # ----------------------------------------------------------
  
  output$cashflow_plot <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(year) %>%
      summarise(
        operating = if (all(is.na(operating_cash_flow))) {
          NA_real_
        } else {
          sum(operating_cash_flow, na.rm = TRUE)
        },
        investing = if (all(is.na(investing_cash_flow))) {
          NA_real_
        } else {
          sum(investing_cash_flow, na.rm = TRUE)
        },
        financing = if (all(is.na(financing_cash_flow))) {
          NA_real_
        } else {
          sum(financing_cash_flow, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = c(operating, investing, financing),
        names_to = "cash_flow_type",
        values_to = "cash_flow"
      ) %>%
      filter(is.finite(cash_flow))
    
    validate(
      need(nrow(data) > 0, "No cash flow data available.")
    )
    
    ggplot(
      data,
      aes(
        x = factor(year),
        y = cash_flow,
        group = cash_flow_type,
        linetype = cash_flow_type
      )
    ) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      labs(
        x = "Fiscal year",
        y = "Cash flow (USD)",
        linetype = "Cash flow type",
        title = "Aggregate Cash Flow Trends"
      ) +
      theme_minimal(base_size = 13)
  })
  
  # ----------------------------------------------------------
  # Growth categories
  # ----------------------------------------------------------
  
  output$growth_category_plot <- renderPlot({
    
    data <- filtered_data() %>%
      mutate(
        growth_category = case_when(
          is.na(revenue_growth) ~ "Growth unavailable",
          revenue_growth >= 0.10 ~ "Growth >= 10%",
          revenue_growth >= 0 ~ "Growth 0% to <10%",
          TRUE ~ "Revenue decline"
        )
      ) %>%
      count(year, growth_category)
    
    validate(
      need(nrow(data) > 0, "No growth records available.")
    )
    
    ggplot(
      data,
      aes(
        x = factor(year),
        y = n,
        linetype = growth_category,
        group = growth_category
      )
    ) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      labs(
        x = "Fiscal year",
        y = "Company-year records",
        linetype = "Growth category",
        title = "Company Revenue Growth Screening"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # ----------------------------------------------------------
  # Data quality table
  # ----------------------------------------------------------
  
  output$quality_table <- renderTable({
    
    filtered_data() %>%
      mutate(
        quality_status = case_when(
          is.na(revenue) ~ "Missing revenue",
          revenue <= 0 ~ "Nonpositive revenue",
          revenue < 1000000 ~ "Small revenue (< $1M)",
          !is.na(revenue_growth) &
            (revenue_growth > 1 | revenue_growth < -0.5) ~
            "Extreme revenue growth",
          TRUE ~ "No basic revenue flag"
        )
      ) %>%
      count(quality_status, name = "records") %>%
      arrange(desc(records))
  })
  
  # ----------------------------------------------------------
  # Data explorer
  # ----------------------------------------------------------
  
  output$data_table <- renderTable({
    
    filtered_data() %>%
      select(
        company_name,
        year,
        revenue,
        revenue_growth,
        operating_margin,
        net_profit_margin,
        current_ratio,
        operating_cash_flow
      ) %>%
      arrange(desc(year), company_name) %>%
      head(500)
  })
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "r_insightrag_financial_data_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
}

# ------------------------------------------------------------
# 6. START APP
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
