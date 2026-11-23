library(shiny)
library(DT)
library(tidyverse)
library(grid)

# Run the existing AQI analysis once when the app starts
source("run_aqi_analysis.R", local = FALSE)

ui <- fluidPage(
  titlePanel("Smart City AQI Forecasting & Health Risk Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Dashboard Filters"),
      
      selectInput(
        "traffic",
        "Select Traffic Density:",
        choices = c("All", sort(unique(risk_df$traffic_density))),
        selected = "All"
      ),
      
      hr(),
      p("Explore AQI patterns, model forecasts and health-risk summaries.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "AQI Overview",
          h3("AQI Distribution and Categories"),
          plotOutput("aqi_plot", height = "450px"),
          h3("Correlation Matrix"),
          imageOutput("correlation_plot", height = "500px")
        ),
        
        tabPanel(
          "Traffic Analysis",
          h3("Traffic and Industrial Activity"),
          plotOutput("traffic_plot", height = "450px"),
          h3("Health Risk Summary"),
          tableOutput("risk_table")
        ),
        
        tabPanel(
          "AQI Forecast",
          h3("ARIMA vs Random Forest"),
          plotOutput("forecast_plot", height = "450px"),
          h3("Model Performance"),
          tableOutput("metrics_table")
        ),
        
        tabPanel(
          "Feature Importance",
          plotOutput("importance_plot", height = "450px")
        ),
        
        tabPanel(
          "Health Risk",
          h3("Health Risk Surface"),
          plotOutput("risk_plot", height = "500px")
        ),
        
        tabPanel(
          "Dataset",
          h3("Cleaned AQI Dataset"),
          DTOutput("data_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_risk <- reactive({
    if (input$traffic == "All") {
      risk_df
    } else {
      risk_df %>%
        filter(traffic_density == as.numeric(input$traffic))
    }
  })
  
  output$aqi_plot <- renderPlot({
    grid::grid.draw(p1_combined)
  })
  
  output$correlation_plot <- renderImage({
    list(
      src = normalizePath(
        "outputs/02_correlation_matrix.png"
      ),
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  output$traffic_plot <- renderPlot({
    grid::grid.draw(p3_combined)
  })
  
  output$risk_table <- renderTable({
    filtered_risk() %>%
      group_by(traffic_density) %>%
      summarise(
        Records = n(),
        `Mean AQI` = round(mean(AQI), 1),
        `Mean Health Risk` = round(mean(health_risk_score), 1),
        `High Risk (%)` =
          round(mean(risk_level == "High Risk") * 100, 1),
        .groups = "drop"
      )
  })
  
  output$forecast_plot <- renderPlot({
    print(p4_forecast)
  })
  
  output$metrics_table <- renderTable({
    metrics_tbl
  })
  
  output$importance_plot <- renderPlot({
    print(p5_imp)
  })
  
  output$risk_plot <- renderPlot({
    print(p6_heatmap)
  })
  
  output$data_table <- renderDT({
    datatable(
      clean_df,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)