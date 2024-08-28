#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(dplyr)
library(ggplot2)
library(gtrendsR)
library(prophet)
library(lubridate)


# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Google Trends Forecasting App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("keywords", "Enter Keywords (comma-separated):", "laptop,desktop,tablet"),
      selectInput("geo", "Select Region:", choices = c("GB", "US","MX","Worldwide"), selected = "GB"),
      sliderInput("forecast_period", "Forecast Period (Days):", min = 1, max = 730, value = 180),
      actionButton("update", "Update Forecast")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trends Plot", plotOutput("trendsPlot")),
        tabPanel("Forecast Plot", plotOutput("forecastPlot")),
        tabPanel("Model Components", plotOutput("componentsPlot"))
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive data retrieval based on user inputs
  trends_data <- eventReactive(input$update, {
    keywords <- unlist(strsplit(input$keywords, ","))
    if (length(keywords) == 1) {
      keywords <- c(keywords, keywords)  # Asegura que siempre haya al menos dos keywords para evitar problemas
    }
    tryCatch({
      curl::handle_setopt(curl::new_handle(), http_version = 1)
      gtrends(
        keyword = keywords,
        geo = input$geo,
        time = "today+5-y",
        tz = 0
      )$interest_over_time %>%
        mutate(date = ymd(date)) %>%
        filter(date < Sys.Date() - 1)
    }, error = function(e) {
      showNotification("Error retrieving data from Google Trends. Please try again later.", type = "error")
      NULL
    })
  })
  
  # Reactive forecasting model
  forecast_data <- eventReactive(input$update, {
    req(trends_data())
    laptop_data <- trends_data() %>%
      filter(keyword == unlist(strsplit(input$keywords, ","))[1]) %>%
      select(date, hits) %>%
      rename(ds = date, y = hits) %>%
      arrange(ds)
    
    m <- prophet(laptop_data)
    future <- make_future_dataframe(m, periods = input$forecast_period, freq = "day", include_history = TRUE)
    predict(m, future)
  })
  
  # Plot trends data
  
  output$trendsPlot <- renderPlot({
    req(trends_data())
    trends_data() %>%
      ggplot() +
      geom_line(aes(date, hits, color = keyword), size = 0.5) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_color_brewer(palette = 'Set1') +
      theme_minimal() +
      labs(x = NULL, y = "Relative Search Interest",
           title = 'Google Trends: interest over time',
           caption = "Data from Google Trends")
  })
# Plot forecast data
output$forecastPlot <- renderPlot({
  req(forecast_data())
  
  ggplot(forecast_data()) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper, fill = "Forecasted"), 
                alpha = 0.3) +
    geom_line(aes(x = ds, y = yhat, color = "Actual"), size = 1) +
    theme_bw() +
    labs(x = NULL, y = "Relative Search Interest",
         title = "Forecasting with Prophet") +
    scale_fill_manual(name = "Legend", values = c("Forecasted" = "green","Actual" = "black")) +
    scale_color_manual(name = " ", values = c("Actual" = "black")) +
    annotate("text", x = max(forecast_data()$ds), y = min(forecast_data()$yhat_lower), 
             label = "www.abelhga.com", 
             hjust = 1, vjust = -0.5, size = 4, colour = "black") +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(order = 2))
})
  
  
  # Plot forecast components
  output$componentsPlot <- renderPlot({
    req(forecast_data())
    prophet_plot_components(prophet(trends_data()), forecast_data())
  })
}

(r_laptop_m , r_laptop_ftdata , uncertainty = TRUE)

# Run the application 
shinyApp(ui = ui, server = server)
