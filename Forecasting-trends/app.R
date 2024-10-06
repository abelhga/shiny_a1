#
# This is a Shiny web application made by Abel Hernández García // hi@abelhga.com // www.abelhga.com

library(shiny)
library(dplyr)
library(ggplot2)
library(gtrendsR)
library(prophet)
library(lubridate)
library(countrycode)
library(shinycssloaders) #to give visual feedback when data is being loaded.
library(plotly)


library(igraph)
library(visNetwork)
library(tidyr)
library(stringr)



#--------
# Getting a list of country names and corresponding ISO 3166-1 alpha-2 codes
  countries <- countrycode::codelist %>%
    filter(!is.na(iso2c)) %>%  # Filter out entries without a country code
    select(iso2c, country.name.en)  # Select ISO 2-letter code and English country name

# Convert to a named vector for use in selectInput
country_choices <- setNames(countries$iso2c, countries$country.name.en)

# Vector for user-friendly time labels
time_choices <- c(
  "Last 1 Hour" = "now 1-H",
  "Last 4 Hours" = "now 4-H",
  "Last 1 Day" = "now 1-d",
  "Last 7 Days" = "now 7-d",
  "Last 1 Month" = "today 1-m",
  "Last 3 Months" = "today 3-m",
  "Last 12 Months" = "today 12-m",
  "Last 5 Years" = "today+5-y",
  "Since 2004" = "all"
)
#--------

#UI
# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Search Trends Forecasting App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("keywords", "Enter Keywords (comma-separated). Only first word will be used for forecasting:", "Lavender, Daffodil, Bluebell"),
      selectInput("geo", "Select Region:", choices = c(country_choices,"Worldwide"), selected = "GB"),
      selectInput("time", "Select the Time Span:", choices = time_choices, selected = "today+5-y"), #we add the list for different periods of time
      # Add help text below the time span selection
      helpText("If you want to identify seasonality of the day of the week, select a time span maximum of 3 months."),
      sliderInput("forecast_period", "Forecast Period (Days):", min = 1, max = 730, value = 180),
      actionButton("update", "Update")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trends Plot", withSpinner(plotlyOutput("trendsPlot"))),
        tabPanel(
          "Forecast and Components",
          withSpinner(plotlyOutput("forecastPlot", height = "400px")),"SEASONALITY",
          withSpinner(plotOutput("componentsPlot", height = "400px")),
          "This model is optimized for forecasting time series with seasonal patterns."
        ),
        tabPanel("Anomaly Detection", withSpinner(plotOutput("anomalyDetection")))
      ),
      textOutput("trendText"),
      textOutput("summaryStats")
    )
  )
)

#SERVER
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
        time = input$time,
        tz = 0
      )$interest_over_time %>%
        mutate(date = ymd(date)) %>%
        filter(date < Sys.Date() - 1)
    }, error = function(e) {
      showNotification("Error retrieving data from Search Trends. Please try again later.", type = "error")
      NULL
    })
  })
  
  #----------------
  # Reactive Prophet model
  prophet_model <- reactive({
    req(trends_data())
    
    # Defining trend_df inside of forecast_data
    trend_df <- trends_data() %>%
      filter(keyword == unlist(strsplit(input$keywords, ","))[1]) %>%
      select(date, hits) %>%
      rename(ds = date, y = hits) %>%
      arrange(ds)
    
    m <- prophet(trend_df)
    m
  })
  
  # Reactive forecast data
  forecast_data <- reactive({
    req(prophet_model())
    
    #Defining trend_df
    trend_df <- trends_data() %>%
      filter(keyword == unlist(strsplit(input$keywords, ","))[1]) %>%
      select(date, hits) %>%
      rename(ds = date, y = hits) %>%
      arrange(ds)
    
    future <- make_future_dataframe(prophet_model(), periods = input$forecast_period, freq = "day", include_history = TRUE)
    forecast <- predict(prophet_model(), future)

  
   # Combine actual and forecast data
    combined_data <- forecast %>%
      mutate(ds = ymd(ds),
             segment = case_when(ds > Sys.Date() - 1 ~ 'forecast', TRUE ~ 'actual')) %>%
      select(ds, segment, yhat_lower, yhat, yhat_upper) %>%
      left_join(trend_df, by = c("ds" = "ds"))
    
    
    # Calculating the linear slope
    trend_lm <- lm(y ~ ds, data = trend_df)
    slope <- coef(trend_lm)["ds"]
    
    attr(combined_data, "trend_slope") <- slope #Saving the slop as an attribute
    
    
    combined_data
 
  })
  
  
  
  
  #----------------
  # Plot trends data
  
  output$trendsPlot <- renderPlotly({
    req(trends_data())
    
    trends_data() %>%
      ggplot() +
      geom_line(aes(date, hits, color = keyword), size = 0.5) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_color_brewer(palette = 'Set1') +
      theme_minimal() +
      labs(x = NULL, y = "Relative Search Interest",
           title = 'Search Trends: interest over time',
           caption = "Data from Google Trends")
  })
  
  # Plot forecast data
  output$forecastPlot <- renderPlotly({
    req(forecast_data())
    
    forecast_data() %>%
      ggplot() +
      geom_line(aes(x = ds, y = y, color = "Actual"), size = 0.5) + # Line for actual data
      geom_line(data = subset(forecast_data(), segment == "forecast"), 
                aes(x = ds, y = yhat, color = "Forecast"), size = 0.5) + # Line for forecast data
      geom_ribbon(data = subset(forecast_data(), segment == "forecast"), 
                  aes(x = ds, ymin = yhat_lower, ymax = yhat_upper, fill = "Forecasted"), 
                  alpha = 0.3) +
      theme_bw() +
      labs(x = NULL, y = "Relative Search Interest",
           title = "Time Series Decomposition and Prediction using Additive Models",
           color = "Legend",
           fill = "Legend") +
      scale_fill_manual(values = c("Forecasted" = "green")) +
      scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
      annotate("text", x = max(forecast_data()$ds) - 100, y = min(forecast_data()$yhat_lower) + 5, 
               label = "www.abelhga.com", 
               hjust = 1, vjust = -0.5, size = 3.5, colour = "black") +  # Adjusted size and position
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 2)) +
      theme(plot.margin = unit(c(1, 2, 1, 1), "cm"))  # Add right margin space
  })
  
  
  
  # Plot forecast components
  output$componentsPlot <- renderPlot({
    req(prophet_model())
    future <- make_future_dataframe(prophet_model(), periods = input$forecast_period, freq = "day", include_history = TRUE)
    forecast <- predict(prophet_model(), future)
    prophet_plot_components(prophet_model(), forecast, plot_cap = TRUE, uncertainty = TRUE)
  })
  
  
  #Plot anomalies
  output$anomalyDetection <- renderPlot({
    req(trends_data())
    trend_df <- trends_data() %>%
      filter(keyword == unlist(strsplit(input$keywords, ","))[1]) %>%
      select(date, hits) %>%
      mutate(change = c(NA, diff(hits)))  # Calculate daily changes
    
    ggplot(trend_df, aes(x = date)) +
      geom_line(aes(y = hits), color = "black", size = 0.5) +
      geom_point(data = trend_df[abs(trend_df$change) > 20, ], 
                 aes(y = hits), color = "red", size = 2) +  # Highlight significant changes
      labs(title = "Significant Changes in Trends", y = "Search Interest", x = "Date") +
      theme_minimal()
  })
  
  
  #Include a summary
  output$summaryStats <- renderText({
    req(trends_data())
    trend_df <- trends_data() %>%
      filter(keyword == unlist(strsplit(input$keywords, ","))[1]) %>%
      select(date, hits)
    
    mean_hits <- mean(trend_df$hits)
    median_hits <- median(trend_df$hits)
    max_hits <- max(trend_df$hits)
    min_hits <- min(trend_df$hits)
    
    paste(
      "Mean: ", round(mean_hits, 2), 
      " | Median: ", round(median_hits, 2),
      " | Max: ", max_hits, 
      " | Min: ", min_hits
    )
  })
  
# Mostrar si la tendencia es positiva o negativa
  output$trendText <- renderText({
    req(forecast_data())
    
    slope <- attr(forecast_data(), "trend_slope")
    
    if (slope > 0) {
      "The historical trend for the first keyword is positive."
    } else if (slope < 0) {
      "The historical trend for the first keyword is negative."
    } else {
      "The historical trend for the first keyword is neutral."
    }
  })
  
  
  
}



#RUN
# Run the application 
shinyApp(ui = ui, server = server)

