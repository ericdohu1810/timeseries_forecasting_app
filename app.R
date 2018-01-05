#' This app demonstrates the use of various methods for forecasting time series 
#' The basic idea is to let the users control the validation windows and the horizon of forecast
#' and also other parameters of the forecaster
#' Particularly, this app will use the prophet open source package developed by Facebook
#' Other technical aspects include using brush events to generate forecasts,
#' using plotly to generate interactive visualizations and shinycssloaders for spinners
#' Author: Eric Do 

library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(plotly)
library(zoo)
library(dplyr)

# Sample Time Series 
air_passengers <- data.frame(
  date = zoo::as.Date(time(AirPassengers)),
  passengers_volume = as.numeric(AirPassengers)
)

ui_app <- dashboardPage(
  skin = 'red',
  dashboardHeader(
    title = "Time Series Forecaster", disable = TRUE
  ),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      column(
        3,
        fluidRow(
          box(
            title = tagList(icon = icon('line-chart'), "Trend Flexibility"),
            status = 'info', width = 12, solidHeader = TRUE,
            sliderInput('changepoint_scale', label = NULL, width = "100%", 
                        min = 0.01, max = 1.0, step = 0.01, value = 0.05),
            radioButtons('growth_type', label = NULL, width = "100%", inline = TRUE,
                         choices = c("Linear Growth", "Logistic Growth"),
                         selected = "Linear Growth")
          )
        ),
        fluidRow(
          box(
            title = tagList(icon = icon('snowflake-o'), "Seasonality Flexibility"),
            status = 'info', width = 12, solidHeader = TRUE,
            sliderInput('seasonality_scale', label = NULL, width = "100%", 
                        min = 1, max = 80, step = 1, value = 10),
            checkboxInput('yearly_seasonal', label = "Yearly Seasonality", width = "100%", value = TRUE)
          )
        ),
        fluidRow(
          box(
            title = tagList(icon = icon('calendar'), "Time Ranges"),
            status = 'info', width = 12, solidHeader = TRUE,
            fluidRow(
              column(
                6,
                dateInput('validate_from_date', label = "Validate From:", width = "100%",
                          value = "1957-01-01", min = "1951-01-01", max = "1960-01-01")
              ),
              column(
                6,
                dateInput('forecast_until_date', label =  "Forecast Until:", width = "100%",
                          value = "1970-12-01", min = "1961-06-01", max = "1999-12-01")
              )
            ),
            actionButton('prophesize', "Prophesize!", icon = icon("flash"), width = "100%")
          ))
      ),
      column(
        9,
        fluidRow(
          # Box to visualize results
          box(
            title = tagList(icon = icon('line-chart'), "Prophet Time Series Model"),
            status = 'success', width = 12, solidHeader = TRUE,
            withSpinner(plotlyOutput('time_series_viz', height = "385px"), type = 1, color = "blue")
          )
        ),
        
        fluidRow(
          # Value boxes to hold model information
          infoBoxOutput('training_error'),
          infoBoxOutput('testing_error'),
          infoBoxOutput('forecast_total')
        )
      )
    )
    
    
  )
)

server_app <- function(input, output, session) {
  
  # Build a model based on brush event 
  time_series_forecast <- reactive({
    return (list(
      ts_viz = plot_ly(air_passengers, x = ~date, y = ~passengers_volume, type = 'scatter', mode = 'lines'),
      train_error = "0%",
      test_error = "0%",
      forecast_total = "0"
    ))
  })
  
  # Visualize the time series first 
  output$time_series_viz <- renderPlotly({
    time_series_forecast()$ts_viz
  })
  
  # Render the value boxes 
  output$training_error <- renderInfoBox({
    infoBox(
      "Training Error", time_series_forecast()$train_error, icon = icon("cogs"), color = "red"
    )
  })
  output$testing_error <- renderInfoBox({
    infoBox(
      "Testing Error", time_series_forecast()$test_error, icon = icon("check"), color = "green"
    )
  })
  output$forecast_total <- renderInfoBox({
    infoBox(
      "Forecast Total", time_series_forecast()$forecast_total, icon = icon("signal"), color = "blue"
    )
  })
  
}

shinyApp(ui = ui_app, server = server_app)