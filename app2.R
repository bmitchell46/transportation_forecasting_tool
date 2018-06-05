library(shiny)
library(magrittr)
library(dplyr)
library(quantmod)
library(ggplot2)
library(dygraphs)
library(DT)
library(shinythemes)
library(lubridate)

codes <- read.csv("./data/FRED-datasets-codes_3.csv", header = TRUE)

#codes$name <- gsub(".*/","",codes$name)

round_two <- function(x){
  round(x, digits = 2)
}

ui <- navbarPage("FED FRED Forecaster",
                tabPanel("Forecaster", 
                 sidebarLayout(
                   sidebarPanel(
                     helpText("Select Series to Analyze.
                              Data is sources from St. Louis Federal Reserve FRED database using the R Quantmod Package."),
                     selectInput("select", "Select Series", unique(codes$description)),
                     selectInput("forecast", "Forecast Period (Months)", c("12", "24", "36"))
                     ),
                   mainPanel(h2(textOutput("intro")),
                             h4(textOutput("description")),
                             br(),
                             h2(textOutput("sum_title")),
                             verbatimTextOutput("summ"),
                             h2(textOutput("plot_title")),
                             dygraphOutput("plot", width = "95%", height = "400px"),
                             br(),
                             br(),
                             h2(textOutput("predict_title")),
                             DT::dataTableOutput("predict"),
                             br(),
                             br()))),

                 tabPanel("Data Dictionary" ,       
                   DT::dataTableOutput("fred_codes")) 
                 )            
                       
# server 
server <- function(input, output) {
  
  data <- reactive({
    desc <- input$select
    name <- codes %>% filter(description == desc) %>% select(name)
    getSymbols(name, src = "FRED", auto.assign = FALSE)
  })
  
  output$plot <- renderDygraph({
    desc <- input$select
    raw_name <- codes %>% filter(description == desc) %>% select(name)
    name <- as.character(raw_name$name)
    data <- getSymbols(name, src = "FRED", auto.assign = FALSE)
    start_raw <- codes %>% filter(description == desc) %>% select(start)
    start <- as.character(start_raw$start)
    end_raw <- codes %>% filter(description == desc) %>% select(end)
    end <- as.character(end_raw$end)
    start_date <- ymd(start)
    start_year <- year(start)
    start_year <- as.numeric(start_year)
    start_month <- month(start)
    start_month <- as.numeric(start_month)
    
    data <- data[paste(start,end, sep = "/")]
    data <- ts(data, start = c(start_year, start_month), frequency = 12)
    hw <- HoltWinters(data)
    p <- predict(hw, n.ahead = input$forecast, prediction.interval = TRUE)
    all <- cbind(data, p)
    dygraph(all, main = paste("FRED Series: ", input$select, " from ", start, " to ", end, sep = ""))%>%
      dySeries("data", label = "Actual") %>%
      dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")%>% dyRangeSelector()%>% dyRangeSelector(height = 20, strokeColor = "")
  })
  
  output$fred_codes <- DT::renderDataTable({
    DT::datatable(codes, filter = 'top')
  })
  
  output$predict <- DT::renderDataTable({
    desc <- input$select
    raw_name <- codes %>% filter(description == desc) %>% select(name)
    name <- as.character(raw_name$name)
    data <- getSymbols(name, src = "FRED", auto.assign = FALSE)
    start_raw <- codes %>% filter(description == desc) %>% select(start)
    start <- as.character(start_raw$start)
    end_raw <- codes %>% filter(description == desc) %>% select(end)
    end <- as.character(end_raw$end)
    start_date <- ymd(start)
    start_year <- year(start)
    start_year <- as.numeric(start_year)
    start_month <- month(start)
    start_month <- as.numeric(start_month)
    data <- data[paste(start,end, sep = "/")]
    data <- ts(data, start = c(start_year, start_month), frequency = 12)
    hw <- HoltWinters(data)
    p <- predict(hw, n.ahead = input$forecast, prediction.interval = TRUE)
    Date <- format(as.Date(time(p)), "%b-%Y")    
    p2 <- cbind(Date, as.data.frame(p))
    
    DT::datatable(p2, filter = 'top', rownames = FALSE, colnames=c("Date", "Predicted Value", "Upper Bound", "Lower Bound")) %>% formatRound(2:4, digits = 2)
  })
  
  output$summ <- renderPrint({
    desc <- input$select
    raw_name <- codes %>% filter(description == desc) %>% select(name)
    name <- as.character(raw_name$name)
    data <- getSymbols(name, src = "FRED", auto.assign = FALSE)
    start_raw <- codes %>% filter(description == desc) %>% select(start)
    start <- as.character(start_raw$start)
    end_raw <- codes %>% filter(description == desc) %>% select(end)
    end <- as.character(end_raw$end)
    start_date <- ymd(start)
    start_year <- year(start)
    start_year <- as.numeric(start_year)
    start_month <- month(start)
    start_month <- as.numeric(start_month)
    data <- data[paste(start,end, sep = "/")]
    data <- ts(data, start = c(start_year, start_month), frequency = 12)
    summary(data)
  }) 
  
  output$description <- renderText("This is a simple model that summarizes, plots, and forecasts select economic variables. The data was sourced from the St. Louis Federal Reserve FRED API using the R Quantmod package. The forecast was created using the Holt Winters model.") 
  output$predict_title <- renderText(paste("Forecasted Values of ", input$select, sep = ""))
  output$intro <- renderText("Model Description")
  output$plot_title <- renderText("Plotted Values")
  output$sum_title <- renderText(paste("Summary Statistics for ", input$select, sep = ""))
  

  
}


# Run the app
shinyApp(ui, server)



