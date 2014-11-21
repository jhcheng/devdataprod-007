# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
require(rCharts)

# load S&P 500 symbols
sp500 <- read.csv("constituents.csv", stringsAsFactors=F, header=T)
choices <- c("SPY", sp500[,1])
names(choices) <- c("S&P 500", paste(sp500[,2],"(",sp500[,1], ")"))

tas <- c("Simple Moving Average" = "SMA",
         "Weighted Moving Average" = "WMA",
         "Exponential Moving Average" = "EMA",
         "Bollinger Bands" = "BOL")

shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
      .row-fluid .span4 {
          width: 15%;
      }
      .row-fluid .span8 {
        width: 80%;
      }
      .tab-content {
      }
    "))
  ),  
  # Application title
  titlePanel("S&P 500 Stock Analysis"),
  # Sidebar with a slider input for number of bins
  navlistPanel("",
               tabPanel("CHART",
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput("symbol", "Symbol:", choices=choices, width='100%'),
                            dateRangeInput("daterange", "Date range:", start = Sys.Date()-365, end = Sys.Date()),
                            radioButtons("g", "",
                                         c("Daily" = "d",
                                           "Weekly" = "w",
                                           "Monthly" = "m")),
                            checkboxInput("v", "Show Volume", FALSE)
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            tags$head(tags$style(".selectize-input {overflow: visible;}")),
                            splitLayout(
                              cellWidths = c("40%", "60%"),
                              cellArgs = list(style = "overflow:visible"),
                              selectizeInput("ta", "Technical Analysis", 
                                             choices = tas,
                                             options = list(
                                               placeholder = 'Please select ......',
                                               onInitialize = I('function() { this.setValue(""); }')                       
                                             )),
                              wellPanel(
                                withMathJax(),
                                uiOutput("ui"))
                            ),
                            showOutput("pricePlot", "highstock")
                          )                          
                        )),
               tabPanel("DOCUMENT",
                        includeHTML("www/help.html")
               )
               ) 
))