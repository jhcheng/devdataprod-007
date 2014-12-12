# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rCharts)
#library(zoo)
source(file='highstock/Highstock.R')
source(file='highstock/Highstock_OHLC.R')
source(file='highstock/Highstock_sPlot.R')

yahooURL <- "http://real-chart.finance.yahoo.com/table.csv"

shinyServer(function(input, output, session) {
   output$ui <- renderUI({
     if (is.null(input$ta) | input$ta == '')
       return()
     #print(input$ta)
     elemList = list()
     selectTA <- input$ta
     elemIdx <- 1
     formulaText <- NULL
     if (selectTA == 'SMA' | selectTA == 'WMA' || selectTA == 'EMA') {
       elemList[[elemIdx]] <- textInput("period", "Period:", "20")
       elemIdx <- elemIdx + 1 
       switch (selectTA,
               "SMA" = {
                 formulaText <- "$$SMA(n)_{M} = \\frac{P_{M} + P_{M-1} + ... + P_{M-(n-1)}}{n}$$"                 
               },
               "WMA" = {
                 formulaText <- "$$WMA(n)_{M} = \\frac{nP_{M} + (n-1))P_{M-1} + ... + 2P_{M-(n-2)} + P_{M-(n-1)}}{n + (n-1) + ... + 2 + 1)}$$"                 
               },
               "EMA" = {
                 formulaText <- "$$EMA_{today} = \\alpha * P_{today} + (1-\\alpha)*EMA_{yesterday}, \\alpha = 2/(n+1)$$"
               }
               )       
     } else if (selectTA == 'BOL') {
       elemList[[elemIdx]] <- textInput("period", "Period:", "20")
       elemIdx <- elemIdx + 1       
       elemList[[elemIdx]] <- textInput("std", "Standard Deviation:", "2")
       elemIdx <- elemIdx + 1 
       formulaText <- "$$Upper/Lower = SMA(n)_{M} \\pm  k * \\sigma_{M}$$"
     }
     elemList[[elemIdx]] <- actionButton("plotButton","Plot")     
     elemIdx <- elemIdx + 1 
     elemList[[elemIdx]] <- tags$p("Formula:")
     elemIdx <- elemIdx + 1 
     elemList[[elemIdx]] <- withMathJax(helpText(formulaText))
     elemList
   })
  
  output$pricePlot <- renderChart({    
    input$plotButton
    # get price data from Yahoo Finance
    data <- readData(input)
    str(data)
    # check technical analysis selection
    selectTA <- input$ta
    series <- list()
    isolate({
      # an ugly workaround to prevent error when first time selection
      if (!is.null(input$plotButton)) {
        if (input$plotButton >= 0) {
          if (selectTA == 'SMA') {
            sma <- MA(data, step = as.numeric(input$period))
            series[[1]] <- list(data=sma, type='line', 
                                name = paste('SMA(',input$period,')', sep=""), 
                                newY=F)
          } else if (selectTA == 'WMA') {
            wma <- MA(data, step = as.numeric(input$period), type="wma")
            series[[1]] <- list(data=wma, type='line', 
                                name = paste('WMA(',input$period,')', sep=""), 
                                newY=F)            
          } else if (selectTA == 'EMA') {
            alpha <- 2/(as.numeric(input$period) + 1)
            ema <- EMA(data, alpha)              
            series[[1]] <- list(data=ema, type='line', 
                                name = paste('EMA(',input$period,',', round(alpha, 4),')', sep=""), 
                                newY=F)              
          } else if (selectTA == 'BOL') {
            bol <- NULL
            std <- NULL
            if (is.null(input$std)) {              
              std <- 2
              bol <- BollingerBand(data, step = as.numeric(input$period), std = std)
            } else {
              std <- as.numeric(input$std)
              bol <- BollingerBand(data, step = as.numeric(input$period), std = std)  
            }
            series[[1]] <- list(data=bol[,c("date", "upper")], type='line', 
                                name = paste('Upper(+', input$std, ')', sep = ""), 
                                newY=F)            
            series[[2]] <- list(data=bol[,c("date", "ma")], type='line', 
                                name = paste('SMA(',input$period,')', sep=""), 
                                newY=F) 
            series[[3]] <- list(data=bol[,c("date", "lower")], type='line', 
                                name = paste('Lower(-', input$std, ')', sep = ""), 
                                newY=F) 
            series[[4]] <- list(data=bol[,c("date", "bv")], type='line', 
                                name = paste('BollingerValue', sep = ""), 
                                newY=T , band = c(-std, std)) 
          }
        }
      }
    })
    ohlcPlot(data = data, name=input$symbol, volume = input$v, series = series)
  })
  
})

buildURL <- function(url, parameters = list()) {  
  qs <- NULL
  for (n in names(parameters)) {
    qs <- paste(qs, n, "=", parameters[n], "&", sep = "")
  }
  paste(url, "?", qs, sep = "")
}

readData <- function(input) {
  # http://real-chart.finance.yahoo.com/table.csv?s=AAPL&a=11&b=12&c=1980&d=10&e=6&f=2014&g=d&ignore=.csv
  # parameters:
  # s:symbol
  # a:start month - 1
  # b:start day
  # c:start year
  # d:end month - 1
  # e:end day
  # f:end year
  # g:daily(d), weekly(w), monthly(m)
  start <- as.POSIXlt(input$daterange[1])
  end <- as.POSIXlt(input$daterange[2])
  startDate <- as.Date(input$daterange)    
  parameters <- list("s" = input$symbol,
                     "a" = start$mon,
                     "b" = start$mday,
                     "c" = start$year + 1900,
                     "d" = end$mon,
                     "e" = end$mday,
                     "f" = end$year + 1900,
                     "g" = input$g,
                     "ignore" = ".csv")
  url <- buildURL(yahooURL, parameters)
  #print(url)
  data <- read.csv(url, header = T, stringsAsFactors = F)
  # add a new column "date" as timestamp for disply in highcharts
  data$date <- as.numeric(as.POSIXct(data$Date, origin="1970-01-01")) * 1000
  # reverse data by row
  data[order(nrow(data):1),] 
}

MA <- function(data, step = 20, type = "sma", alpha = 0) {
  w <- sapply(1:step, function(x) {
    if (type == "wma") {
      step - x + 1
    } else {
      1
    }
  })
  ma <- data.frame(matrix(NA, nrow = dim(data)[1]-step+1, ncol = 2))
  colnames(ma) <- c("date", "Close")
  ma$date <- data[step:dim(data)[1],c("date")]
  ma$Close <- apply(data[step:dim(data)[1],], 1, function(x) {
    idx <- which(data$Date == x["Date"])
    start <- idx-step+1    
    round(sum(data[start:idx, c("Close")] * w)/sum(w) , 2)
  })
  ma
} 

EMA <- function(data, alpha = 1) {
  rows <- dim(data)[1]
  ema <- data.frame(matrix(NA, nrow = rows, ncol = 2))
  colnames(ema) <- c("date", "Close")
  ema$date <- data[,c("date")]
  for (i in 1:rows) {
    price <- data$Close[i]
    if (i == 1) {
      ema[i, c("Close")] <- price
    } else {
      ema[i, c("Close")] <- alpha * price + (1 - alpha) * ema$Close[i-1]
    }
  }
  ema
}

BollingerBand <- function(data, step = 20, std = 2) {
  rows <- dim(data)[1]
  bol <- data.frame(matrix(NA, nrow = rows - step+1, ncol = 5))
  colnames(bol) <- c("date", "upper", "lower", "ma", "bv")
  bol$date <- data[step:rows,c("date")]
  bol[,c("upper","lower", "ma", "bv")] <- t(sapply(1:(rows-step+1), function(x) {
     end <- x + step - 1
     sd_roll <- sd(data[x:end, c("Close")])
     ma_roll <- mean(data[x:end, c("Close")])
     price <- data$Close[x]
     c(round(ma_roll + std*sd_roll, 2), 
       round(ma_roll - std*sd_roll, 2), 
       round(ma_roll, 2),
       round((price - ma_roll)/sd_roll, 2)
       )
   }))
  #print(bol)
  bol
}