##
## OHLC graph

seriesHeight <- 30

ohlcPlot <- highstockOHLC <- function(name='stock', data=data, type='candlestick', radius = 3, title = name, subtitle = NULL,  series = list(), volume = FALSE)
{
  rChart <- Highstock$new()
  
  nrows <- nrow(data)
  data  <- na.omit(data) # remove remaining observations with NA's
  
  if (nrows != nrow(data))
    warning("Observations with NA has been removed")  
    
  rChart$series(
    data = toJSONArray2(data[c('date', 'Open', 'High', 'Low', 'Close')], json = F, names = F),
    type = type,
    name = name,
    marker = list(radius = radius))
  
  yAxisList <- list() 
  yAxisList[[1]] <- list(title='OHLC')
  yIdx = length(yAxisList)
  if (length(series) > 0) {
    for (s in series) { 
      # This feature still has problems....
      if (s$newY) {
        yIdx = yIdx + 1
        yAxisList[[yIdx]] <- list(title=s$name, height=seriesHeight, replace=F)
        if (!is.null(s$band)) {
          yAxisList[[yIdx]]$plotBands <- list(color = "#FCFFC5", from = s$band[1], to = s$band[2])
        }
      }
      rChart$series(
        data = toJSONArray2(s$data, json = F, names = F),
        type = s$type,
        name = s$name,
        yAxis = ifelse(s$newY, yIdx-1, 0),
        marker = list(radius = radius)
      )
    }
  }
  if (volume) {
    yAxisList[[yIdx+1]] <- list(title='Volume', height=seriesHeight, replace=F)
    rChart$series(
      data = toJSONArray2(data[c('date', 'Volume')], json = F, names = F, replace = F),
      type = 'column',
      name = 'Volume',
      yAxis = yIdx,
      marker = list(radius = radius))
  } 
  ylength <- length(yAxisList)
  # calculate OHLC height in %
  ohlcHeight = 250-(ylength)*seriesHeight
  yAxisList[[1]]$height <- ohlcHeight
  # calculate top in %
  if (ylength>= 2) {
    for (yidx in 2:ylength) {
      top <- 300 - (ylength - yidx + 1)*seriesHeight
      yAxisList[[yidx]]$top <- top
    }    
  }
  rChart$yAxis(yAxisList)
    
  rChart$legend(enabled = F)
  
  ## title
  rChart$title(text = title, replace = T)
  
  ## subtitle
  rChart$subtitle(text = subtitle, replace = T)
  ## format number to 2 digits
  rChart$tooltip(pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:, .2f}</b><br/>', replace = T)
  
  return(rChart$copy())
}