##
## OHLC graph
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
  
  if (volume) {
    rChart$yAxis(title='OHLC', height=180, linewidth=2)
    rChart$yAxis(title='Volume', top = 250, height=50, offset=0, linewidth=2, replace=F)
    rChart$series(
      data = toJSONArray2(data[c('date', 'Volume')], json = F, names = F, replace = F),
      type = 'column',
      name = 'Volume',
      yAxis =1,
      marker = list(radius = radius))    
  }
    
  if (length(series) > 0) {
    for (s in series) {
      yAxis = 0
      # This feature still has problems....
      if (s$newColumn) {
        rChart$yAxis(title='OHLC', height=180, linewidth=2)
        rChart$yAxis(title=s$name, top = 250, height=50, offset=0, linewidth=2, replace=F)
          if (volume) {
            yAxis = 2
          } else {
            yAxis = 1
          }
      }
      rChart$series(
        data = toJSONArray2(s$data, json = F, names = F),
        type = s$type,
        name = s$name,
        yAxis = yAxis,
        marker = list(radius = radius)
      )
    }
  }
  
  
  rChart$legend(enabled = F)
  
  ## title
  rChart$title(text = title, replace = T)
  
  ## subtitle
  rChart$subtitle(text = subtitle, replace = T)
  ## format number to 2 digits
  rChart$tooltip(pointFormat = '<span style="color:{series.color}">\u25CF</span> {series.name}: <b>{point.y:, .2f}</b><br/>', replace = T)
  
  return(rChart$copy())
}