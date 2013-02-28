my.xts <- function(dataframe) {
  if (require(xts) == FALSE)
    stop('Error: The package xts could not be loaded')
  df <- dataframe
  df$Date <- paste(df$Year, df$Month, sep = "-")
  df$Date <- as.yearmon(df$Date)
  df.x <- as.xts(x=df[[5]], order.by=df$Date)
  attr(x=df.x, which='LongName') <- colnames(df)[[5]]
  return(df.x)
}
# Examples:
if(FALSE){
  temp <- IDCJAC0004.061078.Data1
  temp <- my.xts(temp)
}

RecodeColnames <- function(dataframe){
  x <- colnames(dataframe) # get colnames
  x <- sub(pattern="IDCJAC0001[[:punct:]][[:digit:]]{6}[[:punct:]]Data1", replacement = 'prec', x = x)
  x <- sub(pattern="IDCJAC0002[[:punct:]][[:digit:]]{6}[[:punct:]]Data1", replacement = 'tempMax', x = x)
  x <- sub(pattern="IDCJAC0004[[:punct:]][[:digit:]]{6}[[:punct:]]Data1", replacement = 'tempMin', x = x)
  colnames(dataframe) <- x # apply edited column names
  return(dataframe)
}
#Examples:
if (FALSE) tom.clim <- RecodeColnames(tom.clim) # usage example