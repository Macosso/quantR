
#' Import trading data from yahoo finance
#'
#' @param tokens A vector of trading tokens to import
#' @param start The start date of the data series
#' @param end The end date of data series
#' @param variable The variable to be selected from ```adjusted.price, volume, close.price, low.price, high.price, open.price```, default: ```adjusted.price```
#'
#' @return An xts object
#' @export
#'
#' @examples tradingData <- loadTokens(c("MSFT", "GOOG", "GM"), start = "2020-01-01")

loadTokens <- function(tokens, start, end=NULL, variable = "adjusted.price"){

  if(is.null(end)) end <- Sys.Date()

  if(variable == "adjusted.price"){
    selectCol <- 6
  }else if(variable == "volume"){
    selectCol <- 5
  }else if(variable == "close.price"){
    selectCol <- 4
  }else if(variable == "low.price"){
    selectCol <- 3
  }else if(variable == "high.price"){
    selectCol <- 2
  }else if(variable == "open.price"){
    selectCol <- 1
  }

  dt = lapply(tokens, function(x) quantmod::getSymbols(x, auto.assign = F, from = start, to = end)[,6])
  dt <- Reduce(merge, dt)
  colnames(dt) <- tokens
  return(dt)
}



