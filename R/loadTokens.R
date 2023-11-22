# library(quantmod)
# library(xts)
# library(zoo)
# library(R6)
# library(stats)

#' @export
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



