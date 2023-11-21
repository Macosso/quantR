library(quantmod)
library(xts)
library(zoo)
library(R6)
library(stats)

#' @export
VaR <- function(data, return.type = "log", interval = "d", alpha = 0.05, dist = "quantile"){
  if(return.type == "log"){
    return = stats::na.omit(diff(log(data)))
  }

  miu <- sapply(return, mean)
  sigma <- sapply(return, stats::sd)

  n = length(return[,1])

  if(dist=="quantile") VaRs <- sapply(return, stats::quantile, alpha)
  else if(dist=="normal"){
    dist <- normalize(return)
    VaRs <- sapply(dist, stats::quantile, alpha)
  }

  return(VaRs)
}



