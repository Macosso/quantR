# library(quantmod)
# library(xts)
# library(zoo)
# library(R6)
# library(stats)

#' @export
VaR <- function(returns, alpha = 0.05, dist = "quantile"){
  returns = stats::na.omit(returns)

  miu <- sapply(returns, mean)
  sigma <- sapply(returns, stats::sd)

  n = length(returns[,1])

  if(dist=="quantile") VaRs <- sapply(returns, stats::quantile, alpha)
  else if(dist=="normal"){
    VaRs <-  miu + qnorm(alpha)*sigma
  }

  return(VaRs)
}


CVaR <- function(returns, alpha = 0.05, dist = "quantile"){
  vars <- quantR::VaR(returns, alpha = alpha, dist = dist)
  Xs <- as.list(as.data.frame(returns))
  cvars <- mapply(function(x,y) mean(x[x<=y], na.rm = T), Xs, vars )
  return(cvars)
}

