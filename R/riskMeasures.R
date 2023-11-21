
VaR <- function(data, return.type = "log", interval = "d", alpha = 0.05, dist = "quantile"){
  if(return.type == "log"){
    return = na.omit(diff(log(data)))
  }

  miu <- sapply(return, mean)
  sigma <- sapply(return, sd)

  n = length(return[,1])

  if(dist=="quantile") VaRs <- sapply(return, quantile, alpha)
  else if(dist=="normal"){
    dist <- normalize(return)
    VaRs <- sapply(dist, quantile, alpha)
  }

  return(VaRs)
}



