library(quantmod)
library(xts)
library(zoo)
library(R6)
library(stats)
require(stats)

#' @export
normalize <- function(x){
  miu <- apply(x, 2, mean, na.rm = T)
  sigma <- apply(x, 2, stats::sd, na.rm = T)

  normal_dist <- mapply(stats::rnorm, n = nrow(x), mean = miu, sd = sigma)
  normal_dist <- xts::xts(normal_dist, zoo::as.Date(zoo::index(x)))
  colnames(normal_dist) <- colnames(x)
  return(normal_dist)
}

#' @export
studentize <- function(x){

  studentize.vector <- function(V){
    miu <- mean(V, na.rm = T)
    sigma <- stats::sd(V, na.rm = T)
    return((V-miu)/sigma)
  }
  t_dist <- mapply(studentize.vector, as.list(as.data.frame(x)))
  t_dist <- xts::xts(t_dist, zoo::as.Date(zoo::index(x)))
  colnames(t_dist) <- colnames(x)
  return(t_dist)
}
