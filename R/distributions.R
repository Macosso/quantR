library(xts)

normalize <- function(x){
  miu <- apply(x, 2, mean, na.rm = T)
  sigma <- apply(x, 2, sd, na.rm = T)

  normal_dist <- mapply(rnorm, n = nrow(x), mean = miu, sd = sigma)
  normal_dist <- xts(normal_dist, as.Date(index(x)))
  colnames(normal_dist) <- colnames(x)
  return(normal_dist)
}


studentize <- function(x){

  studentize.vector <- function(V){
    miu <- mean(V, na.rm = T)
    sigma <- sd(V, na.rm = T)
    return((V-miu)/sigma)
  }
  t_dist <- mapply(studentize.vector, as.list(as.data.frame(x)))
  t_dist <- xts(t_dist, as.Date(index(x)))
  olnames(t_dist) <- colnames(x)
  return(t_dist)
}
