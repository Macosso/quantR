#' Calculate Value at Risk (VaR) for a set of returns.
#'
#' This function computes VaR using either quantiles or a normal distribution
#' assumption. It can handle missing values and supports different confidence
#' levels and distribution assumptions.
#'
#' @param returns Numeric matrix or data frame of returns. Rows represent
#' observations, and columns represent different assets or portfolios.
#' @param alpha Confidence level for VaR, typically between 0 and 1. Default is 0.05.
#' @param dist Distribution assumption for VaR calculation. Options are
#' "quantile" (default) for quantile-based VaR or "normal" for normal distribution
#' assumption.
#'
#' @return A numeric vector or matrix of VaR values corresponding to each column
#' in the input returns.
#' @examples
#' \dontrun{
#' library(xts)
#' # Assuming xts objects
#' returns_xts <- xts(matrix(c(0.01, 0.03, -0.01, 0.04, -0.06, -0.02, 0.03,
#'  -0.01, 0.02, -0.03, 0.03, -0.006, 0.02, -0.03), ncol = 2),
#'  order.by = as.Date(1:7))
#'
#' # Calculate quantile-based VaR
#' VaR(returns_xts, alpha = 0.05, dist = "quantile")
#'
#' # Calculate VaR assuming normal distribution
#' VaR(returns_xts, alpha = 0.05, dist = "normal")
#' }
#'
#' @export
#'
#' @rdname VaR
VaR <- function(returns, alpha = 0.05, dist = "quantile") {
  returns <- stats::na.omit(returns)

  miu <- sapply(returns, mean)
  sigma <- sapply(returns, stats::sd)

  n <- nrow(returns)

  if (dist == "quantile") {
    VaRs <- sapply(returns, stats::quantile, alpha)
  } else if (dist == "normal") {
    VaRs <- miu + stats::qnorm(alpha) * sigma
  }

  return(VaRs)
}

#' Calculate Conditional Value at Risk (CVaR) for a set of returns.
#'
#' This function computes CVaR using either quantiles or a normal distribution
#' assumption. It can handle missing values and supports different confidence
#' levels and distribution assumptions.
#'
#' @param returns Numeric matrix or data frame of returns. Rows represent
#' observations, and columns represent different assets or portfolios.
#' @param alpha Confidence level for CVaR, typically between 0 and 1. Default is 0.05.
#' @param dist Distribution assumption for CVaR calculation. Options are
#' "quantile" (default) for quantile-based CVaR or "normal" for normal distribution
#' assumption.
#'
#' @return A numeric vector or matrix of CVaR values corresponding to each column
#' in the input returns.
#'
#' @examples
#' \dontrun{
#' library(xts)
#' # Assuming xts objects
#' returns_xts <- xts(matrix(c(0.01, 0.03, -0.01, 0.04, -0.06, -0.02, 0.03,
#'  -0.01, 0.02, -0.03, 0.03, -0.006, 0.02, -0.03), ncol = 2),
#'  order.by = as.Date(1:7))
#'
#' # Calculate quantile-based CVaR
#' CVaR(returns_xts, alpha = 0.05, dist = "quantile")
#'
#' # Calculate CVaR assuming normal distribution
#' CVaR(returns_xts, alpha = 0.05, dist = "normal")
#' }
#'
#' @export
#'
#' @rdname CVaR
CVaR <- function(returns, alpha = 0.05, dist = "quantile") {
  vars <- quantR::VaR(returns, alpha = alpha, dist = dist)
  Xs <- as.list(as.data.frame(returns))
  cvars <- mapply(function(x, y) mean(x[x <= y], na.rm = TRUE), Xs, vars)
  return(cvars)
}
