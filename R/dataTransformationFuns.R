#' Calculate logarithmic returns of a numeric vector.
#'
#' This function computes the logarithmic returns of a numeric vector by taking
#' the difference of the logarithm of each element. It can handle missing values
#' if \code{na.rm} is set to \code{TRUE}.
#'
#' @param x A numeric vector.
#' @param na.rm Logical, indicating whether missing values should be removed.
#'   Default is \code{FALSE}.
#'
#' @return A numeric vector of logarithmic returns.
#'
#' @examples
#' \dontrun{
#' # Calculate logarithmic returns with the default options
#' logReturns(c(1, 2, 3, 4, 5))
#'
#' # Calculate logarithmic returns with missing values removed
#' logReturns(c(1, 2, NA, 4, 5), na.rm = TRUE)
#'
#'}
#' @export
#'
#' @rdname logReturns
logReturns <- function(x, na.rm = FALSE) {
  # Remove missing values if na.rm is TRUE
  if (na.rm) {
    return(diff(log(stats::na.omit(x))))
  } else {
    return(diff(log(x)))
  }
}

#' Calculate percentage returns of a numeric vector.
#'
#' This function computes the percentage returns of a numeric vector by taking
#' the difference between each element and its lagged value, divided by the
#' lagged value. It can handle missing values if \code{na.rm} is set to
#' \code{TRUE}.
#'
#' @param x A numeric vector.
#' @param na.rm Logical, indicating whether missing values should be removed.
#'   Default is \code{FALSE}.
#'
#' @return A numeric vector of percentage returns.
#'
#' @examples
#' \dontrun{
#' # Calculate percentage returns with the default options
#' pctReturns(c(1, 2, 3, 4, 5))
#'
#' # Calculate percentage returns with missing values removed
#' pctReturns(c(1, 2, NA, 4, 5), na.rm = TRUE)
#'
#'}
#' @export
#'
#' @rdname pctReturns
pctReturns <- function(x, na.rm = FALSE) {
  # Remove missing values if na.rm is TRUE
  if (na.rm) {
    return((x / stats::lag(stats::na.omit(x))) - 1)
  } else {
    return((x / stats::lag(x)) - 1)
  }
}
