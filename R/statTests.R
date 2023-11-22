#' Calculate skewness of a numeric vector.
#'
#' This function computes the skewness of a numeric vector using the specified
#' sample type for the calculation.
#'
#' @param x A numeric vector.
#' @param na.rm Logical, indicating whether missing values should be removed.
#'   Default is \code{FALSE}.
#' @param sample.type Character, specifying the type of skewness calculation.
#'   Options are \code{"biased"} (default) and \code{"mm"} (method of moments).
#'
#' @return The skewness of the input vector.
#'
#' @examples
#' # Calculate skewness with the default options
#' skewness(c(1, 2, 3, 4, 5))
#'
#' # Calculate skewness with missing values removed
#' skewness(c(1, 2, NA, 4, 5), na.rm = TRUE)
#'
#' # Calculate skewness using the method of moments
#' skewness(c(1, 2, 3, 4, 5), sample.type = "mm")
#'
#' @seealso \code{\link{mean}}
#'
#' @export
#'
#' @rdname skewness
skewness <- function(x, na.rm = FALSE, sample.type = "biased") {
  # Calculate the mean of the input vector
  if(na.rm) x = stats::na.omit(x)

  miu <- mean(x)

  # Compute the third central moment
  n <- length(x)
  m3 <- sum((x - miu)^3) / n

  # Calculate the third power of the standard deviation
  if (sample.type == "biased") {
    s3 <- (sum((x - miu)^2) / n)^(3/2)
  } else if (sample.type == "mm") {
    s3 <- (sum((x - miu)^2) / (n - 1))^(3/2)
  }

  # Return the skewness value
  return(m3 / s3)
}




#' Calculate kurtosis of a numeric vector.
#'
#' This function computes the kurtosis of a numeric vector using the specified
#' sample type for the calculation.
#'
#' @param x A numeric vector.
#' @param na.rm Logical, indicating whether missing values should be removed.
#'   Default is \code{FALSE}.
#' @param sample.type Character, specifying the type of kurtosis calculation.
#'   Options are \code{"biased"} (default) and \code{"unbiased"}.
#'
#' @return The kurtosis of the input vector.
#'
#' @examples
#' # Calculate kurtosis with the default options
#' kurtosis(c(1, 2, 3, 4, 5))
#'
#' # Calculate kurtosis with missing values removed
#' kurtosis(c(1, 2, NA, 4, 5), na.rm = TRUE)
#'
#' # Calculate kurtosis using unbiased estimation
#' kurtosis(c(1, 2, 3, 4, 5), sample.type = "unbiased")
#'
#' @seealso \code{\link{mean}}
#'
#' @export
#'
#' @rdname kurtosis
kurtosis <- function(x, na.rm = FALSE, sample.type = "biased") {

  if(na.rm) x = stats::na.omit(x)

  # Calculate the mean of the input vector
  miu <- mean(x)

  # Compute the fourth central moment
  n <- length(x)
  m4 <- sum((x - miu)^4) / n

  # Calculate the fourth power of the standard deviation
  if (sample.type == "biased") {
    s4 <- (sum((x - miu)^2) / n)^2
  } else if (sample.type == "unbiased") {
    s4 <- (sum((x - miu)^2) / (n - 1))^2
  }

  # Return the kurtosis value
  return(m4 / s4)
}


#' Perform the Jarque-Bera test for normality.
#'
#' This function calculates the Jarque-Bera test statistic and corresponding
#' p-value for assessing the normality of a numeric vector. It can handle
#' missing values if \code{na.rm} is set to \code{TRUE}.
#'
#' @param x A numeric vector.
#' @param na.rm Logical, indicating whether missing values should be removed.
#'   Default is \code{FALSE}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{statistic}{The Jarque-Bera test statistic.}
#'     \item{p.value}{The p-value for the test.}
#'   }
#'
#' @examples
#' # Perform Jarque-Bera test with the default options
#' jarqueBera(c(1, 2, 3, 4, 5))
#'
#' # Perform Jarque-Bera test with missing values removed
#' jarqueBera(c(1, 2, NA, 4, 5), na.rm = TRUE)
#'
#' @seealso \code{\link{skewness}}, \code{\link{kurtosis}}
#'
#' @export
#'
#' @rdname jarqueBera
jarqueBera <- function(x, na.rm = FALSE) {
  # Remove missing values if na.rm is TRUE
  if (na.rm) x <- stats::na.omit(x)

  # Calculate sample size, skewness, and kurtosis
  n <- length(x)
  S <- quantR::skewness(x)
  K <- quantR::kurtosis(x)

  # Calculate Jarque-Bera test statistic
  JB <- (n / 6) * (S^2 + (1 / 4) * (K - 3)^2)

  # Calculate p-value using chi-squared distribution
  p_value <- 1 - stats::pchisq(JB, df = 2)

  # Return a list with the Jarque-Bera test statistic and p-value
  return(list(statistic = JB, p.value = p_value))
}

