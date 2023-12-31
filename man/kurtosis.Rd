% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statTests.R
\name{kurtosis}
\alias{kurtosis}
\title{Calculate kurtosis of a numeric vector.}
\usage{
kurtosis(x, na.rm = FALSE, sample.type = "biased")
}
\arguments{
\item{x}{A numeric vector.}

\item{na.rm}{Logical, indicating whether missing values should be removed.
Default is \code{FALSE}.}

\item{sample.type}{Character, specifying the type of kurtosis calculation.
Options are \code{"biased"} (default) and \code{"unbiased"}.}
}
\value{
The kurtosis of the input vector.
}
\description{
This function computes the kurtosis of a numeric vector using the specified
sample type for the calculation.
}
\examples{
# Calculate kurtosis with the default options
kurtosis(c(1, 2, 3, 4, 5))

# Calculate kurtosis with missing values removed
kurtosis(c(1, 2, NA, 4, 5), na.rm = TRUE)

# Calculate kurtosis using unbiased estimation
kurtosis(c(1, 2, 3, 4, 5), sample.type = "unbiased")

}
\seealso{
\code{\link{mean}}
}
