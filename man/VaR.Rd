
\name{VaR}
\alias{VaR}
\docType{package}
\title{
Compute Value at Risk
}
\description{
Given an xts object, regenerate columns following a normal distribution based on each columns mean and standards deviation
}
\usage{
VaR(data, return.type = "log", interval = "d", alpha = 0.05, dist = "quantile")
}
\arguments{
\item{data}{an xts object containing prices or returns}
\item{return.type}{how should returns be transformed}
\item{interval}{data frequency, ```c(daily, monthly, quarterly, yearly)```}
\item{alpha}{confidence level}
\item{dist}{distribution}
}

\value{named matrix}
\examples{
quantrDate <- loadTokens(c("MSFT", "GOOG", "GM"), start = "2020-01-01")
VaR(quantrDate, return.type = "log")
}
\author{Joao Claudio Macosso}

