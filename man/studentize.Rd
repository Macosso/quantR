
\name{studentize}
\alias{studentize}
\docType{package}
\title{
Studentise columns of a xts like object
}
\description{
Given an xts object x, regenerate columns of x following a student t distribution based on each columns mean and standards deviation
}
\usage{
studentize(x)
}
\arguments{
\item{x}{ an xts like object}
}
\examples{
\dontrun{
x = matrix(c(1:10,5:15,10:-4), ncol=3)
studentize(x)
}
}
\author{Joao Claudio Macosso}
