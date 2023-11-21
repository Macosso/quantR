\name{loadTokens}
\alias{loadTokens}
\docType{package}
\title{
Import trading data from yahoo finance
}
\description{
Import trading data for multiple tickers from yahoo finance
}
\usage{
loadTokens(tokens, start, end=NULL, variable = "adjusted.price")
}
\arguments{
\item{tokens}{A vector of trading tokens to import}
\item{start}{The start date of the data series}
\item{end}{The end date of data series}
\item{variable}{The variable to be selected from ```adjusted.price, volume, close.price, low.price, high.price, open.price```, default: ```adjusted.price```}

}

\value{An xts object}
\examples{

\dontrun{
quantrDate <- loadTokens(c("MSFT", "GOOG", "GM"), start = "2020-01-01")
}
}
\author{Joao Claudio Macosso}
