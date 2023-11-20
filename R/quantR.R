
library(quantmod)
library(xts)
library(zoo)
library(R6)

loadTokens <- function(tokens, start, end){
  dt = lapply(tokens, function(x) quantmod::getSymbols(x, auto.assign = F, from = start, to = end)[,6])
  return(dt)
}

quantR <- R6Class("quantR",
                  public = list(
                    tokens=NULL,
                    start=NULL,
                    end=NULL,
                    data=NULL,

                    initialize = function(tokens, start, end=NULL, data = NULL){
                      self$tokens <- tokens
                      self$start <- start
                      self$end <- end

                      if(!is.null(data)){
                        self$data <- data[,tokens]
                      }
                      #if no data was provided, load using quantmod
                      else{
                        self$data <- loadTokens(self$tokens, start = self$start,  end = self$end)
                        self$data <- Reduce(merge, self$data)
                        colnames(self$data) <- self$tokens
                      }

                    },
                    print = function(...){
                      cat(paste("tokens: ",self$tokens, collapse = ", "), " \n")
                      cat("start date: ", self$start, " \n")
                      cat("end date: ", self$end)
                      invisible(self)
                    }
                  )

  )

obj <- quantR$new(tokens = c("MSFT", "GOOG"), start = "2020-01-01",  end = "2022-01-01")
print(obj)

head(obj$data)



