
library(quantmod)
library(xts)
library(zoo)
library(R6)

loadTokens <- function(tokens, start, end){
  dt = lapply(tokens, function(x) quantmod::getSymbols(x, auto.assign = F, from = start, to = end)[,6])
  dt <- Reduce(merge, dt)
  colnames(dt) <- tokens
  return(dt)
}

quantR <- R6Class("quantR",
                  public = list(
                    tokens=NULL,
                    start=NULL,
                    end=NULL,
                    data=NULL,
                    return = NULL,

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
                      }

                    },
                    VaR = function(return.type = "log", interval = "d", alpha = 0.05,...){
                      if(return.type == "log"){
                        self$return = na.omit(diff(log(self$data)))
                        VaRs = sapply(self$return, quantile, alpha)
                        return(VaRs)
                      }
                      #work in progress ...
                    },
                    print = function(...){
                      cat(paste("tokens: ",self$tokens, collapse = ", "), " \n")
                      cat("start date: ", self$start, " \n")
                      cat("end date: ", self$end)
                      invisible(self)
                    }
                  )

  )

