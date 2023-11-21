library(stats)
# library(quantmod)
# library(xts)
# library(zoo)
library(R6)

quantR <- R6::R6Class("quantR",
                  public = list(
                    tokens=NULL,
                    start=NULL,
                    end=NULL,
                    data=NULL,
                    return = NULL,

                    initialize = function(tokens, start, end=NULL, data = NULL){
                      self$tokens <- tokens
                      self$start <- start
                      if(!is.null(end)) {
                        self$end <- end
                      }
                      else{
                        self$end <- Sys.Date()
                      }

                      if(!is.null(data)){
                        self$data <- data[,tokens]
                      }
                      #if no data was provided, load using quantmod
                      else{
                        self$data <- loadTokens(self$tokens, start = self$start,  end = self$end)
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
