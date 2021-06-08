#' @title Funnel plot
#' @description
#' Using metafor package to create a funnel plot.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param measure
#' A character string indicating underlying summary measure.
#' @return
#' returns a Funnel plot for the given dataset
#' @author Robert Studtrucker
#' @export
funnelPLot <- function(yi,vi,measure,d,effectName="Effect") {

  library(metafor)
  #library(metaviz)

  #load the in variable d defined dataset from the package
  dat <- tryCatch(
    {get(d)},
    error=function(cond) {
      message(paste("This dataset does not exist:", d))
      message("Here's the original error message:")
      message(cond)
      return(NULL)
    },
    warning=function(cond) {
      message(paste("input caused a warning:", d))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
  )

  rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
  RTest <-regtest(x=rma_model)

  metafor::funnel(rma_model, yaxis="sei") # 'label'
  metafor::funnel(rma_model, level=c(90, 95, 99), shade=c("white", "orange", "red"), refline=0, legend=TRUE)
  gc() # Force R to release memory it is no longer using

  return(RTest)
}
