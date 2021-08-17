#' @title Forest plot
#' @description
#' Using metafor package to create a forest plot and metaviz package to create a rain forest plot.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @return
#' creates a forest and rainforest plot
#' also creates a json file (imgHeight.json) that is used in a later api call to define the height of the plots
#' @author Robert Studtrucker
#' @export
rmaZCOR <- function(yi,vi,d,effect="Effect") {
  library('metafor')
  library("ggplot2")
  library("metaviz")
  library('jsonlite')
  library('psych')

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
  rma_model <- rma.uni(yi=transf.rtoz(dat[,yi],dat[,o_ni]), vi=transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",slab=paste(dat$r_author, dat$r_year))

    }
