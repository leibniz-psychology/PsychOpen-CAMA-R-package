#' @title rma uni model
#' @description
#' Using metafor package to fit a rma uni model.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @return
#' rma uni model
#' also creates a json file (imgHeight.json) that is used in a later api call to define the height of the plots
#' @author Robert Studtrucker
#' @export
rma <- function(yi,vi,measure,d,effect="Effect") {
  #library('metafor')
  #library("ggplot2")
  #library('jsonlite')
  #library('psych')

  requireNamespace("metafor")
  requireNamespace("ggplot2")
  requireNamespace("jsonlite")
  requireNamespace("psych")

  #load the in variable d defined dataset from the package
  dat <- d
  checkParameter(dat,c(yi,vi))
  dat <- dat[order(dat$r_year),]

  if(measure == "COR") {
    # z-standardisierte Daten erstellen
    temp_dat <- metafor::escalc(measure="ZCOR", ri=dat[,yi], vi=dat[,vi], ni=dat[,"o_ni"], data=dat, var.names=c("o_zcor","o_zcor_var"))

    # Modell berechnen
    rma_model <- metafor::rma.uni(temp_dat[,"o_zcor"],temp_dat[,"o_zcor_var"], measure="ZCOR",slab=paste(dat$r_author, dat$r_year))

  }else{
    rma_model <- metafor::rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,slab=paste(dat$r_author, dat$r_year))
  }
  gc() # Force R to release memory it is no longer using
  return(rma_model)
}
