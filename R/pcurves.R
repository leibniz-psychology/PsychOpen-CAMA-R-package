#' @title  Plot pcurve
#' @description
#' Using the R package meta to calculate a random effects meta-analysis based on estimates (e.g. log hazard ratios) and their standard errors. Afterwards using dmetar package to create a pcurve plot.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param measure
#' A character string indicating underlying summary measure
#' @return returns a pcurve plot
#' @author Robert Studtrucker
#' @export
pcurves<-function(yi,vi,measure,d) {

  requireNamespace("dmetar")
  requireNamespace("meta")


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

#Filtern auf nur peer reviewed arcticles
dat<-dat[dat$r_peer=="yes",]

# meta-Model
overall.meta <- meta::metagen(TE=dat[,yi], seTE=sqrt(dat[,vi]),data = dat, studlab = paste(r_author),
                        comb.fixed = FALSE,comb.random = TRUE,method.tau = "SJ",
                        hakn = TRUE,prediction = TRUE,sm = measure)

# p-curve
dmetar::pcurve(overall.meta)

}
