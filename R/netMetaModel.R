
#' @title netmeta model
#' @description
#' Creates a netmeta model which is used for further function calls
#' @param d
#' A \code{string} representing the dataset name that is used to fit the netmeta model.
#' @param reference
#' A \code{string} representing the reference category that should be used for the netmeta model.
#' @return returns nothing but creates a netmeta model which could be uses in other function calls
#' @author Robert Studtrucker
#' @export
netMetaModel <- function(d,reference="Placebo") {

  #load needed dependencies
  requireNamespace("netmeta")


  #load the in variable d defined dataset from the package
  dat <- checkData(d)

  TE <- dat[,"TE"]
  seTE <- dat[,"seTE"]
  studlab<-dat[,"studlab"]
  treat1<-dat[,"treat1"]
  treat2<-dat[,"treat2"]

  combined_treat<-c()
  combined_treat<-c(combined_treat,dat["treat1"],recursive = TRUE,use.names=FALSE)
  combined_treat<-c(combined_treat,dat["treat2"],recursive = TRUE,use.names=FALSE)

  nma1<-netmeta::netmeta(
    data=dat,
    TE=TE,
    seTE = seTE,
    studlab = studlab,
    treat1 = treat1,
    treat2 = treat2,
    comb.fixed = FALSE,
    seq = unique(combined_treat)[!is.na(unique(combined_treat))],
    tol.multiarm=0.1,
    ref = reference,
    warn = FALSE)
  }
