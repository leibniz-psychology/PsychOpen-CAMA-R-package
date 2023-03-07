#' @title Funnel plot
#' @description
#' Using metafor package to create a funnel plot.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param effectName
#' A \code{string} representing the effect name that should be printed as label. defaults to "Effect"
#' @param measure
#' A character string indicating underlying summary measure.
#' @return
#' returns a Funnel plot for the given dataset
#' @author Robert Studtrucker
#' @export
funnelPLot <- function(yi,vi,measure,d,peer="no", effectName="Effect") {

  requireNamespace("metafor")

  #load the in variable d defined dataset from the package
  dat <- checkData(d)
  checkParameter(dat,c(yi,vi,"r_peer"))

  # Filtern nach veröffentlichten Studien wenn in der Anwendung ausgewählt (peer reviewed yes/no)
  # Per default werden alle Studien mit einbezogen
  if(peer == "yes"){
    filtered_dat <- subset(dat,r_peer=="yes")
  }else{
    filtered_dat<-dat
  }

  if(measure == "COR") {
    # z-standardisierte Daten erstellen
    temp_dat <- metafor::escalc(measure="ZCOR", ri=filtered_dat[,yi], vi=filtered_dat[,vi], ni=filtered_dat[,"o_ni"], data=filtered_dat, var.names=c("o_zcor","o_zcor_var"))

    # Modell berechnen
    rma_model <- metafor::rma.uni(temp_dat[,"o_zcor"],temp_dat[,"o_zcor_var"], measure="ZCOR")

    RTest <-metafor::regtest(x=rma_model)
    metafor::funnel(rma_model, yaxis="sei") # 'label'
    metafor::funnel(rma_model, level=c(90, 95, 99), shade=c("white", "orange", "red"), refline=0, legend=TRUE)
    gc() # Force R to release memory it is no longer using

    return(RTest)

  }else{
    rma_model <- metafor::rma.uni(yi=filtered_dat[,yi],vi=filtered_dat[,vi],measure=measure)
    RTest <-metafor::regtest(x=rma_model)

    metafor::funnel(rma_model, yaxis="sei") # 'label'
    metafor::funnel(rma_model, level=c(90, 95, 99), shade=c("white", "orange", "red"), refline=0, legend=TRUE)
    gc() # Force R to release memory it is no longer using

    return(RTest)
  }


}
