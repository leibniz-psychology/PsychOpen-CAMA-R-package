rmaModel <- function(yi,vi,measure,moderators,d) {

  library(metafor)
  library(psych)
  library(jsonlite)
  dat<-get(d)

  if(measure == "COR") {
    rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",data=dat)

    ## Transform Z back to Pearson's r and calculation of 95% CI
    #rma_model$yi<-fisherz2r(rma_model$yi)

    theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)

    return(paste(print(rma_model),print(theRealModel)))

  }else{
    rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
    return(summary(rma_model))
  }
}
