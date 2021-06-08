#' @title rma Model
#' @description
#' Using metafor rma.uni function to fit and return a meta-analytic univariate fixed- and random/mixed-effects model with or without moderators for the given dataset.
#' See Documentation of metafor package for details.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param measure
#' A character string indicating underlying summary measure
#' @param pred1
#' Optional parameter of type \code{String}, wich represents the name of the variable which holds the vectors used as input for the metafor mods argument in the selected dataset (d).
#' @param pred2
#' Optional parameter of type \code{String}, wich represents the name of the variable which holds the vectors used as input for the metafor mods argument in the selected dataset (d).
#' @return returns a fitted rma_uni model
#' @author Robert Studtrucker
#' @export
rmaModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL) {

  library(metafor)
  #library(labelVector)
  #library(psych)
  library(jsonlite)

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

  pred1<-unlist(pred1)
  pred2<-unlist(pred2)

    #no moderators given ***
    if( is.null(pred1) && is.null(pred2)){
      if(measure == "COR") {
        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",data=dat)
        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)
        print(rma_model)
        print(theRealModel)

      }else{
        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,data=dat)
        return(summary(rma_model))
      }
    }

    #two moderators given ****
    if( !is.null(pred1) && !is.null(pred2)){
      moddat<-dat
      #Moderatoren transformieren
      if(pred1["type"]=="num"){
          mod1<-scale(dat[,pred1["value"]])[,1]
        }else{
          mod1<-factor(dat[,pred1["value"]])
        }

      if(pred2["type"]=="num"){
        mod2<-scale(dat[,pred2["value"]])[,1]
      }else{
        mod2<-factor(dat[,pred2["value"]])
      }

      moddat[pred1["value"]]<-mod1
      moddat[pred2["value"]]<-mod2
      mods <- paste(c(pred1["value"],pred2["value"]), collapse = "+")

      #calculate model depending on the measure
      if(measure == "COR") {

        moddat["cor_yi"]<-transf.rtoz(dat[,yi],dat[,o_ni])
        moddat["cor_vi"]<-transf.rtoz(dat[,vi],dat[,o_ni])
        rma_formula <- as.formula(sprintf("%s ~ %s", "cor_yi",mods))

        rma_model <- rma.uni(rma_formula, vi=moddat[,"cor_vi"], measure="ZCOR",data=moddat)

        return(rma_model)

      }else{

        rma_formula <- as.formula(sprintf("%s ~ %s", yi,mods))
        rma_model <- rma.uni(rma_formula,vi=dat[,vi],measure=measure,data=moddat)
        return(summary(rma_model))
      }
    }

    #one moderator given ***
    if(!is.null(pred1) && is.null(pred2)){

      #transform moderators
      if(pred1["type"]=="num"){
        mod1<-scale(dat[,pred1["value"]])[,1]
        moddat<-dat
        moddat[pred1["value"]]<-mod1
        rma_formula <- as.formula(sprintf("%s ~ %s", yi,pred1["value"]))
      }else{
        mod1<-factor(dat[,pred1["value"]])
        moddat<-dat
        moddat[pred1["value"]]<-mod1
        rma_formula <- as.formula(sprintf("%s ~ %s", yi,pred1["value"]))
      }

      #calculate model depending on the measure
      if(measure == "COR") {
        moddat["cor_yi"]<-transf.rtoz(dat[,yi],dat[,o_ni])
        moddat["cor_vi"]<-transf.rtoz(dat[,vi],dat[,o_ni])

        rma_formula <- as.formula(sprintf("%s ~ %s", "cor_yi",pred1["value"]))
        rma_model <- rma.uni(rma_formula,vi=moddat[,"cor_vi"], measure="ZCOR",data=moddat)

        return(rma_model)

      }else{

        rma_model <- rma.uni(rma_formula, vi=dat[,vi],measure=measure,data=moddat)
        return(rma_model)
      }
    }
}



