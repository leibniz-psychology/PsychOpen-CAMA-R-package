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

  #library(metafor)
  #library(jsonlite)
  requireNamespace("metafor")
  requireNamespace("jsonlite")

  #load the in variable d defined dataset from the package
  dat <- checkData(d)
  checkParameter(dat,c(yi,vi))
  pred1<-unlist(pred1)
  pred2<-unlist(pred2)

    #no moderators given ***
    if( is.null(pred1) && is.null(pred2)){

      if(measure == "COR") {

        # z-standardisierte Daten erstellen
        temp_dat <- metafor::escalc(measure="ZCOR", ri=dat[,yi], vi=dat[,vi], ni=dat[,"o_ni"], data=dat, var.names=c("o_zcor","o_zcor_var"))

        # Modell berechnen
        rma_model <- metafor::rma.uni(temp_dat[,"o_zcor"],temp_dat[,"o_zcor_var"], measure="ZCOR")

        # Backtransformation für Interpretation
        theRealModel <- predict(rma_model, transf=metafor::transf.ztor, digits=3)

        print(rma_model)
        print(theRealModel)

        # Egger's Test (bei Funnel)
        # eggers <- regtest(res_temp)

      }else{
        rma_model <- metafor::rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,data=dat)
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

        # z-standardisierte Daten erstellen
        moddat <- metafor::escalc(measure="ZCOR", ri=moddat[,yi], vi=moddat[,vi], ni=moddat[,"o_ni"], data=moddat, var.names=c("o_zcor","o_zcor_var"))

        rma_formula <- as.formula(sprintf("%s ~ %s", "o_zcor",mods))

        rma_model <- metafor::rma.uni(rma_formula, vi=moddat[,"o_zcor_var"], measure="ZCOR",data=moddat)

        return(rma_model)

      }else{

        rma_formula <- as.formula(sprintf("%s ~ %s", yi,mods))
        rma_model <- metafor::rma.uni(rma_formula,vi=dat[,vi],measure=measure,data=moddat)
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

        # z-standardisierte Daten erstellen
        moddat <- metafor::escalc(measure="ZCOR", ri=moddat[,yi], vi=moddat[,vi], ni=moddat[,"o_ni"], data=moddat, var.names=c("o_zcor","o_zcor_var"))

        rma_formula <- as.formula(sprintf("%s ~ %s", "o_zcor",pred1["value"]))
        rma_model <- metafor::rma.uni(rma_formula,vi=moddat[,"o_zcor_var"], measure="ZCOR",data=moddat)

        return(rma_model)

      }else{

        rma_model <- metafor::rma.uni(rma_formula, vi=dat[,vi],measure=measure,data=moddat)
        return(rma_model)
      }
    }
}



