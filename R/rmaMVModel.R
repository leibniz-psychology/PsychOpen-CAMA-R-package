#' @title rma MVModel
#' @description
#' Using metafor rma.mv function to fit and return a meta-analytic multivariate/multilevel fixed- and random/mixed-effects model with or without moderators for the given dataset.
#' See Documentation of metafor package for details.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param pred1
#' Optional parameter of type \code{String}, wich represents the name of the variable which holds the vectors used as input for the metafor mods argument in the selected dataset (d).
#' @param pred2
#' Optional parameter of type \code{String}, wich represents the name of the variable which holds the vectors used as input for the metafor mods argument in the selected dataset (d).
#' @param nesting
#' Optional parameter of type \code{list}, containing nesting parameter
#' @return returns a fitted rma_mv model
#' @author Robert Studtrucker
#' @export
rmaMVModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL,nesting=NULL) {

  #load needed dependencies
  #library(metafor)
  #library(psych)
  #library(labelVector)
  requireNamespace("metafor")
  requireNamespace("psych")
  #requireNamespace("labelVector")

  #load the in variable d defined dataset from the package
  dat <- checkData(d)
  checkParameter(dat,c(yi,vi))

  pred1<-unlist(pred1)
  pred2<-unlist(pred2)

  #check if the choosen dataset has a nesting and prepare corresponding rma_mv model input if so
  if(is.null(nesting)){
    nest<-list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID)
  }else{
    nest<-list()
    i=1
    for(p in nesting){
      nest[i] <-paste("~1 |", p)
      i=i+1
    }
    nest<-lapply(nest, as.formula)
  }
  # there is no moderator defined
  if( is.null(pred1) && is.null(pred2)){
    if(measure == "COR") {

      # z-standardisierte Daten erstellen
      temp_dat <- metafor::escalc(measure="ZCOR",
                         ri=dat[,yi],
                         vi=dat[,vi],
                         ni=dat[,"o_ni"],
                         data=dat,
                         var.names=c("o_zcor","o_zcor_var"))

      # Modell berechnen
      rma_mvmodel <- metafor::rma.mv(temp_dat[,"o_zcor"],
                            temp_dat[,"o_zcor_var"],
                            data=temp_dat,
                            measure="ZCOR",
                            random=nest)

      # Backtransformation für Interpretation
      theRealModel <- predict(rma_mvmodel, transf=metafor::transf.ztor, digits=3)


      # rma_mvmodel <- rma.mv(transf.rtoz(dat[,yi], dat[,o_ni]),
      #                       transf.rtoz(dat[,vi],dat[,o_ni]),
      #                       random=nest,data=dat)
      #
      # theRealModel<-predict( rma_mvmodel,
      #                        digits = 3,
      #                        transf = transf.ztor)


      print(rma_mvmodel)
      print(theRealModel)

    }else{
      rma_mvmodel <- metafor::rma.mv(yi=dat[,yi],V=dat[,vi],
                            random=nest,
                            measure=measure,data=dat)
      gc() # Force R to release memory it is no longer using
      return(summary(rma_mvmodel))
    }
  }

  # there are two moderators defined
  if( !is.null(pred1) && !is.null(pred2)){
    moddat<-dat

    # Moderatoren transformieren
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

    # calculate model depending on given measure
    if(measure == "COR") {

      # z-standardisierte Daten erstellen
      moddat <- metafor::escalc(measure="ZCOR",
                       ri=moddat[,yi],
                       vi=moddat[,vi],
                       ni=moddat[,"o_ni"],
                       data=moddat,
                       var.names=c("o_zcor","o_zcor_var"))

      rma_formula <- as.formula(sprintf("%s ~ %s", "o_zcor",mods))

      rma_mvmodel <- metafor::rma.mv(rma_formula, V=moddat[,"o_zcor_var"], measure="ZCOR",data=moddat,random=nest,)


      # moddat["cor_yi"]<-transf.rtoz(dat[,yi],dat[,o_ni])
      # moddat["cor_vi"]<-transf.rtoz(dat[,vi],dat[,o_ni])
      # rma_formula <- as.formula(sprintf("%s ~ %s", "cor_yi",mods))
      #
      # rma_mvmodel <- rma.mv(rma_formula, V=moddat[,"cor_vi"],
      #                     random=nest,
      #                     measure="ZCOR",data=moddat)

      gc() # Force R to release memory it is no longer using
      return(rma_mvmodel)

    }else{

      rma_formula <- as.formula(sprintf("%s ~ %s", yi,mods))
      rma_mvmodel <- metafor::rma.mv(rma_formula,V=dat[,vi],
                            random=nest,
                            measure=measure,data=moddat)
      gc() # Force R to release memory it is no longer using
      return(summary(rma_mvmodel))
    }
  }

  # there is one moderator defined
  if(!is.null(pred1) && is.null(pred2)){

    #Moderatoren transformieren
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

    # fitting model depending on defined measure
    if(measure == "COR") {
      # z-standardisierte Daten erstellen
      moddat <- metafor::escalc(measure="ZCOR", ri=moddat[,yi], vi=moddat[,vi], ni=moddat[,"o_ni"], data=moddat, var.names=c("o_zcor","o_zcor_var"))

      rma_formula <- as.formula(sprintf("%s ~ %s", "o_zcor",pred1["value"]))
      rma_mvmodel <- metafor::rma.mv(rma_formula,
                            V=moddat[,"o_zcor_var"],
                            measure="ZCOR",
                            data=moddat,
                            random=nest)

      # moddat["cor_yi"]<-transf.rtoz(dat[,yi],dat[,o_ni])
      # moddat["cor_vi"]<-transf.rtoz(dat[,vi],dat[,o_ni])
      #
      # rma_formula <- as.formula(sprintf("%s ~ %s", "cor_yi",pred1["value"]))
      # rma_mvmodel <- rma.mv(rma_formula,V=moddat[,"cor_vi"],
      #                       random=nest,
      #                       measure="ZCOR",data=moddat)

      gc() # Force R to release memory it is no longer using
      return(rma_mvmodel)

    }else{

      rma_mvmodel <- metafor::rma.mv(rma_formula, V=dat[,vi],
                            random=nest,
                            measure=measure,data=moddat)
      gc() # Force R to release memory it is no longer using
      return(rma_mvmodel)
    }
  }
}


