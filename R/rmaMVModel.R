###### Documentation #######
# Using metafor rma.mv function to fit and return a meta-analytic multivariate/multilevel fixed- and random/mixed-effects model with or without moderators for the given dataset. See Documentation of metafor package for details.
#

## Input variables ##

# yi -> string of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
# vi ->
# measure
# d -> string of dataset name that should be used for fitting
# pred1 -> optional parameter of type string, wich represents the name of the variable which holds the vectors used as input for the metafor mods argument in the selected dataset (d)
# pred2 -> optional parameter of type string, wich represents the name of the variable which holds the vectors used as input for the metafor mods argument in the selected dataset (d)
# nesting -> optional parameter of type string containing a list of nesting parameter

## Output ##

# returns a fitted rma_mv model

rmaMVModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL,nesting=NULL) {

  library(metafor)
  library(psych)
  library(jsonlite)
  library(labelVector)

  dat<-get(d)

  pred1<-unlist(pred1)
  pred2<-unlist(pred2)

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

  #es gibt keinen Moderator*****************************************************
  if( is.null(pred1) && is.null(pred2)){
    if(measure == "COR") {
      rma_mvmodel <- rma.mv(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),
                            random=nest,
                            measure="ZCOR",data=dat)

      theRealModel<-predict( rma_mvmodel, digits = 3, transf = transf.ztor)
      print(rma_mvmodel)
      print(theRealModel)

    }else{
      rma_mvmodel <- rma.mv(yi=dat[,yi],V=dat[,vi],
                            random=nest,
                            measure=measure,data=dat)
      return(summary(rma_mvmodel))
    }
  }

  #es gibt zwei Moderatoren*****************************************************
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

    #Model berechnen je nach measure
    if(measure == "COR") {

      moddat["cor_yi"]<-transf.rtoz(dat[,yi],dat[,o_ni])
      moddat["cor_vi"]<-transf.rtoz(dat[,vi],dat[,o_ni])
      rma_formula <- as.formula(sprintf("%s ~ %s", "cor_yi",mods))

      rma_mvmodel <- rma.mv(rma_formula, V=moddat[,"cor_vi"],
                          random=nest,
                          measure="ZCOR",data=moddat)

      return(rma_mvmodel)

    }else{

      rma_formula <- as.formula(sprintf("%s ~ %s", yi,mods))
      rma_mvmodel <- rma.mv(rma_formula,V=dat[,vi],
                            random=nest,
                            measure=measure,data=moddat)
      return(summary(rma_mvmodel))
    }
  }

  #es gibt einen Moderator******************************************************
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

    #Model berechnen je nach measure
    if(measure == "COR") {
      moddat["cor_yi"]<-transf.rtoz(dat[,yi],dat[,o_ni])
      moddat["cor_vi"]<-transf.rtoz(dat[,vi],dat[,o_ni])

      rma_formula <- as.formula(sprintf("%s ~ %s", "cor_yi",pred1["value"]))
      rma_mvmodel <- rma.mv(rma_formula,V=moddat[,"cor_vi"],
                            random=nest,
                            measure="ZCOR",data=moddat)

      return(rma_mvmodel)

    }else{

      rma_mvmodel <- rma.mv(rma_formula, V=dat[,vi],
                            random=nest,
                            measure=measure,data=moddat)
      return(rma_mvmodel)
    }
  }
}


