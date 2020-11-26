rmaModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL) {

  library(metafor)
  library(psych)
  library(jsonlite)
  library(labelVector)


  dat<-get(d)
  pred1<-unlist(pred1)
  pred2<-unlist(pred2)

    #es gibt keinen Moderator*****************************************************
    if( is.null(pred1) && is.null(pred2)){
      if(measure == "COR") {
        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",data=dat)
        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)
        return(paste(print(rma_model),print(theRealModel)))
      }else{
        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,data=dat)
        return(summary(rma_model))
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

        rma_model <- rma.uni(rma_formula, vi=moddat[,"cor_vi"], measure="ZCOR",data=moddat)

        return(rma_model)

      }else{

        rma_formula <- as.formula(sprintf("%s ~ %s", yi,mods))
        rma_model <- rma.uni(rma_formula,vi=dat[,vi],measure=measure,data=moddat)
        return(summary(rma_model))
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
        rma_model <- rma.uni(rma_formula,vi=moddat[,"cor_vi"], measure="ZCOR",data=moddat)

        return(rma_model)

      }else{

        rma_model <- rma.uni(rma_formula, vi=dat[,vi],measure=measure,data=moddat)
        return(rma_model)
      }
    }
}



