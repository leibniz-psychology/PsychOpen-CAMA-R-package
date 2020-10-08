rmaModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL) {

  library(metafor)
  library(psych)
  library(jsonlite)
  library(labelVector)

  if( is.null(pred1) && is.null(pred2)){


    if(measure == "COR") {
      rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",data=dat)

      ## Transform Z back to Pearson's r and calculation of 95% CI
      #rma_model$yi<-fisherz2r(rma_model$yi)

      theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)
      rma_model.tidy
      return(paste(print(rma_model),print(theRealModel)))

    }else{
      rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)

      return(summary(rma_model))

    }
  }

  if( !is.null(pred1) && !is.null(pred2)){

    if(pred1["type"]=="num" && pred2["type"]=="num"){

      if(measure == "COR") {
        mod1<-scale(dat[,pred1["value"]])[,1]
        mod2<-scale(dat[,pred2["value"]])[,1]

        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=~mod1+mod2, measure="ZCOR",data=dat)

        ## Transform Z back to Pearson's r and calculation of 95% CI
        #rma_model$yi<-fisherz2r(rma_model$yi)

        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)

        return(paste(print(rma_model),print(theRealModel)))

      }else{

        mod1<-scale(dat[,pred1["value"]])[,1]
        mod2<-scale(dat[,pred2["value"]])[,1]

        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=~mod1+mod2,measure=measure,data=dat)

        return(summary(rma_model))
      }
    }else if(pred1["type"]=="cat" && pred2["type"]=="cat"){

      if(measure == "COR") {
        mod1<-factor(dat[,pred1["value"]])
        mod1 <- set_label(mod1,pred1["label"])
        mod2<-factor(dat[,pred2["value"]])
        mod2 <- set_label(mod2,pred2["label"])

        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=~mod1+mod2, measure="ZCOR",data=dat)

        ## Transform Z back to Pearson's r and calculation of 95% CI
        #rma_model$yi<-fisherz2r(rma_model$yi)

        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)

        return(paste(print(rma_model),print(theRealModel)))

      }else{
        mod1<-factor(dat[,pred1["value"]])
        mod1 <- set_label(mod1,pred1["label"])
        mod2<-factor(dat[,pred2["value"]])
        mod2 <- set_label(mod2,pred2["label"])

        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=~mod1+mod2,measure=measure,data=dat)

        return(rma_model)
        # return(summary(rma_model))
      }
    }else {
      numerical<-""
      categorical<-""
      if(pred1["type"]=="cat"){
        categorical<-pred1
        numerical<-pred2
      }else{
        categorical<-pred2
        numerical<-pred1
      }
      if(measure == "COR") {
        mod1<-scale(dat[,numerical["value"]])[,1]
        mod2<-factor(dat[,categorical["value"]])

        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=~mod1+mod2, measure="ZCOR",data=dat)

        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)

        return(paste(print(rma_model),print(theRealModel)))

      }else{

        # label1<-pred1["label"]
        # label2<-pred2["label"]
        #
        # df<-data.frame(
        #   "label1"=scale(dat[,numerical["value"]])[,1],
        #   "label2"=factor(dat[,categorical["value"]])
        # )
        mod1<-scale(dat[,numerical["value"]])[,1]
        mod2<-factor(dat[,categorical["value"]])

        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=~mod1+mod2 ,measure=measure,data=dat)
        return(rma_model)
      }
    }
    }else if(!is.null(pred1) && is.null(pred2)){
        #Es gibt einen Prädiktor
        if(pred1["type"]=="cat"){
          mod<-factor(dat[,pred1["value"]])
          rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=mod,measure=measure,data=dat)
          return(rma_model)
        }else{
          mod<-scale(dat[,pred1["value"]])[,1]

          rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=mod,measure=measure,data=dat)
          return(rma_model)
        }
      }
    }


