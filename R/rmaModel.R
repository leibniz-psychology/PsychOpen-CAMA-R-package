rmaModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL) {

  library(metafor)
  library(psych)
  library(jsonlite)
  dat<-get(d)
  pred1<-unlist(pred1)
  pred2<-unlist(pred2)


  if( is.null(pred1) && is.null(pred2)){


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

  if( !is.null(pred1) && !is.null(pred2)){

    if(pred1["type"]=="num" && pred2["type"]=="num"){

      if(measure == "COR") {
        somedata<-cbind(label1=scale(dat[,pred1["value"]])[,1],label2=scale(dat[,pred2["value"]])[,1])
        colnames(somedata)[1]<-pred1["label"]
        colnames(somedata)[2]<-pred2["label"]

        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=somedata, measure="ZCOR",data=dat)

        ## Transform Z back to Pearson's r and calculation of 95% CI
        #rma_model$yi<-fisherz2r(rma_model$yi)

        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)

        return(paste(print(rma_model),print(theRealModel)))

      }else{

        somedata<-cbind(label1=scale(dat[,pred1["value"]])[,1],label2=scale(dat[,pred2["value"]])[,1])

        colnames(somedata)[1]<-pred1["label"]
        colnames(somedata)[2]<-pred2["label"]

        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=somedata,measure=measure,data=dat)

        return(summary(rma_model))
      }
    }else if(pred1["type"]=="cat" && pred2["type"]=="cat"){

      if(measure == "COR") {
        somedata<-cbind(factor(dat[,pred1["value"]]),factor(dat[,pred2["value"]]))

        colnames(somedata)[1]<-pred1["label"]
        colnames(somedata)[2]<-pred2["label"]

        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=somedata, measure="ZCOR",data=dat)

        ## Transform Z back to Pearson's r and calculation of 95% CI
        #rma_model$yi<-fisherz2r(rma_model$yi)

        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)

        return(paste(print(rma_model),print(theRealModel)))

      }else{
        somedata<-cbind(label1=factor(dat[,pred1["value"]]),label2=factor(dat[,pred2["value"]]))
        colnames(somedata)[1]<-pred1["label"]
        colnames(somedata)[2]<-pred2["label"]
        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=somedata,measure=measure,data=dat)

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
        somedata<-cbind(scale(dat[,numerical["value"]])[,1],factor(dat[,categorical["value"]]))

        colnames(somedata)[1]<-numerical["label"]
        colnames(somedata)[2]<-categorical["label"]
        rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=somedata, measure="ZCOR",data=dat)

        ## Transform Z back to Pearson's r and calculation of 95% CI
        #rma_model$yi<-fisherz2r(rma_model$yi)

        theRealModel<-predict(rma_model, digits = 3, transf = transf.ztor)

        return(paste(print(rma_model),print(theRealModel)))

      }else{
        somedata<-cbind(scale(dat[,numerical["value"]])[,1],factor(dat[,categorical["value"]]))

        colnames(somedata)[1]<-pred1["label"]
        colnames(somedata)[2]<-pred2["label"]
        rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=somedata,measure=measure,data=dat)
        return(rma_model)
      }
    }
    }else if(!is.null(pred1) && is.null(pred2)){
        #Es gibt einen Prädiktor
        if(pred1["type"]=="cat"){
          somedata<-cbind(factor(dat[,pred1["value"]]))
          colnames(somedata)<-pred1["label"]
          rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=somedata,measure=measure,data=dat)
          return(rma_model)
        }else{
          somedata<-cbind(scale(dat[,pred1["value"]])[,1])

          colnames(somedata)<-pred1["label"]

          rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],mods=somedata,measure=measure,data=dat)
          return(rma_model)
        }
      }
    }


