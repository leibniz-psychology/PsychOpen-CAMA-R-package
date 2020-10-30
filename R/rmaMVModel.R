rmaMVModel <- function(yi,vi,measure,d,pred1=NULL,pred2=NULL) {

  library(metafor)
  library(psych)
  library(jsonlite)
  library(labelVector)

  dat<-get(d)

  pred1<-unlist(pred1)
  pred2<-unlist(pred2)

  if( is.null(pred1) && is.null(pred2)){


    if(measure == "COR") {
      rma_mvmodel <- rma.mv(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),
                          random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                          measure="ZCOR",data=dat)

      ## Transform Z back to Pearson's r and calculation of 95% CI
      #rma_model$yi<-fisherz2r(rma_model$yi)

      theRealModel<-predict(rma_mvmodel, digits = 3, transf = transf.ztor)

      return(paste(print(rma_mvmodel),print(theRealModel)))

    }else{

      rma_mvmodel <- rma.mv(yi=dat[,yi],V=dat[,vi],
                            random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                            measure=measure,data=dat)

      return(summary(rma_mvmodel))

    }
  }

  if( !is.null(pred1) && !is.null(pred2)){

    if(pred1["type"]=="num" && pred2["type"]=="num"){

      if(measure == "COR") {
        mod1<-scale(dat[,pred1["value"]])[,1]
        mod2<-scale(dat[,pred2["value"]])[,1]

        rma_mvmodel <- rma.mv(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=~mod1+mod2,
                              random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                              measure="ZCOR",data=dat)

        ## Transform Z back to Pearson's r and calculation of 95% CI
        #rma_model$yi<-fisherz2r(rma_model$yi)

        theRealModel<-predict(rma_mvmodel, digits = 3, transf = transf.ztor)

        return(paste(print(rma_mvmodel),print(theRealModel)))

      }else{

        mod1<-scale(dat[,pred1["value"]])[,1]
        mod2<-scale(dat[,pred2["value"]])[,1]

        rma_mvmodel <- rma.mv(yi=dat[,yi],V=dat[,vi],mods=~mod1+mod2,
                              random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                              measure=measure,data=dat)

        return(summary(rma_mvmodel))
      }
    }else if(pred1["type"]=="cat" && pred2["type"]=="cat"){

      if(measure == "COR") {
        mod1<-factor(dat[,pred1["value"]])
        mod1 <- set_label(mod1,pred1["label"])
        mod2<-factor(dat[,pred2["value"]])
        mod2 <- set_label(mod2,pred2["label"])

        rma_mvmodel <- rma.mv(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=~mod1+mod2,
                             random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                             measure="ZCOR",data=dat)

        ## Transform Z back to Pearson's r and calculation of 95% CI
        #rma_model$yi<-fisherz2r(rma_model$yi)

        theRealModel<-predict(rma_mvmodel, digits = 3, transf = transf.ztor)

        return(paste(print(rma_mvmodel),print(theRealModel)))

      }else{
        mod1<-factor(dat[,pred1["value"]])
        mod1 <- set_label(mod1,pred1["label"])
        mod2<-factor(dat[,pred2["value"]])
        mod2 <- set_label(mod2,pred2["label"])

        rma_mvmodel <- rma.mv(yi=dat[,yi],V=dat[,vi],mods=~mod1+mod2,
                              random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                              measure=measure,data=dat)

        return(rma_mvmodel)

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

        rma_mvmodel <- rma.mv(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),mods=~mod1+mod2,
                             random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                             measure="ZCOR",data=dat)

        theRealModel<-predict(rma_mvmodel, digits = 3, transf = transf.ztor)

        return(paste(print(rma_mvmodel),print(theRealModel)))

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

        rma_mvmodel <- rma.mv(yi=dat[,yi],V=dat[,vi],mods=~mod1+mod2 ,
                              random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                              measure=measure,data=dat)
        return(rma_mvmodel)
      }
    }
  }else if(!is.null(pred1) && is.null(pred2)){
    #Es gibt einen Prädiktor
    if(pred1["type"]=="cat"){


      #       rma_mvmodel <- rma.mv(yi=dat[,yi],V=dat[,vi],mods=~mod,
      #                       random=list(~1 | outcome_ID / sample_ID / report_ID),
      #                       measure=measure,data=dat)
      # mods <- paste(mod, collapse = "+")

      #warum haben wir hier factor genommen?
      mod<-factor(dat[,pred1["value"]])
      moddat<-cbind(dat, mod)
      rma_formula <- as.formula(sprintf("%s ~ %s", yi,pred1["value"]))

      rma_mvmodel <- metafor::rma.mv(rma_formula, V =moddat[,vi],
                      random=list(~1 | outcome_ID,~1 | sample_ID, ~1| report_ID),
                      data=moddat)

      return(rma_mvmodel)
    }else{
      #hier stimmt etwas noch nicht
      mod<-scale(dat[,pred1["value"]])[,1]
      moddat<-cbind(dat, mod)
      rma_formula <- as.formula(sprintf("%s ~ %s", "mod",pred1["value"]))


      rma_mvmodel <- rma.mv(rma_formula,V=moddat[,vi],
                            random=list(~1 | outcome_ID ,~1 | sample_ID ,~1 | report_ID),
                            measure=measure,data=moddat)
      return(rma_mvmodel)
    }
  }
}


