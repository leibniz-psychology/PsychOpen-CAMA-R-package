



funnelPLot <- function(yi,vi,measure,d,effectName="Effect") {

  library(metafor)
  library(metaviz)

  dat<-get(d)


    #rma_model <- rma.uni(transf.rtoz(dat[,yi],dat[,o_ni]), transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",data=dat)

  rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
  RTest <-regtest(x=rma_model)

    #fp <- viz_funnel(x=rma_model,
    #                 text_size =4,
    #                 xlab = effectName,
    #                 method = "REML",
    #                 x_trans_function = tanh,
    #                 contours_type="REM"
    #                 )

    #rma_model$yi<-fisherz2r(rma_model$yi)
    #rma_model$sei<-fisherz2r(rma_model$sei)
  metafor::funnel(rma_model, yaxis="sei", xlab=measure) # 'label'


  return(RTest)
}
