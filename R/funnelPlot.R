



funnelPLot <- function(yi,vi,measure,d) {

  library(metafor)

  dat<-get(d)

  funnel_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
  RTest <-regtest(x=funnel_model)

  metafor::funnel(funnel_model, yaxis="sei", xlab=measure) # 'label'

  return(RTest)
}
