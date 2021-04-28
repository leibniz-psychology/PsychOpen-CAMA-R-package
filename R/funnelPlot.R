funnelPLot <- function(yi,vi,measure,d,effectName="Effect") {

  library(metafor)
  library(metaviz)

  dat<-get(d)

  rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
  RTest <-regtest(x=rma_model)

  metafor::funnel(rma_model, yaxis="sei") # 'label'
  metafor::funnel(rma_model, level=c(90, 95, 99), shade=c("white", "orange", "red"), refline=0, legend=TRUE)
  gc() # Force R to release memory it is no longer using

  return(RTest)
}
