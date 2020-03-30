



rmaModel <- function(yi,vi,measure,moderators,random,d) {

  library(metafor)
  dat<-get(d)


  overall_forest <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure, data=dat)
  return(summary(overall_forest))

}
