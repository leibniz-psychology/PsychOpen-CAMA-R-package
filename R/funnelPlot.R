



funnelPLot <- function(yi,vi,measure,d) {

  library(metafor)

  dat<-get(d)

  overall_forest <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure, data=dat)

  metafor::funnel(overall_forest, yaxis="sei", xlab="SMD") # 'label'

}
