# Contour enhanced funnel

contour_funnel <- function(yi,vi,measure,d,effectName="Effect") {
library(metafor)

  dat<-get(d)

  rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
  metafor::funnel(rma_model, level=c(90, 95, 99), shade=c("white", "orange", "red"), refline=0, legend=TRUE)
}
