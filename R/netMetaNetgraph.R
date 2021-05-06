


netMetaNetgraph <- function(model,d) {

  #load needed dependencies
  library(netmeta)
#
#   #load the in variable d defined dataset from the package
  dat<-get(d)

  TE <- dat[,"TE"]
  seTE <- dat[,"seTE"]
  studlab<-dat[,"studlab"]
  treat1<-dat[,"treat1"]
  treat2<-dat[,"treat2"]

  combined_treat<-c()
  combined_treat<-c(combined_treat,dat["treat1"],recursive = TRUE,use.names=FALSE)
  combined_treat<-c(combined_treat,dat["treat2"],recursive = TRUE,use.names=FALSE)

  trts<-unique(combined_treat)[!is.na(unique(combined_treat))]

  gr<-netgraph(model, col="#0000CC", col.points="white", seq = trts)
  print(gr)
 # gc() # Force R to release memory it is no longer using

}
