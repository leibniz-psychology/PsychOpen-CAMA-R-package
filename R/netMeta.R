


netMetaModel <- function(d,reference) {

  #load needed dependencies
  library(netmeta)


  #load the in variable d defined dataset from the package
  dat<-get(d)

  combined_treat<-c()
  for(i in names(dat)){
    if(substr(i,0,7) == "t_treat"){
      combined_treat<-c(combined_treat,dat[i],recursive = TRUE,use.names=FALSE)
      }
  }
  trts<-unique(combined_treat)[!is.na(unique(combined_treat))]


  nma1<-netmeta(dat, comb.fixed = FALSE, seq = trts, ref = reference)

  summary(nma1)

  }
