###### Documentation #######
#
# Code developed by Tanja Burgard and Robert Studtrucker
#

## Input variables ##

# d -> string of dataset name which json representation should be returned

## Output ##


netMetagetTRTS <- function(d) {
  library(jsonlite)
  #load the in variable d defined dataset from the package
  dat<-get(d)

  combined_treat<-c()
  combined_treat<-c(combined_treat,dat["treat1"],recursive = TRUE,use.names=FALSE)
  combined_treat<-c(combined_treat,dat["treat2"],recursive = TRUE,use.names=FALSE)

  trts<-unique(combined_treat)[!is.na(unique(combined_treat))]
  write_json(trts, "trts.json")
}
