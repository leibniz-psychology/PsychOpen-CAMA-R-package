###### Documentation #######
#
# Code developed by Tanja Burgard and Robert Studtrucker
#

## Input variables ##

# d -> string of dataset name which json representation should be returned

## Output ##

# returns a json representation of an included dataset

jsonSummary <- function(d) {

  dat<-get(d)
  val <-jsonlite::toJSON(lapply(dat, function(x){as.list(summary(x))}), pretty = TRUE, auto_unbox = TRUE)
  gc() # Force R to release memory it is no longer using
  return(val)

}
