###### Documentation #######
#
# Code developed by Tanja Burgard and Robert Studtrucker
#

## Input variables ##

# model

## Output ##


netMetaMatrix <- function(model) {

  #load needed dependencies
  library(netmeta)

  # Output 2: Matrix of all pairwise comparisons
  res.matrix<-round(model$TE.fixed,3)
  res.matrix[lower.tri(res.matrix, diag=FALSE)] <- "."
  print(res.matrix)
  }
