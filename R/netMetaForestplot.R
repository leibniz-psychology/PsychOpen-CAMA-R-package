###### Documentation #######
#
# Code developed by Tanja Burgard and Robert Studtrucker
#

## Input variables ##

# d -> string of dataset name which json representation should be returned

## Output ##
netMetaForestplot <- function(model,reference) {
  netmeta::forest.netmeta(model, ref = reference, sortvar = TE,col.square = "#0097c6")
}
