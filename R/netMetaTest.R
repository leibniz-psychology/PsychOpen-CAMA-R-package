

netMetaTest <- function(model,reference) {

  netmeta::forest.netmeta(model, ref = reference, sortvar = TE,col.square = "#0097c6")
  #gc() # Force R to release memory it is no longer using

}
