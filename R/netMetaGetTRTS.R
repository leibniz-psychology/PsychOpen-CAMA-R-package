#' @title  Helper Function to get possible reference categories fot the netmeta model
#' @description
#' Helper Function to get possible reference categories to be used in the netmeta model
#' This function is used to populate the gui with reference options
#' @param d
#' A \code{string} representing the dataset name for which the reference categories should be retrieved
#' @return create a json file containing the reference categories in the opencpu session which can then be retrieved in another call
#' @author Robert Studtrucker
#' @export
netMetagetTRTS <- function(d) {
  #Todo check if there is a better way to do this
  requireNamespace("jsonlite")

  #load the in variable d defined dataset from the package
  dat <- checkData(d)

  combined_treat<-c()
  combined_treat<-c(combined_treat,dat["treat1"],recursive = TRUE,use.names=FALSE)
  combined_treat<-c(combined_treat,dat["treat2"],recursive = TRUE,use.names=FALSE)

  trts<-unique(combined_treat)[!is.na(unique(combined_treat))]
  jsonlite::write_json(trts, "trts.json")
}
