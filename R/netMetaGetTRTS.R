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
  library(jsonlite)

  #load the in variable d defined dataset from the package
  dat <- tryCatch(
    {get(d)},
    error=function(cond) {
      message(paste("This dataset does not exist:", d))
      message("Here's the original error message:")
      message(cond)
      return(NULL)
    },
    warning=function(cond) {
      message(paste("input caused a warning:", d))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
  )

  combined_treat<-c()
  combined_treat<-c(combined_treat,dat["treat1"],recursive = TRUE,use.names=FALSE)
  combined_treat<-c(combined_treat,dat["treat2"],recursive = TRUE,use.names=FALSE)

  trts<-unique(combined_treat)[!is.na(unique(combined_treat))]
  write_json(trts, "trts.json")
}
