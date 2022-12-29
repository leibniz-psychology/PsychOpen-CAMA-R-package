#' @title check parameter
#' @description
#' Helper function to check if the given dataset has the matching list of parameters
#' @param d
#' A \code{string} representing the dataset name that should be used.
checkParameter <- function(d,paramList) {
  if(!all(paramList %in% names(d))){
    stop("stopped code because of one or more parameters did not exist in the dataset")
  }
}
