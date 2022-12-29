#' @title JSON summary
#'
#' @description
#' produces a json summary of includes dataset specified by parameter [d].
#' @param d
#' A \code{string} representing the dataset name which json representation should be returned.
#' @return
#' returns a json representation of an included dataset
#' @author
#' Robert Studtrucker
#' @export
#' @examples

jsonSummary <- function(d) {
  requireNamespace("jsonlite")

  #load the in variable d defined dataset from the package
  dat <- checkData(d)

  val <-jsonlite::toJSON(lapply(dat, function(x){as.list(summary(x))}), pretty = TRUE, auto_unbox = TRUE)
  return(val)

}
