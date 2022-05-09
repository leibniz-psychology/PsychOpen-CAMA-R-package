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

  val <-toJSON(lapply(dat, function(x){as.list(summary(x))}), pretty = TRUE, auto_unbox = TRUE)
  return(val)

}
