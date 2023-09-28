#' @title check data
#' @description
#' Helper function to check if the given dataset exists
#' @param d
#' A \code{string} representing the dataset name that should be used.

checkData <- function(d) {

  dat <- tryCatch(
    {get(d)},
    error=function(cond) {
      message(paste("This dataset does not exist:", d))
      message("Here's the original error message:")
      message(cond)
      stop("stopped code because of error")
    },
    warning=function(cond) {
      message(paste("input caused a warning:", d))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      stop("stopped code because of error")
    }
  )
  return(dat)
}
