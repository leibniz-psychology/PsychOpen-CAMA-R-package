#' @title  Summary for netmeta model
#' @description
#' Using the R package netmeta to to create and return a summary for a given netmeta model.
#' This indirect calling is needed since calling functions other than those from psychOpenCama package is blocked on the opencpu server for security reasons.
#' @param model
#' A netmeta model object
#' @return returns a summary for a given netmeta model.
#' @author Robert Studtrucker
#' @export
netMetaSummary <- function(model) {
  library(netmeta)
  requireNamespace("netmeta")
    netmeta::summary.netmeta(model)
}
