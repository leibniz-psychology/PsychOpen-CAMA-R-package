#' @title  netmeta forest plot
#' @description
#' Using the R package netmeta to to create and and return a forest plot for the given model and reference category
#' This indirect calling is needed since calling functions other than those from psychOpenCama package is blocked on the opencpu server for security reasons.
#' @param model
#' A netmeta model object
#' @param reference
#' A \code{string} representing the reference category that should be used. Here the reference is just needed for the ploting function.
#' @return returns a forest plot for a given netmeta model.
#' @author Robert Studtrucker
#' @export
netMetaForestplot <- function(model,reference) {
  netmeta::forest.netmeta(model, ref = reference, sortvar = TE,col.square = "#0097c6")
}
