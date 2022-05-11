#' @title  Matrix comparison
#' @description
#' Using the R package netmeta to create a matrix of all pairwise comparison
#' This indirect calling is needed since calling functions other than those from psychOpenCama package is blocked on the opencpu server for security reasons.
#' @param model
#' A netmeta model object
#' @return returns a matrix of all pairwise comparison for the given netmeta model.
#' @author Robert Studtrucker
#' @export
netMetaMatrix <- function(model) {

  #load needed dependencies
  requireNamespace("netmeta")


  # Output 2: Matrix of all pairwise comparisons
  res.matrix<-round(model$TE.fixed,3)
  res.matrix[lower.tri(res.matrix, diag=FALSE)] <- "."
  print(res.matrix)
  }
