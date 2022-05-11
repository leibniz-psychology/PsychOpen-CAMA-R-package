#' @title  plot network graph
#' @description
#' Using the R package netmeta to to create and and return a network graph.
#' This indirect calling is needed since calling functions other than those from psychOpenCama package is blocked on the opencpu server for security reasons.
#' @param model
#' A netmeta model object
#' @param d
#' A \code{string} representing the dataset name that is used to extract the sequence and treatment parameter to be used of the netmeta::netgraph function
#' @return returns a network graph for a given netmeta model.
#' @author Robert Studtrucker
#' @export
netMetaNetgraph <- function(model,d) {

  #load needed dependencies
  requireNamespace("netmeta")


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

  #TE <- dat[,"TE"]
  #seTE <- dat[,"seTE"]
  #studlab<-dat[,"studlab"]
  #treat1<-dat[,"treat1"]
  #treat2<-dat[,"treat2"]

  combined_treat<-c()
  combined_treat<-c(combined_treat,dat["treat1"],recursive = TRUE,use.names=FALSE)
  combined_treat<-c(combined_treat,dat["treat2"],recursive = TRUE,use.names=FALSE)

  trts<-unique(combined_treat)[!is.na(unique(combined_treat))]

  gr<-netmeta::netgraph(model, col="#0000CC", col.points="white", seq = trts)
  print(gr)
 # gc() # Force R to release memory it is no longer using

}
