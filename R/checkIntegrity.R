#' @title check integrity
#' @description
#' Helper function to check if the
#' @export
#' @param d
#' A \code{string} representing the dataset name that should be used.
checkIntegrity <- function() {
  datasetList<-data(package='psychOpenCama')$results[,"Item"]
  tablesThatShouldExist<-c("r_year","r_peer","r_author")

  for (datasetName in datasetList) {
    dat<-checkData(datasetName)
    print(datasetName)
    checkParameter(dat,tablesThatShouldExist)

  }


}

