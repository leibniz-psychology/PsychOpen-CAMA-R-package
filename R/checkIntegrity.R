#' @title check integrity
#' @description
#' Helper function to check if the
#' @export
#' @param d
#' A \code{string} representing the dataset name that should be used.
# Load the required library
library(jsonlite)



checkIntegrity <- function() {
  datasetList<-data(package='psychOpenCama')$results[,"Item"]

  tablesThatShouldExist<-c("r_year","r_peer","r_author")
  metadata_folder <- "metadata"


  for (datasetName in datasetList) {
    dat<-psychOpenCama::checkData(datasetName)
    print(datasetName)
    checkParameter(dat,tablesThatShouldExist)


    jsonFileName <- paste0("Datensatz_", datasetName,".json")
    #print(jsonFileName);

    # Define the path to the JSON file within your package
    json_file <- system.file("metadata", jsonFileName, package = "psychOpenCama")

    rel_path_from_root <- file.path("metadata", jsonFileName)

    if (file.exists(rel_path_from_root)) {
      # Read and parse the JSON file
      json_data <- jsonlite::fromJSON(rel_path_from_root)

      # Print the JSON data (for demonstration purposes)
      #print(json_data)
    } else {
      #cat("JSON file not found in the 'metadata' subfolder.\n")
    }
  }


}

