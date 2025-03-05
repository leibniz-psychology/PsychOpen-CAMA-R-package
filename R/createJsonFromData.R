# Load required package
library(jsonlite)

# Define the function
createJsonFromData <- function(d) {
  dat <- checkData(d)

  json_data <- toJSON(dat, pretty = TRUE, digits = NA)

  write(json_data, paste0(d, ".json"))

  }
