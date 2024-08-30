# Load required package
library(jsonlite)

# Define the function
createData <- function(json_string) {
  # Trim any extra whitespace
  json_string <- trimws(json_string)

  # Print json_string for debugging (optional)
  print(json_string)

  # Parse the JSON string into an R list
  parsed_data <- jsonlite::fromJSON(json_string)

  # Convert the list to a data frame
  dataset <- as.data.frame(parsed_data)

  # Return the data frame
  return(dataset)
}
