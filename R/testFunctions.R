library(jsonlite)

testListParameter <- function(nesting = NULL) {
  # Parse the JSON input string
  nesting <- fromJSON(nesting)

  # Initialize an empty list to store the formulas
  nest <- list()

  # Iterate over the parsed JSON array and create formulas
  for (i in seq_along(nesting)) {
    # Create a formula and assign it to the nest list
    nest[[i]] <- as.formula(paste("~1 |", nesting[[i]]))
  }

  # Print the resulting nest list
  print(nest)

  # Original nest2 for comparison
  nest2 <- list(~1 | outcome_ID, ~1 | sample_ID, ~1 | report_ID)
  print(nest2)
}


testpredParameter<-function(pred1){


  # Parse the JSON string into a list
  pred1<-jsonlite::fromJSON(pred1)

  # Convert the list to a vector
  #pred1 <- unlist(pred1, use.names = TRUE)

    print(pred1)
  pred1["value"]
}
