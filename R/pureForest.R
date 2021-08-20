#' @title Pure Forest PLot
#' @description
#' Using metafor package to create a forest plot and metaviz package to create a rain forest plot.
#' @param rma_model
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @return
#' creates a forest and rainforest plot
#' also creates a json file (imgHeight.json) that is used in a later api call to define the height of the plots
#' @author Robert Studtrucker
#' @export
pureForest <- function(rma_model) {

  library("metaviz")
  library('jsonlite')
  library('psych')

fp <- viz_forest(x = rma_model,
                 variant = "classic",
                 study_labels = rma_model$slab,
                 text_size =4,
                 xlab = "Effect",
                 annotate_CI = TRUE,
                 method = "REML",
                 x_trans_function = tanh,
                 type = "standard")

rainfp <-viz_forest(x = rma_model,
                    variant = "rain",
                    study_labels = rma_model$slab,
                    text_size =4,
                    xlab = "Effect",
                    annotate_CI = TRUE,
                    method = "REML",
                    x_trans_function = tanh,
                    type = "standard")


height<-list("height" = length(rma_model$yi))
write_json(height, "imgHeight.json")

print(fp)
print(rainfp)

invisible();
}
