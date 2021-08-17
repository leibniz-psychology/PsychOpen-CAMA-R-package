
#' @export
pureForest <- function(rma_model) {

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
