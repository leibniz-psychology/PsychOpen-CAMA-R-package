#' @title Forest plot
#' @description
#' Using metafor package to create a forest plot and metaviz package to create a rain forest plot.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param effect
#' A \code{string} representing the effect name that should be printed as label. defaults to "Effect"
#' @param measure
#' A character string indicating underlying summary measure.
#' @return
#' creates a forest and rainforest plot
#' also creates a json file (imgHeight.json) that is used in a later api call to define the height of the plots
#' @author Robert Studtrucker
#' @export
forest <- function(yi,vi,measure,d,effect="Effect") {

  requireNamespace("metafor")
  requireNamespace("ggplot2")
  requireNamespace("metaviz")
  requireNamespace("jsonlite")
  requireNamespace("psych")

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

  if(measure == "COR") {

    rma_model <- metafor::rma.uni(yi=metafor::transf.rtoz(dat[,yi],dat[,o_ni]), vi=metafor::transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",slab=paste(dat$r_author, dat$r_year))

    fp <- metaviz::viz_forest(x = rma_model,
                     variant = "classic",
                     study_labels = rma_model$slab,
                     text_size =4,
                     xlab = effect,
                     annotate_CI = TRUE,
                     method = "REML",
                     x_trans_function = tanh,
                     type = "standard")

    rainfp <-metaviz::viz_forest(x = rma_model,
                        variant = "rain",
                        study_labels = rma_model$slab,
                        text_size =4,
                        xlab = effect,
                        annotate_CI = TRUE,
                        method = "REML",
                        x_trans_function = tanh,
                        type = "standard")


    height<-list("height" = length(rma_model$yi))
    jsonlite::write_json(height, "imgHeight.json")

    print(fp)
    print(rainfp)

    invisible();

  }else{
    rma_model <- metafor::rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,slab=paste(dat$r_author, dat$r_year))

    fp <- metaviz::viz_forest(x = rma_model,
                     variant = "classic",
                     study_labels = rma_model$slab,
                     text_size =4,
                     xlab = effect,
                     annotate_CI = TRUE,
                     type = "standard")

    rainfp <- metaviz::viz_forest(x = rma_model,
                        variant = "rain",
                        study_labels = rma_model$slab,
                        text_size =4,
                        xlab = effect,
                        annotate_CI = TRUE,
                        type = "standard")

    height<-list("height" = length(rma_model$yi))
    jsonlite::write_json(height, "imgHeight.json")

    print(fp)
    print(rainfp)

    invisible();
  }
  }
