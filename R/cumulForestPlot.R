#' @title cumulative Forest plot
#' @description
#' Using metafor rma.uni function to fit a meta-analytic multivariate/multilevel fixed- and random/mixed-effects model for the given dataset.
#' The model then is used as input for the metaviz viz_forest function to print a forrest plot
#' See Documentation of metafor and metaviz packages for details.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param measure
#' A character string indicating underlying summary measure.
#' @return
#' creates a cumulative forest plot
#' also creates a json file (imgHeight.json) that is used in a later api call to define the height of the plots
#' @author Robert Studtrucker
#' @export
cumulforest <- function(yi,vi,measure,d,effectName="Effect") {

  #load needed dependencies
  library('metafor')
  library("ggplot2")
  library("metaviz")
  library('jsonlite')

  #load the in variable d defined dataset from the package
  dat <- tryCatch(
    {get(d)},
    error=function(cond) {
      message("This dataset does not exist")
      message("Here's the original error message:")
      message(cond)
      return(NULL)
    },
    warning=function(cond) {
      message("input caused a warning:")
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
  )

  #order the loaded data depending on the r_year column
  dat <- dat[order(dat$r_year),]

  # depending on the given measure the input for rma.uni model is z transformed
  if(measure == "COR") {

    #fitting the rma.uni model based on z transformed data

    rma_model <- metafor::rma.uni(yi=transf.rtoz(dat[,yi],dat[,o_ni]), vi=transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",slab=paste(dat$r_author, dat$r_year))

    tmp<-metafor::cumul(rma_model, order=order(dat$r_year))
    #creating a cumulative forest plot based on the fitted rma.uni model


    #fp <- metaviz::viz_forest(x = rma_model,
    #                 variant = "classic",
    #                 study_labels = rma_model$slab,
    #                 text_size =4,
    #                 xlab = effectName,
    #                 annotate_CI = TRUE,
    #                 x_trans_function = tanh,
    #                 type = "cumulative")

    fp<- metafor::forest(x=tmp,
                        cex=0.75,
                         xlab = "Correlation Coefficient",
                         study_labels = tmp$slab,
                         transf="ztor",
                         efac=0.2
                        )
  }else{
    # 1. Overall-Effekt und Cumulative forest ####

    #fitting the rma.uni model
    rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,slab=paste(dat$r_author, dat$r_year))

    #tmp<-cumul(rma_model, order=order(dat$r_year))

    #creating a cumulative forest plot based on the fitted rma.uni model
    fp <- viz_forest(x = rma_model,
                     variant = "classic",
                     study_labels = rma_model$slab,
                     text_size =4,
                     xlab = effectName,
                     annotate_CI = TRUE,
                     type = "cumulative")
    }

  # creating a json object with information about the height of the plot
  # this information is needed by the web service to define how big the requested image has to be.
  # If not specified standard height and wde is used wich may cause deformed plots when there are a lot of rows in the plot.
  height<-list("height" = length(rma_model$yi))
  write_json(height, "imgHeight.json")

  # print the cumul forest plot so the corresponding object can be retrieved by the web service
  print(fp)
  invisible();
}
