#' @title Scatter Plot
#' @description
#' Produces a scatterplot depending of given dataset and optionally predictors.
#' @param d
#' A \code{string} representing the dataset name.
#' @param pred1
#' A \code{list} for first predictor containing entries for type and value.
#' @param pred2
#' A \code{list} for first predictor containing entries for type and value.
#' @return
#' returns a scatterplot for the given dataset and predictor variables.
#' @author Robert Studtrucker
#' @export
scatterPlot <- function(yi,d,pred1=NULL,pred2=NULL, effectName="Effect") {
  requireNamespace("metafor")
  requireNamespace("ggplot2")
  requireNamespace("GGally")
  library(rlang)

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

  pred1<-unlist(pred1)
  pred2<-unlist(pred2)

  if( !is.null(pred1) && !is.null(pred2)){

    #Es gibt zwei Prädiktoren
    if(pred1["type"]=="num" && pred2["type"]=="num"){

      # Scatter, 2 predictors,  num/num
      GGally::ggscatmat(dat, columns=c(yi,pred1["value"],pred2["value"])) +
        ggplot2::geom_point(colour="#34B4D8", shape=19) +
        ggplot2::labs(title="scatterplot matrix")

    }else if(pred1["type"]=="cat" && pred2["type"]=="cat"){
      # Violin, 2 predictors,   cat/cat

      ggplot2::ggplot(dat, ggplot2::aes(x=dat[,pred1["value"]], y=dat[,yi], color=dat[,pred2["value"]])) +
        ggplot2::geom_violin(draw_quantiles = c(0.2,0.4,0.6,0.8)) +
        ggplot2::geom_point(ggplot2::aes(x=dat[,pred1["value"]],color=dat[,pred2["value"]]),shape=19,size=2) +
        ggplot2::labs(title="Violin Plot",x=pred1["label"], y=effectName, color=pred2["label"]) +
        ggplot2::theme(legend.position = "right",plot.title = ggplot2::element_text(hjust = 0.5))
    }else {
      numerical<-""
      categorical<-""
      if(pred1["type"]=="cat"){
        categorical<-pred1
        numerical<-pred2
      }else{
        categorical<-pred2
        numerical<-pred1
      }
      # Scatter, 2 predictors,  num/cat
      ggplot2::ggplot(dat = dat, ggplot2::aes(x = dat[,numerical["value"]], y = dat[,yi])) +
        ggplot2::geom_point(ggplot2::aes(color = dat[,categorical["value"]]),shape=19, size=3) +
        ggplot2::theme(legend.position = "right",plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(title="Scatter Plot",x=numerical["label"], y=effectName, color=categorical["label"])
      }
    }
    else if(!is.null(pred1) && is.null(pred2)){
    #Es gibt einen Prädiktor

    if(pred1["type"]=="cat"){
      # Violin, 1 predictor,  cat

      ggplot2::ggplot(dat, ggplot2::aes(x=dat[,pred1["value"]], y=dat[,yi])) +
        ggplot2::geom_violin(draw_quantiles = c(0.2,0.4,0.6,0.8), color='#0480a3') +
        ggplot2::geom_point(size=2,color="#34b4d8") +
        ggplot2::labs(title="Violin Plot",x=pred1["label"], y=effectName) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    }else{
      # Scatter, 1 predictor,   num

      ggplot2::ggplot(dat = dat, ggplot2::aes(x = dat[,pred1["value"]], y = dat[,yi])) +
        ggplot2::geom_point(colour="#34B4D8", shape=19, size=3) +
        ggplot2::labs(title="Scatter Plot",x=pred1["label"], y=effectName) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }

  }else if(is.null(pred1)&&is.null(pred2)) {
    # Scatter, no predictor

    dat.cleared<- subset(x = dat, subset = !is.na(dat[,yi]))
    print(dat.cleared)


    ggplot2::ggplot(dat = dat, ggplot2::aes(x=seq_along(dat[,yi]), y=dat[,yi])) +
      ggplot2::geom_violin(draw_quantiles = c(0.2,0.4,0.6,0.8), color="#0480A3") +
      ggplot2::geom_point(size=2, color="#34B4D8") +
      ggplot2::labs(title="Violin Plot",x="", y=effectName) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
}



