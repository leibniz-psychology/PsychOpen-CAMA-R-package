#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

forest <- function(yi,vi,measure,d,effect="Effect") {

  library('metafor')
  library("ggplot2")
  library("metaviz")
  library('jsonlite')
  library('psych')

  #par(mar=c(1,0,1,0))
  dat<-get(d)


  if(measure == "COR") {


    rma_model <- rma.uni(yi=transf.rtoz(dat[,yi],dat[,o_ni]), vi=transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",slab=paste(dat$r_author, dat$r_year))


    fp <- viz_forest(x = rma_model,
                     variant = "classic",
                     study_labels = rma_model$slab,
                     text_size =4,
                     xlab = effect,
                     annotate_CI = TRUE,
                     method = "REML",
                     x_trans_function = tanh,
                     type = "standard")

    rainfp <-viz_forest(x = rma_model,
                        variant = "rain",
                        study_labels = rma_model$slab,
                        text_size =4,
                        xlab = effect,
                        annotate_CI = TRUE,
                        method = "REML",
                        x_trans_function = tanh,
                        type = "standard")


    height<-list("height" = length(rma_model$yi))
    write_json(height, "imgHeight.json")

    print(fp)
    print(rainfp)

    invisible();


  }else{
    rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,slab=paste(dat$r_author, dat$r_year))


    fp <- viz_forest(x = rma_model,
                     variant = "classic",
                     study_labels = rma_model$slab,
                     text_size =4,
                     xlab = effect,
                     annotate_CI = TRUE,
                     type = "standard")

    rainfp <-viz_forest(x = rma_model,
                        variant = "rain",
                        study_labels = rma_model$slab,
                        text_size =4,
                        xlab = effect,
                        annotate_CI = TRUE,
                        type = "standard")

    height<-list("height" = length(rma_model$yi))
    write_json(height, "imgHeight.json")

    print(fp)
    print(rainfp)

    invisible();

  }
  }

