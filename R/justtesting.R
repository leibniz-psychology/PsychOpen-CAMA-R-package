justtesting <- function(yi,vi,measure,d,effect="Effect") {

  requireNamespace("metafor")
  requireNamespace("ggplot2")
  requireNamespace("metaviz")
  requireNamespace("jsonlite")
  requireNamespace("psych")

  #load the in variable d defined dataset from the package
  dat <- d

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
