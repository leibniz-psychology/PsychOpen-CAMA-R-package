

cumulforest <- function(yi,vi,measure,d,effectName="Effect") {

  library('metafor')
  library("ggplot2")
  library("metaviz")
  library('jsonlite')


  dat<-get(d)
  dat <- dat[order(dat$r_year),]

  if(measure == "COR") {

    rma_model <- rma.uni(yi=transf.rtoz(dat[,yi],dat[,o_ni]), vi=transf.rtoz(dat[,vi],dat[,o_ni]),measure="ZCOR",slab=paste(dat$r_author, dat$r_year))


    fp <- viz_forest(x = rma_model,
                     variant = "classic",
                     study_labels = rma_model$slab,
                     text_size =4,
                     xlab = effectName,
                     annotate_CI = TRUE,
                     method = "REML",
                     x_trans_function = tanh,
                     type = "cumulative")

  }else{
    # 1. Overall-Effekt und Cumulative forest ####
    rma_model <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,slab=paste(dat$r_author, dat$r_year))


    fp <- viz_forest(x = rma_model,
                     variant = "classic",
                     study_labels = rma_model$slab,
                     text_size =4,
                     xlab = effectName,
                     annotate_CI = TRUE,
                     type = "cumulative")
  }



  height<-list("height" = length(rma_model$yi))
  write_json(height, "imgHeight.json")

  print(fp)

  invisible();

}




