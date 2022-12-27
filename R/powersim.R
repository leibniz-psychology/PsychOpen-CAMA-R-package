#' @title Power simulation
#' @description
#' Using the R packages metafor and pwr to calculate a powersimulation for a given dataset and specified parameters. The results are plotted using ggplot2.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param d
#' A \code{string} representing the dataset name that should be used for fitting.
#' @param measure
#' A character string indicating underlying summary measure. Depending on the type of measure the input data for fitting needs to be transformed and backtransformed later.
#' @param n
#' A \code{Integer} representing the sample size which should be used for the power simulation.
#' @param pval
#' A \code{Numeric} representing the significance level which should be used for the power simulation.
#' @return returns a power plot
#' @author Robert Studtrucker
#' @export
powersim <- function(yi,vi,measure,d,n,pval=0.05) {

  requireNamespace("metafor")
  requireNamespace("pwr")
  requireNamespace("ggplot2")
  requireNamespace("jsonlite")

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

  uni<-metafor::rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
  lb<-uni$ci.lb
  estimate<-uni$beta[[1]]
  ub<-uni$ci.ub
  nvec = 1:200

  power.lb = vector()  ## Lower bound estimate
  for (i in 1:length(nvec)) {power.lb[i] <- pwr::pwr.t.test(nvec[i],d=lb,sig.level=pval)$power}

  power.est = vector()   ## Estimate

  for (i in 1:length(nvec)) {power.est[i] <- pwr::pwr.t.test(nvec[i],d=estimate,sig.level=pval)$power}

  power.ub = vector()   ## Upper bound estimate
  for (i in 1:length(nvec)) {power.ub[i] <- pwr::pwr.t.test(nvec[i],ub,sig.level=pval)$power}

  power <- power.est[nvec==n]

  #Wird nur für den Ausgabetext benötigt
  ID=c("power","Samplesize")
  VALUE=c(round(power*100,1),round(pwr::pwr.t.test(d=estimate,sig.level=pval, power=0.8)$n,0))

  df <- data.frame(ID, VALUE)
  print(df)
  jsonlite::write_json(df, "output.json")



  # Generate plot
  plotdat = rbind(
    as.data.frame(cbind(nvec=as.numeric(nvec), powvect=power.lb,estvec=rep(1,200))),
    as.data.frame(cbind(nvec=as.numeric(nvec), powvect=power.est,estvec=rep(2,200))),
    as.data.frame(cbind(nvec=as.numeric(nvec), powvect=power.ub,estvec=rep(3,200))))

  text.estimate<-paste("Estimate: ",round(estimate,2))
  text.lowerbound<-paste("Lo bound: ",round(lb,2))
  text.upperbound<-paste("Up bound: ",round(ub,2))


  if(pwr::pwr.t.test(d=estimate,sig.level=pval, power=0.8)<300){
    ggplot2::ggplot(data = plotdat, ggplot2::aes(x = nvec, y = powvect, group=factor(estvec), color=factor(estvec))) +
      ggplot2::geom_line(size=1.2) +
      ggplot2::geom_point(ggplot2::aes(x = n, y = power), color = "black", size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_discrete(name="Assumed effect",labels=c(text.lowerbound,text.estimate,text.upperbound)) +
      ggplot2::theme(legend.position="bottom") +
      ggplot2::geom_hline(yintercept = 0.8,color = "grey", linetype = "dotdash") +
      ggplot2::geom_vline(xintercept = pwr::pwr.t.test(d=estimate,sig.level=pval, power=0.8)$n,color = "grey", linetype = "dotdash") +
      ggplot2::geom_vline(xintercept=n, color = "black", linetype = "longdash") +
      ggplot2::geom_hline(yintercept=power, color = "black", linetype = "longdash") +
      ggplot2::labs(y="Power", x="Sample size")
  }else{
    ggplot2::ggplot(data = plotdat, ggplot2::aes(x = nvec, y = powvect, group=factor(estvec), color=factor(estvec))) +
      ggplot2::geom_line(size=1.2) +
      ggplot2::geom_point(ggplot2::aes(x = n, y = power), color = "black", size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_discrete(name="Assumed effect",labels=c(text.lowerbound,text.estimate,text.upperbound)) +
      ggplot2::theme(legend.position="bottom") +
      ggplot2::geom_vline(xintercept=n, color = "black", linetype = "longdash") +
      ggplot2::geom_hline(yintercept=power, color = "black", linetype = "longdash") +
      ggplot2::labs(y="Power", x="Sample size")
  }
}
