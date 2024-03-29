#' @title meta power plot
#' @description
#' creates a metapower plot.
#' Not used at the moment
#' @param d
#' A \code{string} representing the dataset name.
#' @param yi
#' A \code{string} of the variable which holds the vector of length k with the observed effect sizes or outcomes in the selected dataset (d)
#' @param vi
#' A \code{string} of the variable which holds the vector of length k with the corresponding sampling variances in the selected dataset (d)
#' @param measure
#' A character string indicating underlying summary measure
#' @param pval
#' A \code{Numeric} representing the significance level which should be used for the power simulation.
#' @return
#' A metapower plot
#' @author Robert Studtrucker
#' @export
metapower <- function(yi,vi,measure,d, pval=0.05) {

  requireNamespace("metafor")
  requireNamespace("meta")
  requireNamespace("pwr")
  requireNamespace("dmetar")
  requireNamespace("metapower")
  requireNamespace("ggplot2")


   #load the in variable d defined dataset from the package
  dat <- checkData(d)
  checkParameter(dat,c(yi,vi))

  uni <- metafor::rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)

   n1<-mean(dat$o_n1i)
   n2<-mean(dat$o_n2i)
   d_ma<-uni$beta[[1]]
   k<-uni$k

   odds = FALSE

   if (missing(d_ma)) {
     stop("'d_ma' must be provided.")
   }

   # Loop for data for plot
   dvec = (1:1000)/1000

   if (d_ma > 1) {
     dvec = (1:(d_ma * 1000))/1000
   }

   powvect.l = vector()  ## Heterogeneity low, het.factor: 1.33

   for (i in 1:length(dvec)) {
     d = dvec[i]
     v.m = 1.33 * (((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2))))/k
     lambda = (d/sqrt(v.m))
     zval = qnorm(p = 1 - (pval/2), 0, 1)
     powvect.l[i] = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
   }

   powvect.m = vector()   ## Heterogeneity moderate, het.factor: 1.67

   for (i in 1:length(dvec)) {
     d = dvec[i]
     v.m = 2 * (((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2))))/k
     lambda = (d/sqrt(v.m))
     zval = qnorm(p = 1 - (pval/2), 0, 1)
     powvect.m[i] = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
   }

   powvect.h = vector()   ## Heterogeneity high, het.factor: 2

   for (i in 1:length(dvec)) {
     d = dvec[i]
     v.m = 3 * (((n1 + n2)/(n1 * n2)) + ((d * d)/(2 * (n1 + n2))))/k
     lambda = (d/sqrt(v.m))
     zval = qnorm(p = 1 - (pval/2), 0, 1)
     powvect.h[i] = 1 - (pnorm(zval - lambda)) + (pnorm(-zval - lambda))
   }

   power <- powvect.m[dvec==round(d_ma,3)]

   # Generate plot

   plotdat = rbind(
     as.data.frame(cbind(dvec=as.numeric(dvec), powvect=powvect.l,hetvec=rep(1,1000))),
     as.data.frame(cbind(dvec=as.numeric(dvec), powvect=powvect.m,hetvec=rep(2,1000))),
     as.data.frame(cbind(dvec=as.numeric(dvec), powvect=powvect.h,hetvec=rep(3,1000))))

   ggplot2::ggplot(data = plotdat, ggplot2::aes(x = dvec, y = powvect, group=factor(hetvec), color=factor(hetvec))) +
     ggplot2::geom_line(size=1.2) +
      ggplot2::geom_point(ggplot2::aes(x = d_ma, y = power), color = "black", size = 3) +
      ggplot2::theme_minimal() +
     ggplot2::scale_color_discrete(name="Heterogeneity",labels=c("Low", "Moderate", "High")) +
      ggplot2::geom_hline(yintercept = 0.8,color = "grey", linetype = "dotdash") +
      ggplot2::geom_vline(xintercept=d_ma, color = "black", linetype = "longdash") +
      ggplot2::geom_hline(yintercept=power, color = "black", linetype = "longdash") +
      ggplot2::labs(y="Power", x="Effect size (SMD)")
}
