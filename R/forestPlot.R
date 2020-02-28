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

cumulforest <- function(yi,vi,measure,d) {

  library(metafor)

  #par(mar=c(1,0,1,0))
  dat<-get(d)


  # 1. Overall-Effekt und Cumulative forest ####
  overall_forest <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,slab=paste(r_author, r_year), data=dat)

  tmp<-cumul(overall_forest, order=order(dat$r_year),slab=paste(r_author, r_year))


  metafor::forest(tmp,xlab="Response Rates", xlim=c(-1,1.8), cex=1)
  abline(v = mean(dat$o_g_calc),lty=3,col=2)

  invisible();

  #yi=o_g_calc,vi=o_g_var_calc,measure="SMD",slab=paste(r_author, r_year),xlab="Response Rates",xlim=c(-1,1.8),cex=1, data="CAMA_Math"
  #ForestPlot(o_g_calc,o_g_var_calc,"SMD",paste(r_author, r_year),"Response Rates",c(-1,1.8),1,'CAMA_Math')

}




