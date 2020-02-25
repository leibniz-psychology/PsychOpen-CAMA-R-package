# Hello, world!
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

ForestPlot <- function() {

  data("CAMA_Math")

  library(metafor)

  # 1. Overall-Effekt und Cumulative forest ####
  overall_forest <- rma.uni(yi=o_g_calc, vi=o_g_var_calc, measure="SMD", slab=paste(r_author, r_year), data=Data)
  summary(overall_forest)
  tmp<-cumul(overall_forest, order=order(Data$r_year),slab=paste(r_author, r_year, sep=", "))

  forest(tmp, xlab="Response Rates", xlim=c(-1,1.8), cex=0.75)
  abline(v = mean(Data$o_g_calc),lty=3)

  print(plot)

  #return(true)
}
