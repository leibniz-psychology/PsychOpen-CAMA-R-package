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


  library('metafor')
  library('dplyr')
  library("ggplot2")
  library("metaviz")
  library('jsonlite')

  #par(mar=c(1,0,1,0))
  dat<-get(d)


  # 1. Overall-Effekt und Cumulative forest ####
  overall_forest <- rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure,slab=paste(r_author, r_year), data=dat)

  tmp<-cumul(overall_forest, order=order(dat$r_year))


 # metafor::forest(tmp,xlab="Response Rates", xlim=c(-1,1.8), cex=1)
 # abline(v = mean(dat$o_g_calc),lty=3,col=2)


  short_cite <- tmp$slab
  cil<-tmp$ci.lb
  cih<-tmp$ci.ub
  estimate<-tmp$estimate
  fplot.data <- data.frame(short_cite,estimate,cil,cih)

  fplot.data<-arrange(fplot.data,-row_number())
  fplot.data$short_cite<-factor(fplot.data$short_cite, levels = fplot.data$short_cite)

  #fp <- ggplot(data=fplot.data, aes(x=short_cite, y=estimate, ymin=cil, ymax=cih)) +
   #geom_pointrange(position=position_dodge2(width = 0.5, padding = 3.5)) +
    #geom_hline(yintercept=mean(dat[,yi]), lty=2) +  # add a dotted line at x=1 after flip
    #coord_flip() +  # flip coordinates (puts labels on y axis)
    #xlab("") +
    #ylab(measure) +
    #theme_bw()  # use a white background
  #scale_x_discrete(limits=short_cite)

  fp <- viz_forest(x = data.frame("estimate"=tmp$estimate,"se"=tmp$se),
             variant = "classic",
             study_labels = tmp$slab,
             text_size =4,
             annotate_CI = TRUE,
             type = "cumulative")

  height<-list("height" = length(tmp$estimate))
  write_json(height, "imgHeight.json")

  print(fp)

  invisible();

  #yi=o_g_calc,vi=o_g_var_calc,measure="SMD",slab=paste(r_author, r_year),xlab="Response Rates",xlim=c(-1,1.8),cex=1, data="CAMA_Math"
  #ForestPlot(o_g_calc,o_g_var_calc,"SMD",paste(r_author, r_year),"Response Rates",c(-1,1.8),1,'CAMA_Math')

}




