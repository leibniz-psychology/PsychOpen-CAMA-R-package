pcurves<-function(yi,vi,measure,d,effectName="Effect") {

library(metafor)
library(dmetar)
library(meta)

dat<-get(d)

# meta-Model
overall.meta <- metagen(TE=dat[,yi], seTE=sqrt(dat[,vi]),data = dat, studlab = paste(r_author),
                        comb.fixed = FALSE,comb.random = TRUE,method.tau = "SJ",
                        hakn = TRUE,prediction = TRUE,sm = measure)
# p-curve
pcurve(overall.meta)
}
