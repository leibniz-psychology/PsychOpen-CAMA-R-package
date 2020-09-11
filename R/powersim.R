powersim <- function(yi,vi,measure,d,n,pval=0.05) {
  library(metafor)
  library(meta)
  library(pwr)
  #library(dmetar)
  library(metapower)
  library(ggplot2)

  dat<-get(d)

    uni<-rma.uni(yi=dat[,yi],vi=dat[,vi],measure=measure)
    lb<-uni$ci.lb
    estimate<-uni$beta[[1]]
    ub<-uni$ci.ub

    nvec = 1:200

    power.lb = vector()  ## Lower bound estimate
    for (i in 1:length(nvec)) {power.lb[i] <- pwr.t.test(nvec[i],d=lb,sig.level=pval)$power}

    power.est = vector()   ## Estimate
    for (i in 1:length(nvec)) {power.est[i] <- pwr.t.test(nvec[i],d=estimate,sig.level=pval)$power}

    power.ub = vector()   ## Upper bound estimate
    for (i in 1:length(nvec)) {power.ub[i] <- pwr.t.test(nvec[i],ub,sig.level=pval)$power}

    power <- power.est[nvec==n]

    # Generate plot

    plotdat = rbind(
      as.data.frame(cbind(nvec=as.numeric(nvec), powvect=power.lb,estvec=rep(1,200))),
      as.data.frame(cbind(nvec=as.numeric(nvec), powvect=power.est,estvec=rep(2,200))),
      as.data.frame(cbind(nvec=as.numeric(nvec), powvect=power.ub,estvec=rep(3,200))))

    ggplot(data = plotdat, aes(x = nvec, y = powvect, group=factor(estvec), color=factor(estvec))) +
      geom_line(size=1.2) +
      geom_point(aes(x = n, y = power), color = "black", size = 3) +
      theme_minimal() +
      scale_color_discrete(name="Assumed effect size",labels=c("Lower bound", "Estimate", "Upper bound")) +
      geom_hline(yintercept = 0.8,color = "grey", linetype = "dotdash") +
      geom_vline(xintercept = pwr.t.test(d=estimate,sig.level=pval, power=0.8)$n,color = "grey", linetype = "dotdash") +
      geom_vline(xintercept=n, color = "black", linetype = "longdash") +
      geom_hline(yintercept=power, color = "black", linetype = "longdash") +
      labs(y="Power", x="Sample size")
}
