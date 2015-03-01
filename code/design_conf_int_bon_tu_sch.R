data<-#read.table("F:/Stat 435-535/data.merch.txt", header=FALSE, col.names=c('agent', 'obs', 'y'))
data
attach(data)
data.fit<-lm(y~agent)
data.fit
# To make agent a factor,
agent<-factor(agent)

data.fit<-aov(y~agent)
data.fit
model.anova=anova(data.fit)

TukeyHSD(data.fit)
pairwise.t.test(y, wtp, p.adj="bonferroni")

all.pairs <- function(r) {
  list(first = rep(1:r,rep(r,r))[lower.tri(diag(r))],
       second = rep(1:r, r)[lower.tri(diag(r))])
}
bonferroniCI <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- fitted[pairs$first] - fitted[pairs$second]
  T <- qt(1-(1-conf.level)/(2*r*(r-1)),df)
  hwidths <-  T*sqrt(MSE*(1/nis[pairs$first] + 1/nis[pairs$second]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  dimnames(val) <- list(paste("mu",pairs$first," - mu", pairs$second,
                              sep=""), c("Lower", "Diff","Upper"))
  val
}

scheffeCI <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- fitted[pairs$first] - fitted[pairs$second]
  T <- sqrt((r-1)*qf(conf.level,r-1,df))
  hwidths <-  T*sqrt(MSE*(1/nis[pairs$first] + 1/nis[pairs$second]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  dimnames(val) <- list(paste("mu",pairs$first," - mu", pairs$second,
                              sep=""), c("Lower", "Diff","Upper"))
  val
}

model.means <- tapply(data$y,data$agent,mean)
model.len <- tapply(data$y,data$agent,length)
dfMSE=model.anova$Df
dfMSE2=dfMSE[2]
MSE1=model.anova$"Mean Sq"
MSE2=MSE1[2]
bonferroniCI(model.means, model.len, dfMSE2, MSE2, conf=.90)
scheffeCI(model.means, model.len, dfMSE2, MSE2, conf=.90)

pairwise.t.test(y,agent, p.adj= "bonferroni")
?pairwise.t.test