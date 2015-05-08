paste("Jason Rich, Test #2", date())

setwd("/Users/jasonrich/Desktop/temp_r_repo")
getwd()

#####################################################################
# System set-up and package install
#####################################################################


chooseCRANmirror() ## server mirror I am using 86 (Berkley California)

pkgs<- install.packages()[,1]
pkgs.need<- c("car","MASS", "caret", "ggplot2", "leaps", "segmented", "lattice", "lme4")
pkgs.missing<- pkgs.need[!pkgs.need %in% pkgs] 
if (length (pkgs.missing)>0) {
  install.packages(pkgs.missing, dep = TRUE)
}
library(MASS)
library(caret)
library(ggplot2)
library(car)
library(leaps)
library(segmented)
library(lattice)
library(lme4)
library(stats)
#####################################################################


data<- read.table("agent.txt", sep=" ", header = TRUE, fill = FALSE, strip.white=TRUE)
data
summary(data)
attach(data)
agent = factor(data$agent)
dataFrame<- data.frame(agent, obs, y); 
dataFrame
str(dataFrame)

detach(data)

attach(dataFrame)



#####################################################################

#refer to the test papers for model, assumptions, and goals
# 1a.)


meanValue<- tapply(y, agent, mean); 
meanValue
# agent:  1     2     5 
# 		24.55 22.55 30.10 
meanSum = sum(meanValue); 
meanSum 
#77.2

grandMean<- mean(y);
grandMean
  #25.73333


dataFrame.fit<- lm(formula = y ~ agent, data = dataFrame) ## linear  regression
dataFrame.fit ## call
summary(dataFrame.fit) 

  ##ANOVA table
dataFrame.anova<- aov(dataFrame.fit)
dataFrame.anova
model.anova=anova(dataFrame.anova)
summary(dataFrame.anova)
#With F value 37.3  p < 0.01 (4.41e-11), we reject the null hypothesis, and say that there is evidence to support the claim
#that agent selection does not affect the mean premium distribution of products.


# 1b.)
#???


#1c.) 
lsContrast<- c(1/2,1/2,-1); 
lsContrast

lsContrastsq<- sum(lsContrast^2);
lsContrastsq

lsMeancontrast<- sum(lsContrast * meanValue);
lsMeancontrast #-6.55

tStat = lsMeancontrast/sqrt(8.21 /3*lsContrastsq); 
tStat #-3.233
criticalT = -qt(p=0.025,df=57);
criticalT #2.0025

tStat; criticalT;  #-3.232844 < 2.002465
# there is enough tatistical evidence to support the claim that the average agent premium distribution for agents agents 1 and 2  is different than
# the average premium distribution of agent 5. 


#1.c)

#l-one contrast coefficients
coeffOne = c(1,0,-1); 
coeffOne

coeffTwo =sum(coeffOne^2)
coeffTwo

l_One = sum(coeffOne * meanValue);
l_One
#-5.55

lsMeancontrast
# -6.55


#95% confidence intervals L.1
ninetyFivepent<- c(l_One-criticalT* sqrt(8.21/3*lsContrastsq), 
				  l_One+criticalT*sqrt(8.21/3*lsContrastsq));
ninetyFivepent
#(-9.607155 -1.492845)

#95% confidence intervals L.2
ninetyFivepentTwo<- c(lsMeancontrast-criticalT* sqrt(8.21/3*lsContrastsq), 
          lsMeancontrast+criticalT*sqrt(8.21/3*lsContrastsq));
ninetyFivepentTwo
#(-10.607155  -2.492845)



#L.1 = MU.1- MU.5
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


model.means <- tapply(dataFrame$y,data$agent,mean)
modelMeans2<- sum(model.means*coeffOne)
model.len <- tapply(dataFrame$y,data$agent,length); model.len
dfMSE=model.anova$Df
dfMSE2=dfMSE[2]
MSE1=model.anova$"Mean Sq"
MSE2=MSE1[2]
bonferroniCI(modelMeans2, model.len, dfMSE2, MSE2, conf=.95)
scheffeCI(modelMeans2, model.len, dfMSE2, MSE2, conf=.95)


#L.2 = ((MU.1 + MU.2)/2) - MU.5 
all.pairs <- function(r) {
  list(first = rep(1:r,rep(r,r))[lower.tri(diag(r))]
       ,second = rep(1:r, r)[lower.tri(diag(r))]
       ,third = rep(1:r, r)[lower.tri(diag(r))])
}

bonferroniCI2 <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- (fitted[pairs$first] + fitted[pairs$second])/2 - fitted[pairs$third] #adjusted for contrast
  T <- qt(1-(1-conf.level)/(2*r*(r-1)),df)
  hwidths <-  T*sqrt(MSE*(coeffTwo/nis[pairs$first] + coeffTwo/nis[pairs$second]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  #dimnames(val) <- list(paste(("mu",pairs$first,"+ mu",pairs$second)/2 "- mu",pairs$third,
   #                                 sep=""), c("Lower", "Diff","Upper"))
  val
}

scheffeCI2 <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- (fitted[pairs$first] + fitted[pairs$second])/2 - fitted[pairs$third] #adjusted for contrast
  T <- sqrt((r-1)*qf(conf.level,r-1,df))
  hwidths <-  T*sqrt(MSE*(coeffTwo/nis[pairs$first] + coeffTwo/nis[pairs$second]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  #dimnames(val) <- list(paste(("mu", pairs$first," + mu", pairs$second)/2 " - mu", pairs$third,
  #                                  sep=""), c("Lower", "Diff","Upper"))
  val
}


model.means2 <- tapply(dataFrame$y,data$agent,mean)
model.len2 <- tapply(dataFrame$y,data$agent,length); model.len
dfMSE=model.anova$Df
dfMSE2=dfMSE[2]
MSE1=model.anova$"Mean Sq"
MSE2=MSE1[2]
bonferroniCI2(model.means2, model.len2, dfMSE2, MSE2, conf=.95)
scheffeCI2(model.means2, model.len2, dfMSE2, MSE2, conf=.95)

#1.d)
# meaningful contrasts
choose(3,2)*choose(1,1)
#3

#####################################################################
#2

#2a.) 
agentLmer<- lmer(y~1+(1|agent), data=dataFrame);
agentLmer
summary(agentLmer)

means<- sapply(split(y,agent), mean);
means
grandMeans2<- mean(y);
grandMeans2
agentFit<- sapply(split(fitted(agentLmer),agent),mean);
agentFit
agentDataframe<- data.frame(mean=means, fitted=agentFit);
agentDataframe

msTreat<- (60*14.891) + 8.205;
msTreat


#2b.)
propVar<- 14.891/(14.891+8.205);
propVar*100


agentOne = 24.55
agentTwo = 22.55
agentMeandiff<- (agentOne - agentTwo);
agentMeandiff


#2c.)
diffConinterval<- c(agentMeandiff-criticalT* sqrt((2*8.21)/3), 
          agentMeandiff+criticalT*sqrt((2*8.21)/3));
diffConinterval
#( -2.684799  6.684799)

#####################################################################
