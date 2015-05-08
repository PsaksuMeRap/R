#1 : Problem 3:10
#(a)
seq1=seq(from=15, to=35, by= 5)
w=rep(seq(from=15, to=35, by= 5), 5)
rep(seq1, times=5)
weight=rep(seq1, times=c(5, 5, 5, 5, 5))

weight=as.factor(weight)

# or another way to include the data is to consider the following
weight=factor(c(rep("15", 5),rep("20", 5),rep("25", 5),rep("30", 5),rep("35", 5)))
weight=rep(1:5, each=5)
weight=factor(c((rep(15, 5)),rep(20, 5),rep(25, 5),rep(30, 5),rep(35, 5)))
is.numeric(weight)
strength=c(7,7,15,11,9,12,17,12,18,18,14,19,19,18,18,19,25,22,19,23,7,10,11,15,11)
scores=data.frame(strength, weight)
model=aov(strength~weight, data=scores)
model
summary(model)

# or you can also consider the following
x1<-c(7,7,15,11,9)     
x2<-c(12,17,12,18,18)
x3<-c(14,19,19,18,18)
x4<-c(19,25,22,19,23)
x5<-c(7,10,11,15,11)
scores=data.frame(x1,x2,x3,x4,x5)
scores = stack(scores)
scores.lm<-lm(values~ind,data=scores)
scores.anova = anova(scores.lm)  ; scores.anova
#With F value 14.757, p<0.01, we reject the null hypothesis, and say that there is evidence to support the claim
#that cotton content affects the mean tensile strength.

#b
contrastcoeff=c(1/2,-1/3,-1/3,-1/3,1/2); contrastcoeff
contrastSquared = sum(contrastcoeff^2)  ; contrastSquared
meansum = c(mean(x1),mean(x2),mean(x3),mean(x4),mean(x5))  ; meansum 
tnum = sum(contrastcoeff*meansum)     ; tnum
tstat=tnum/sqrt(8.06/5*contrastSquared)  ; tstat
criticalT = -qt(p=0.025,df=20)   ; criticalT
tstat
criticalT
# We clearly reject the null here.  critical T value is 2.08, but we get a t-value of -6.82. Hence, we say that there is enough
# statistical evidence to support the claim that the average strength for cotton weight percentages of 15 and 20 is different than
# the average strength for cotton weight percentages of 25, 30, and 35.

#c - Confidence Interval is:
CI = c(tnum-criticalT* sqrt(8.06/5*contrastSquared), tnum+criticalT*sqrt(8.06/5*contrastSquared))
CI
# Result is (-10.17233, -5.62767)

#(d) Meaningful Contrasts
choose(5,2)+choose(5,1)*choose(4,2)+choose(5,1)*choose(4,3)+choose(5,1)*choose(4,4)+0.5*choose(5,2)*choose(3,2)+choose(5,2)*choose(3,3)
#The answer is 90

#(e) One possible maximal set of mutually orthagonal contrasts:
# C1 = m1 - m2
# C2 = m1+m2-2*m3
# C3 = m1+m2+m3-3*m4
# C4 = m1+m2+m3+m4-4*m5


# (f) so, 90 contrasts are possible with Scheffe's method.
# (g) By Tukey's method, we do pairwise comparison, giving us choose(5,2), or 10
# (h)
TukeyHSD(aov(values~ind, scores),conf.level=0.90)

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

model.means <- tapply(scores$values,scores$ind,mean)
model.len <- tapply(scores$values,scores$ind,length)
dfMSE=scores.anova$Df
dfMSE2=dfMSE[2]
MSE1=scores.anova$"Mean Sq"
MSE2=MSE1[2]
bonferroniCI(model.means, model.len, dfMSE2, MSE2, conf=.90)
scheffeCI(model.means, model.len, dfMSE2, MSE2, conf=.90)
#The shortest interval is given by Tukey's method (interval length of 9.49.)  The longest interval is given by
#Bonferroni - interval length of 11.32.
#(i)
#Use Tukey's method to determine confidence intervals for each of the pair-wise contrast means, since it gives the 
#smallest interval of possibilites, still at the 90% CI.

#2 - Problem 3.18
#a
x1<-c(143,141,150,146)
x2<-c(152,149,137,143)
x3<-c(134,136,132,127)
x4<-c(129,127,132,129)
scores=data.frame(x1,x2,x3,x4)
scores = stack(scores)
scores.lm<-lm(values~ind,data=scores)
scores.anova = anova(scores.lm)
scores.anova
#With F value 14.302, p<0.01, we reject the null hypothesis, and say that there is evidence to support the claim
#that difference in conductivity is due to coating type.

#b
#Overall mean estimate is 137.9375
grandmean<-(sum(x1)+sum(x2)+sum(x3)+sum(x4))/16
#Treatment effect for type1= 7.0625; type2 =7.3125; type3 = -5.6875; type4 = -8.6875
mean(x1)-grandmean
mean(x2)-grandmean
mean(x3)-grandmean
mean(x4)-grandmean

#c
#Confidence interval - coating 4 is (124.4159, 134.0841)
c(mean(x4)-(-qt(p=0.025,df=12))*sqrt(19.69/4),mean(x4)+(-qt(p=0.025,df=12))*sqrt(19.69/4))
# Confidence interval - mean difference of coatings 1 and 4 is (8.913595, 22.586405)
c(mean(x1)-mean(x4)-(-qt(p=0.025,df=12))*sqrt(2*19.69/4),mean(x1)-mean(x4)+(-qt(p=0.025,df=12))*sqrt(2*19.69/4))


#d Fisher tests
#cut value is 6.836405
-qt(p=0.025,df=12)*sqrt(2*19.69/4)
mean(x1)-mean(x2) # not significant - value of -0.25
mean(x1)-mean(x3) #significant - value of 12.75
mean(x1)-mean(x4) #significant - value of 15.75
mean(x2)-mean(x3) #significant - value of 13
mean(x2)-mean(x4) #significant - value of 16
mean(x3)-mean(x4) #not significant - value of 3

#e 

#f
#Based on Fisher tests and treatment effects, it appears that coating type 4 seems to the best to minimize conductivity.
# Based on Fisher tests, we see that coating 4 is significantly better in 2 out of the three comparisons.  We also see that type 4
# has the greatest treatment effect for reducing conductivity.

#3 - problem 3.21s
#(a)
x1<-c(80,83,83,85)
x2<-c(75,75,79,79)
x3<-c(74,73,76,77)
x4<-c(67,72,74,74)
x5<-c(62,62,67,69)
x6<-c(60,61,64,66)
scores=data.frame(x1,x2,x3,x4,x5,x6)
scores = stack(scores)
scores.lm<-lm(values~ind,data=scores)
scores.anova = anova(scores.lm)
scores.anova
# Reject the null hypothesis, and say that the size of the orifice affect the mean percentage of radon released.
#(b) - p<0.01, F = 30.852
#(c)
plot(scores.lm$fit,scores.lm$res)
#Based on this plot, there is no obvious patterns.  Therefore, there is no apparent violation of model assumptions.
#(d) CI 95% is (62.15211, 67.84789)
c(mean(x5)-(-qt(p=0.025,df=18))*sqrt(7.35/4),mean(x5)+(-qt(p=0.025,df=18))*sqrt(7.35/4))
#(e)  See comment in part e, #3.16 (problem #2.)  

#4 - Problem 3.28
x1<-c(110,157,194,178)
x2<-c(2,3,4,18)
x3<-c(880,1256,5276,4355)
x4<-c(495,7040,5307,10050)
x5<-c(7,5,29,2)
scores=data.frame(x1,x2,x3,x4,x5)
scores = stack(scores)
scores.lm<-lm(values~ind,data=scores)
scores.anova = anova(scores.lm)
scores.anova
#Based on this model, we reject the null hypothesis, given F = 6.1905 and p<0.01.  In other words,
#there is statistical evidence to suggest that all five materials do not have the same effect on 
#mean failure time.
#4-b
plot(scores.lm$fit,scores.lm$res)
qqnorm(scores.lm$residual)
#There are serious problems with these plots.  The qqnorm plot is definitely not linear, and the residuals plot produces
# a fanning effect, meaning there is a problem with the equality of variances assmuption.
#4-c
scores[,1]=log(scores[,1])
scores.anova=anova(scores.lm)
scores.lm<-lm(values~ind,data=scores)
scores.anova
plot(scores.lm$fit,scores.lm$res)
qqnorm(scores.lm$residual)
#Residual plots look much better here.  qqnorm return a near linear fit, and the residuals plot is more randomized.
#With the log transformation, we still reject the null hyptothesis (F=6.1905, p<0.01) and say that there is statistical
#evidence that all five materials do not have the same effect on mean failure time.


# More
# Data Problem in hw 2

Problem3.19 <- read.csv(file="L:\\Stat 435-535\\Problem3.19.csv")

attach(Problem3.19)
sapply(Problem3.19, data.class) # this will allows us to know the type of variables we have:
# if the data is not in the form you want, then consider making them factor using
# Orifice<-factor(Orifice)
Orifice <- factor(Orifice)
PartA <- aov(Released~Orifice)
summary(PartA)
hist(Released)
hist(Orifice)  ## cannot be done since Orifice is categorical

data.fit <-lm(Released~Orifice); data.fit

predict(data.fit)

plot(fitted.values(data.fit), residuals(data.fit))
qqnorm(residuals(data.fit))

# Two tests of normality
Released
lillie.test(Released)
sf.test(Released)


library(nortest)
?library
library()  ## list all the possible libraries
library(tseries)     ## is another one to check out.


ad.test(residuals(PartA))
cvm.test(residuals(PartA))
lillie.test(residuals(PartA))
sf.test(residuals(PartA))
Problem3.19
sf.test(Released)
shapiro.test(residuals(PartA))
library(lawstat)
rjb.test(residuals(PartA))
rjb.test(residuals(PartA), option=c("JB"))
rjb.test(residuals(PartA), crit.values=c("empirical"), N=10000)
windows(width=8,height=4)
par(mai=c(1, 1,0.5,0.5))
qqnorm(residuals(PartA), ylab="Residuals"); qqline(residuals(PartA), col=2)
hist(residuals(PartA), xlim=c(-10,10))
plot(Orifice, fitted.values(PartA), type='p', main="Main Effects Plot",xlab="Orifice Diameter", ylab="Mean % Radon Released")
plot(as.numeric(Orifice), fitted.values(PartA), type='p', main="Main Effects Plot", xlab="Orifice Diameter", ylab="Mean % Radon Released")
lines(as.numeric(Orifice), fitted.values(PartA),lty=2)
predict(PartA, interval='confidence', level=0.95, se=FALSE)


# Going back to the data called light bulb we have seen in the class notes

data<-read.table("L:/Stat 435-535/bulb.txt", header=FALSE, col.names=c('block', 'treatment', 'y'))

## A represents the block factor coefficients
A=data[,1]
A
## B is the treatment factor coefficients
B=data[,2]
B

AB=A*B

y=data[,3]

## An easy way to complete the analysis

data
attach(data)
A<-factor(A); B<-factor(B)
treatment<-factor(treatment); block<-factor(block)
fit=lm(y~ A+B+A*B)
fit

anova(fit)

m1.aov=lm(y ~ block +treatment + treatment*block)
m1.aov
summary(m1.aov)

m2.aov=aov(y ~ block +treatment + treatment*block)
m2.aov
summary(m2.aov)



## RCBD with interaction based on the detergent data
## This is an example of a 2 by 2 factorial design.

data<-read.table("G:/Stat 435-535/detergent.txt", header=FALSE, col.names=c('block', 'treatment', 'y'))
## to create the design matrix

mu=rep(1,dim(data)[1])
mu
## A represents the block factor coefficients
A=data[,1]
A
## B is the treatment factor coefficients
B=data[,2]
B

AB=A*B


## An easy way to complete the analysis
fit=lm(y~ A+B+AB)
fit
anova(fit)

## Here is another method that I found in web

y=data[,3]
y
mean(y)
y1bar=mean(y[A==-1])
y1bar
y2bar=mean(y[A==1])
y2bar

## To calculate the main effect of factors A, B and the interaction, take:
mainA=sum(A*y)/(length(y)/2)
mainB=sum(B*y)/(length(y)/2)
intAB=sum(AB*y)/(length(y)/2)

mainA; mainB; intAB

mu.hat=sum(mu*y)/length(y)
mu.hat

tau1.hat=-1*sum(A*y)/ length(y)
tau2.hat=+1*sum(A*y)/ length(y)
beta1.hat=-1*sum(B*y)/ length(y)
beta2.hat=+1*sum(B*y)/ length(y)
ab11.hat=+1*sum(AB*y)/ length(y)
ab12.hat=-1*sum(AB*y)/ length(y)
ab21.hat=-1*sum(AB*y)/ length(y)
ab22.hat=+1*sum(AB*y)/ length(y)

tau1.hat; tau2.hat; beta1.hat; beta2.hat;
ab11.hat; ab12.hat; ab21.hat; ab22.hat

y.pred=sum(mu*y)/length(y)+A*sum(A*y)/length(y)+B*sum(B*y)/length(y)+AB*sum(AB*y)/length(y)
y.pred
SSE=sum((y-y.pred)**2)
SSE

## To find the corresponding p-value

n=length(y)/(2*2)
n

## SSAB is easy to find since interaction is the same in absolute value
SSAB=4*n*ab11.hat**2
SSAB

F=(SSAB/((1)*(1)))/(SSE/((2)*(2)*(n-1)))
F
pvalue=1-pf(F, 1, 4*(n-1))
pvalue

## We reject the null hypothesis that there is no interaction effect.

fit=lm(y~ A+B+AB)
fit
anova(fit)





#5 - Problem 3.36
PowerCalcs<-function(means, variance, alpha) 
{ 
  overallmean<-sum(means)/length(means)
  taus<-c()
  for (i in 1:length(means)) {
    taus<-cbind(taus,means[i]-overallmean)
  }
  taussqaured = taus^2
  sum_tausquared = sum(taussqaured)
  
  thetable<-c()
  
  for (j in 3:20) {
    phisquared = (j*sum_tausquared)/(length(means)*variance)
    phi = sqrt(phisquared)
    a_n = length(means)*(j-1)
    critical=qf(p=1-alpha, df1=length(means)-1, df2=a_n)
    thebeta= pf(q=critical, df1=length(means)-1, df2=a_n, ncp=phisquared*length(means))
    thepower = 1-thebeta
    thetable<-rbind(thetable, c(j,phisquared,phi,a_n,thebeta,thepower))
  }     
  return (thetable)
}   

PowerCalcs(c(50,60,50,60),25,0.05)
#Based on this output, it appears that 5 replicates would do the job.
#37 (a) and (b)
PowerCalcs(c(50,60,50,60),36,0.05)  #7 replicates needed
PowerCalcs(c(50,60,50,60),49,0.05)  #8 replicates needed
#37(c)
#It appears that the sample size needed is about 1 more than the square root of the variance.
#37(d)
#Based on c, find the standard deviation, add 1, and this is how many replicates are needed.




# Problem 8 On random effects models
# R code for part(a)
corn<-read.table("corn.txt", sep=" ", header = TRUE)
attach(corn)
sapply(corn, data.class) # this will allows us to know the type of variables we have:
# if the data is not in the form you want, then consider making them factor using
# site<-factor(site) or parcel<-factor(parcel)
corn

library(lme4) # lme linear mixed effect, or linear mixed effect regression, the equivalent of prc mixed in SAS
ears.lm = lm(ears~1+site); ears.lm  # this is for the fixed effect model. We should not analyze the data using fixed effect model
anova(ears.lm)

ears.lmer = lmer(ears~1+(1|site)); ears.lmer
# the first 1 is the fixed effect model part, that is the overall mean.
# the term (1|site) means that the site is a random effect, and is nested within the intercept
summary(ears.lmer)
anova(ears.lmer) # this should not be studied. I guess that is why there is no output
means=sapply(split(ears, site), mean)
siteFit=sapply(split(fitted(ears.lmer), site), mean); siteFit
data.frame(mean=means, fitted=siteFit)

# Now assume that both variables are random
ears.lmer2 = lmer(ears~1+(1|site)+(1|parcel)); ears.lmer2
summary(ears.lmer2)

# to add the interaction, consider  (1|site:parcel)
# This would not be valid, because we only have one observation per cell combination
# ears.lmer3 = lmer(ears~1+(1|site)+(1|parcel)+ (1|site:parcel)); ears.lmer3
# summary(ears.lmer3)

# we could also add the fact that parcel is nested within site. That is nested type design.
# we will study it in chapter 14, but just to give you an idea, consider
# we assume that both variables are fixed


# for Mixed effect, consider  treatment is fixed and block is random
# response=lmer(treat+(1|blocks))
# Now if we want to add the interaction between treatment and block, consider
# response=lmer(treat+(1|blocks)+ (1|treat:blocks))
# one can also create a variable for the random component:
# say  
nesting<-factor (site:parcel)
ears.lmer5 = lmer(ears~site+site:parcel, data=corn); ears.lm5
ears.lmer5 = lmer(ears~site+nesting, data=corn); ears.lm5
anova(ears.lm5)
summary(ears.lm5);

library(lattice)
par(mfrow=c(1,2))
xyplot(ears~site, pch=16, col="red", main="dotplot")
plot(fitted(ears.lmer), residuals(ears.lmer), main="residual plot") ; abline(h=0);
# We see no clear pattern on the residual plot
