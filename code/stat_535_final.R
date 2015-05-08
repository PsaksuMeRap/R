paste("Jason Rich, STAT 535, Final Exam", date())

#getwd()
#setwd("../)
####################################
# System set-up and package install
####################################

chooseCRANmirror()

pkgs<- install.packages()[,1]
pkgs.need<- c("car","MASS", "caret", "ggplot2", "leaps", "segmented","lattice", "lme4", "stats", "agricolae", "nortest", "pwr")
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
library(agricolae)
library(nortest)
library(pwr)
####################################

####################################
#                   Questions 1
####################################
# a.) Max set of orthoginal contrasts for the feed rate variable
# refer to test for orthogonal contrast

df<- read.table("stat_535_final_prob_one.txt", header= TRUE)
df

str(df)


depthOfCut = as.factor(df$depthOfCut)
feedRate = as.factor(df$feedRate)
surfaceFinish = (df$surfaceFinsh)

df.1<- data.frame(feedRate,depthOfCut,surfaceFinish)
attach(df.1)
str(df.1)

length(depthOfCut); #24
length(feedRate); #24

####################################
# Factor Means
####################################

factorMeans<- tapply(surfaceFinish, feedRate, mean)
factorMeans
# 0.2    0.25     0.3 
# 82.250 100.125 105.375

# c.) 
fit.1<- lm(surfaceFinish ~ feedRate + depthOfCut + feedRate:depthOfCut , data = df.1)
summary(fit.1)

anovaFitOne<- aov(fit.1)
summary(anovaFitOne)

# Analysis of Variance Table
# 
# Response: surfaceFinish
#                     Df  Sum Sq Mean Sq F value    Pr(>F)    
# feedRate             2 2351.58 1175.79 55.3314 8.766e-07 ***
# depthOfCut           3 1543.50  514.50 24.2118 2.233e-05 ***
# feedRate:depthOfCut  6  335.75   55.96  2.6333   0.07241 .  
# Residuals           12  255.00   21.25                      
# ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
####################################
# Tukey's, Bonferroni's, and Scheffe's test

TukeyHSD(anovaFitOne)
pairwise.t.test(surfaceFinish, feedRate, p.adj="bonferroni")

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

model.means.1 <- tapply(surfaceFinish, feedRate, mean)
model.len.1 <- tapply(surfaceFinish, feedRate,length)
dfMSE.1=anovaFitOne$Df
dfMSE2.1=dfMSE[4]
MSE1.1=anovaFitOne$"Mean Sq"
MSE2.1=MSE1[4]
bonferroniCI(model.means.1, model.len.1, dfMSE2.1, MSE2.1, conf=.95)
scheffeCI(model.means.1, model.len.1, dfMSE2.1, MSE2.1, conf=.95)



# d.) 
# feed rate 0.20
mu.11 = (74+64)/2 #69
mu.12 = (79+68)/2 #73.5
mu.13 = (82+88)/2 #85
mu.14 = (99+104)/2 #101.5

# feed rate 0.25
mu.21 = (92+86)/2 #89
mu.22 = (98+104)/2 #101
mu.23 = (99+108)/2 #103.5
mu.24 = (104+110)/2 #107

# feed rate 0.30
mu.31 = (99+98)/2 #98.5
mu.32 = (104+99)/2 #101.5
mu.33 = (108+110)/2 #109
mu.34 = (114+111)/2 #112.5

l.1 = (mu.11 + mu.12 + mu.13 + mu.14)/4

l.2 = (mu.21 + mu.22 + mu.23 + mu.24)/4

meanDiff<- (l.1 - l.2); meanDiff #17.875

tVal<- qt(.975, 23)
#2.068658

MS<- anovaFitOne$"Mean Sq";
MSE<- MS[4]; #21.25
RMSE<- sqrt(MSE); RMSE #4.609772

SE.1<- 4.609772 * (sqrt(1/6))
SE.1

ME.1<- SE.1 * tVal
ME.1

CI<- c(meanDiff-ME.1,meanDiff+ME.1); #(-21.76807, -13.98193)

#e.)
l.3 = (mu.31 + mu.32 + mu.33 + mu.34)/4 #210.75

SE.2<- sqrt((2*MSE)/3)
SE.2 #3.764

ME.2<- SE.2 * tVal
ME.2

(mu.11 - mu.31)-ME.2; (mu.11 - mu.31)+ME.2 
(mu.12 - mu.32)-ME.2; (mu.12 - mu.32)+ME.2 
(mu.13 - mu.33)-ME.2; (mu.13 - mu.33)+ME.2 
(mu.14 - mu.34)-ME.2; (mu.14 - mu.34)+ME.2 


#f.) 

tVal.2<- qt(.9875, 23)

ME.3<- tVal.2 * SE.2
ME.3

(mu.11 - mu.31)-ME.3; (mu.11 - mu.31)+ME.3 
(mu.12 - mu.32)-ME.3; (mu.12 - mu.32)+ME.3 
(mu.13 - mu.33)-ME.3; (mu.13 - mu.33)+ME.3 
(mu.14 - mu.34)-ME.3; (mu.14 - mu.34)+ME.3 


#g.)

### attempt in R

rowMeans<- tapply(surfaceFinish, feedRate, mean);
rowMeans

columnMeans<- tapply(surfaceFinish,depthOfCut, mean);
columnMeans

interactionMeans<- tapply(surfaceFinish,feedRate:depthOfCut, mean)
interactionMeans


#D = 10
PowerCalcs.1<-function(means, variance, alpha) 
{
  overallmean<-sum(means)/length(means)
  taus = 10
  for (i in 1:length(means)) 
  {
  taus<-cbind(taus,means[i]-overallmean)
}
taussqaured = taus^2
sum_tausquared = sum(taussqaured)
thetable<-c()
for (j in 3:20)
{
    phisquared = (j*sum_tausquared)/(length(means)*variance)
    phi = sqrt(phisquared)
    a_n = length(means)*(j-1)
    critical=qf(p=1-alpha, df1=length(means)-1, df2=a_n)
    thebeta= pf(q=critical, df1=length(means)-1, df2=a_n, ncp=phisquared*length(means))
    thepower = 1-thebeta
    sampleSizeTable<-rbind(thetable, c(j,phisquared,phi,a_n,thebeta,thepower))
}
return (sampleSizeTable)
}
PowerCalcs.1(rowMeans,21.25,0.05)
# a = 11

#D = 8
PowerCalcs.2<-function(means, variance, alpha) 
{
  overallmean<-sum(means)/length(means)
  taus = 8
  for (i in 1:length(means)) 
  {
    taus<-cbind(taus,means[i]-overallmean)
  }
  taussqaured = taus^2
  sum_tausquared = sum(taussqaured)
  thetable<-c()
  for (j in 3:20)
  {
    phisquared = (j*sum_tausquared)/(length(means)*variance)
    phi = sqrt(phisquared)
    a_n = length(means)*(j-1)
    critical=qf(p=1-alpha, df1=length(means)-1, df2=a_n)
    thebeta= pf(q=critical, df1=length(means)-1, df2=a_n, ncp=phisquared*length(means))
    thepower = 1-thebeta
    sampleSizeTable<-rbind(thetable, c(j,phisquared,phi,a_n,thebeta,thepower))
  }
  return (sampleSizeTable)
}
PowerCalcs.2(columnMeans,21.25,0.05)
#b = 8

#D = 4
PowerCalcs.3<-function(means, variance, alpha) 
{
  overallmean<-sum(means)/length(means)
  taus = 4
  for (i in 1:length(means)) 
  {
    taus<-cbind(taus,means[i]-overallmean)
  }
  taussqaured = taus^2
  sum_tausquared = sum(taussqaured)
  thetable<-c()
  for (j in 3:20)
  {
    phisquared = (j*sum_tausquared)/(length(means)*variance)
    phi = sqrt(phisquared)
    a_n = length(means)*(j-1)
    critical=qf(p=1-alpha, df1=length(means)-1, df2=a_n)
    thebeta= pf(q=critical, df1=length(means)-1, df2=a_n, ncp=phisquared*length(means))
    thepower = 1-thebeta
    sampleSizeTable<-rbind(thetable, c(j,phisquared,phi,a_n,thebeta,thepower))
  }
  return (sampleSizeTable)
}
PowerCalcs.3(interactionMeans,21.25,0.05)
# c = 13
# from my answers here and my answers from SAS, this code is not producing valid sample size info
# below is the SAS code used to calculate to sample size annotated on my exam


# SAS CODE

# %let alpha=0.05;
# %let a=3;
# %let b=4;
# %let sigma=4.609; /*since sigma^2=21.25 */
#   %let maxsize=1000;
# 
# 
# data power1;
# D=10;
# do size_n=2 to &maxsize by 1;
# df1=&a-1;
# df2=&a*&b*(size_n-1);
# critical=finv(1-&alpha, df1, df2,0);
# nc=size_n*&b*D**2/2/&sigma**2;
# power=1-probf(critical, df1, df2, nc);
# output;
# end;
# proc print;
# title 'sample size needed for the difference in the row means D=10';
# id size_n;
# run;
# 
# 
# data power2;
# D=8;
# do size_n=2 to &maxsize by 1;
# df1=&b-1;
# df2=&a*&b*(size_n-1);
# critical=finv(1-&alpha, df1, df2,0);
# nc=size_n*&a*D**2/2/&sigma**2;
# power=1-probf(critical, df1, df2, nc);
# output;
# end;
# proc print;
# title 'sample size needed for the difference in the column means D=8';
# id size_n;
# run;
# 
# 
# 
# data power3;
# D=4;
# do size_n=2 to &maxsize by 1;
# df1=(&a-1)*(&b-1);
# df2=&a*&b*(size_n-1);
# critical=finv(1-&alpha, df1, df2,0);
# nc=size_n*D**2/2/&sigma**2;
# power=1-probf(critical, df1, df2, nc);
# output;
# end;
# proc print;
# title 'sample size needed for the difference in the interaction effect D=4';
# id size_n;
# run;
# quit;


# problem 2

#c.) 
df.2<- read.table("stat_535_prob_2.txt", header = TRUE)
df.2
str(df.2)

machine = as.factor(df.2$machine)
operator = as.factor(df.2$operator)
day = as.factor(df.2$day)
y = (df.2$y)
nestFactor<- factor(machine:operator)

df.3<- data.frame(machine, operator, day, nestFactor, y)
str(df.3)

nestedFit = lm(y ~ machine + nestFactor + day, data = df.3)
summary(nestedFit)

nestedAnova<- aov(nestedFit)
nestedAnova
# treating operator as random, studying its results are not useful. 

#d.) 
machineMeans<- tapply(y, machine, mean)
# 1     2     3 
# 61.20 69.60 73.55 
machineMeans
machineMeans[1]-machineMeans[2] #-8.47
machineMeans[1]-machineMeans[3] #-12.35
machineMeans[2]-machineMeans[3] #-3.95

pairwise.t.test(y, machine, p.adj="bonferroni")
# Pairwise comparisons using t tests with pooled SD 

# data:  y and machine 

# 1      2     
# 2 0.0068 -     
# 3 5e-05  0.4145

# P value adjustment method: bonferroni 


TukeyHSD(nestedAnova)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = nestedFit)
# 
# $machine
# diff       lwr       upr     p adj
# 2-1  8.40  3.948458 12.851542 0.0001123
# 3-1 12.35  7.898458 16.801542 0.0000001
# 3-2  3.95 -0.501542  8.401542 0.0910548

#e.) 

tVal.3<- qt(.95, 9)
seDiff<- sqrt((2*235.53)/45)
tVal.3 * seDiff

((machineMeans[1]-machineMeans[2])- qt(.95, 9)*sqrt((2*235.53)/45))
((machineMeans[1]-machineMeans[2])+ qt(.95, 9)*sqrt((2*235.53)/45))

#f.)

OperatorMeans<- sapply(split(y, nestFactor), mean)
OperatorMeans per machine 
# 1:1  1:2  1:3  1:4  2:1  2:2  2:3  2:4  3:1  3:2  3:3  3:4 
# 61.8 67.8 62.6 52.6 70.4 75.2 55.8 77.0 76.8 69.6 74.4 73.4 
# difference per machine

pairwise.t.test(y, nestFactor, p.adj="bonferroni")

# Pairwise comparisons using t tests with pooled SD 
# 
# data:  y and nestFactor 
# 
#   1:1     1:2     1:3     1:4     2:1     2:2     2:3     2:4     3:1     3:2     3:3    
#   1:2 1.00000 -       -       -       -       -       -       -       -       -       -      
#   1:3 1.00000 1.00000 -       -       -       -       -       -       -       -       -      
#   1:4 1.00000 0.01908 0.87403 -       -       -       -       -       -       -       -      
#   2:1 1.00000 1.00000 1.00000 0.00219 -       -       -       -       -       -       -      
#   2:2 0.07838 1.00000 0.14286 3.2e-05 1.00000 -       -       -       -       -       -      
#   2:3 1.00000 0.22133 1.00000 1.00000 0.03083 0.00055 -       -       -       -       -      
#   2:4 0.01908 1.00000 0.03611 6.2e-06 1.00000 1.00000 0.00011 -       -       -       -      
#   3:1 0.02241 1.00000 0.04225 7.5e-06 1.00000 1.00000 0.00013 1.00000 -       -       -      
#   3:2 1.00000 1.00000 1.00000 0.00432 1.00000 1.00000 0.05767 1.00000 1.00000 -       -      
#   3:3 0.14286 1.00000 0.25546 6.5e-05 1.00000 1.00000 0.00110 1.00000 1.00000 1.00000 -      
#   3:4 0.29448 1.00000 0.51307 0.00016 1.00000 1.00000 0.00260 1.00000 1.00000 1.00000 1.00000
# 
# P value adjustment method: bonferroni 

# Rejection where p-value <.05
#g.)
contrast<- ((61.8 + 67.8 + 62.6)/3)-52.6
contrast

qt(.95, 3)
11.4667-5.574
11.4667+5.574

