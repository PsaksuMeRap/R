paste("Jason Rich, Homework #3", date())

setwd("/Volumes/Lacie/GitHub/R/data")
getwd()

#####################################################################
# System set-up and package install
#####################################################################


chooseCRANmirror() ## server mirror I am using 86 (Berkley California)

pkgs<- install.packages()[,1]
pkgs.need<- c("car","MASS", "caret", "ggplot2", "leaps", "segmented", "lattice", "lme4","stats", "agricolae", "nortest")
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
#####################################################################


data<- read.table("bolt.txt", sep=" ", header = TRUE, fill = FALSE, strip.white=TRUE)
data
summary(data)
attach(data)

#### transforming chemical and bolt to factors ###
chemical= factor(data$chemical); # treatment factor with 4 levels
bolt= factor(data$bolt); # blocking factor with 5 levels
dataFrame<- data.frame(chemical, bolt, y); #new dataframe with chemical and bolt as factors, and y (response) as a integer
dataFrame
str(dataFrame)

detach(data)
attach(dataFrame)
#####################################################################

# a.)  point estimates of the four strength means of cloth

factorMeans<- tapply(y, chemical, mean);
factorMeans;
#  1    2    3    4 
# 70.6 71.4 72.4 72.6 

boltModel<- lm(y~ chemical+ bolt, data= dataFrame)
boltModel
summary(boltModel)

boltAnova<- anova(boltModel)
boltAnova

# Analysis of Variance Table
# 
# Response: y
#           Df Sum Sq  Mean Sq  F-value   Pr(>F)    
# chemical   3  12.95   4.317   2.3761    0.1211    
# bolt       4 157.00  39.250  21.6055    2.059e-05 ***
# Residuals 12  21.80   1.817                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
###

# b.) testing equality of the mean strength

# H_0: mu.1=mu.2=mu.3=mu.4
# H_a: at least two mu.i!= 0  for all i=1,2,3,4

# from the ANOVA table above, the F-value  for the treamtment means is 2.376 and a p-value of 0.1211 
qf(.975, 3, 12) # 4.4742 

#rejection criteria F-value > F(a/2, a-1, N-a)
# at this time, the evidence supports the claim that the chemical mean does not affect the strength of the cloth, and I fail to reject the null hypothesis
###

# c.) Pairwise comparison using Fisher LSD, Tukey and Bonferroni's method

# Fisher's LSD pairwise intervals
dfMSE= boltAnova$Df; dfMSE
dfMSE2= dfMSE[3]; dfMSE2
MSE1= boltAnova$"Mean Sq"; MSE1
MSE2= MSE1[3]; MSE2


fisherLsdTest<- LSD.test(y, chemical, dfMSE2, MSE2, alpha= .025, group= FALSE) # group is false, gives us comparison between treatment means
fisherLsdTest
# $statistics
# Mean       CV MSerror      LSD
# 71.75 8.731682   39.25 13.84993
# 
# $parameters
# Df ntr  t.value
# 12   4 2.560033
# 
# $means
#      y      std r      LCL      UCL Min Max
# 1 70.6 3.049590 5 69.05689 72.14311  67  74
# 2 71.4 3.049590 5 69.85689 72.94311  67  75
# 3 72.4 4.393177 5 70.85689 73.94311  68  78
# 4 72.6 2.607681 5 71.05689 74.14311  69  75
# 
# $comparison
#       Difference     pvalue sig.       LCL       UCL
# 1 - 2       -0.8 0.36650665      -2.982294 1.3822936
# 1 - 3       -1.8 0.05637430    . -3.982294 0.3822936
# 1 - 4       -2.0 0.03696797    * -4.182294 0.1822936
# 2 - 3       -1.0 0.26351854      -3.182294 1.1822936
# 2 - 4       -1.2 0.18459029      -3.382294 0.9822936
# 3 - 4       -0.2 0.81846029      -2.382294 1.9822936



# the Tukeys pairwise comparisons for chemical (we are not concerned with the blocking comparisons)
choose(4,2) # 6 pairwise compairsons

TukeyHSD(aov(y~ chemical+ bolt,data= dataFrame ),conf.level=0.95)
# $chemical
# diff        lwr      upr     p adj
# 2-1  0.8 -1.7308322 3.330832 0.7852734
# 3-1  1.8 -0.7308322 4.330832 0.2042593
# 4-1  2.0 -0.5308322 4.530832 0.1417326
# 3-2  1.0 -1.5308322 3.530832 0.6540138
# 4-2  1.2 -1.3308322 3.730832 0.5182726
# 4-3  0.2 -2.3308322 2.730832 0.9952030


# Bonferroni's intervals
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


model.means<- tapply(y,chemical,mean); model.means
model.len<- tapply(y,chemical,length); model.len
dfMSE= boltAnova$Df; dfMSE
dfMSE2= dfMSE[3]; dfMSE2
MSE1= boltAnova$"Mean Sq"; MSE1
MSE2= MSE1[3]; MSE2
bonferroniCI(model.means, model.len, dfMSE2, MSE2, conf=.95);

#               Lower Diff    Upper
# mu1 - mu2 -3.806882 -0.8 2.206882
# mu1 - mu3 -4.806882 -1.8 1.206882
# mu1 - mu4 -5.006882 -2.0 1.006882
# mu2 - mu3 -4.006882 -1.0 2.006882
# mu2 - mu4 -4.206882 -1.2 1.806882
# mu3 - mu4 -3.206882 -0.2 2.806882

# another method to calculate Bonferroni's pairwise comparisons
# using pairwise.t.test(x, g, p.adjust.method) to test the pairwise comparisons between the treatment group means 
# one note; other options exist for p.adj object with the function. use  ?pairwise.t.test for more information
# default is pooled standard deviation

# Bonferroni adjustment
bonPairwise<- pairwise.t.test(y, chemical, p.adj= "bonferroni", data= dataFrame)
bonPairwise

# Pairwise comparisons using t tests with pooled SD 
# 
# data:  y and chemical 
#   1 2 3
# 2 1 - -
# 3 1 1 -
# 4 1 1 1
# P value adjustment method: bonferroni 

# This comparison states that all four chemicals are statistically insignificant, and have no affect on the cloth in testing.  

###
# d. ) One possible maximal set of mutually orthagonal contrasts:

# c.1 = mu.1 - mu.2
# c.2 = mu.1+ mu.2- 2* mu.3
# c.3 = mu.1+ mu.2+ mu.3- 3* mu.4


# contrast set-up, to use for the Bonferroni intervals

treatMeans<- tapply(y, chemical, mean)
treatMeans


mu.1== treatMeans[1]; mu.1 # 70.6
mu.2== treatMeans[2]; mu.2 # 71.4
mu.3== treatMeans[3]; mu.3 # 72.4
mu.4== treatMeans[4]; mu.4 # 72.6

coeffOne<- c(1,-1)
coeffTwo<- c(1, 1, -2)
coeffThree<- c(1, 1, 1, -3)

# c.1 = mu.1 - mu.2; c.1 # -0.8
# c.2 = mu.1+ mu.2- 2* mu.3; c.2 # -2.8
# c.3 = mu.1+ mu.2+ mu.3- 3* mu.4; c.3 # -3.4

# CI for orthogonal contrast 1
all.pairs <- function(r) {
  list(first = rep(1:r,rep(r,r))[lower.tri(diag(r))],
       second = rep(1:r, r)[lower.tri(diag(r))])
}
# Bonferroni's intervals for c.1
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

# scheffe's for c.1
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


model.means <- tapply(y,chemical, mean); model.means
model.len <- tapply(y, chemical,length); model.len
dfMSE=boltAnova$Df; dfMSE
dfMSE2=dfMSE[3]; dfMSE2
MSE1=boltAnova$"Mean Sq"; MSE1
MSE2=MSE1[3]; MSE2

bonferroniCI(model.means, model.len, dfMSE2, MSE2, conf=.95)
#               Lower  Diff   Upper
#             Lower Diff    Upper
# mu1 - mu2 -3.806882 -0.8 2.206882
# mu1 - mu3 -4.806882 -1.8 1.206882
# mu1 - mu4 -5.006882 -2.0 1.006882
# mu2 - mu3 -4.006882 -1.0 2.006882
# mu2 - mu4 -4.206882 -1.2 1.806882
# mu3 - mu4 -3.206882 -0.2 2.806882

scheffeCI(model.means, model.len, dfMSE2, MSE2, conf=.95)
#               Lower  Diff   Upper
# mu1 - mu2 -3.558413 -0.8 1.9584131
# mu1 - mu3 -4.558413 -1.8 0.9584131
# mu1 - mu4 -4.758413 -2.0 0.7584131
# mu2 - mu3 -3.758413 -1.0 1.7584131
# mu2 - mu4 -3.958413 -1.2 1.5584131
# mu3 - mu4 -2.958413 -0.2 2.5584131

# CI for orthogonal contrast 2
all.pairs<- function(r) {
  list(first= rep(1:r,rep(r,r))[lower.tri(diag(r))]
       ,second= rep(1:r, r)[lower.tri(diag(r))]
       ,third= rep(1:r, r)[lower.tri(diag(r))])
}

bonferroniCI2 <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- (fitted[pairs$first] + fitted[pairs$second]) - 2*fitted[pairs$third] #adjusted for contrast
  T <- qt(1-(1-conf.level)/(2*r*(r-1)),df)
  hwidths <-  T*sqrt(MSE*(1/nis[pairs$first] + 1/nis[pairs$second]+ 1/nis[pairs$third]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  #dimnames(val) <- list(paste(("mu",pairs$first,"+ mu",pairs$second)/2 "- mu",pairs$third,
                                   #sep=""), c("Lower", "Diff","Upper"))
  val
}

# scheffe's for c.2
scheffeCI2 <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- (fitted[pairs$first] + fitted[pairs$second])- 2*fitted[pairs$third] #adjusted for contrast
  T <- sqrt((r-1)*qf(conf.level,r-1,df))
  hwidths <-  T*sqrt(MSE*(1/nis[pairs$first] + 1/nis[pairs$second]+ 1/nis[pairs$third]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  #dimnames(val) <- list(paste(("mu", pairs$first," + mu", pairs$second)" - mu", pairs$third,
                                    #sep=""), c("Lower", "Diff","Upper"))
  val
}


model.means # calculated above, this is just a call
model.len<- tapply(y, chemical,length); model.len
dfMSE= boltAnova$Df; dfMSE
dfMSE2= dfMSE[3]; dfMSE2
MSE1= boltAnova$"Mean Sq"; MSE1
MSE2= MSE1[3]; MSE2
bonferroniCI2(model.means, model.len, dfMSE2, MSE2, conf=.95)
# 1 -4.482664  -0.8 2.882664
# 1 -5.482664  -1.8 1.882664
# 1 -5.682664  -2.0 1.682664
# 2 -4.682664  -1.0 2.682664
# 2 -4.882664  -1.2 2.482664
# 3 -3.882664  -0.2 3.482664
scheffeCI2(model.means, model.len, dfMSE2, MSE2, conf=.95)
# 1 -4.178352  -0.8 2.578352
# 1 -5.178352  -1.8 1.578352
# 1 -5.378352  -2.0 1.378352
# 2 -4.378352  -1.0 2.378352
# 2 -4.578352  -1.2 2.178352
# 3 -3.578352  -0.2 3.178352

# CI for orthogonal contrast 3
all.pairs<- function(r) {
  list(first= rep(1:r,rep(r,r))[lower.tri(diag(r))]
       ,second= rep(1:r, r)[lower.tri(diag(r))]
       ,third= rep(1:r, r)[lower.tri(diag(r))]
       ,fourth= rep(1:r, r)[lower.tri(diag(r))])
}

bonferroniCI3 <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- (fitted[pairs$first] + fitted[pairs$second] + fitted[pairs$third]) - (3*fitted[pairs$fourth]) #adjusted for contrast
  T <- qt(1-(1-conf.level)/(2*r*(r-1)),df)
  hwidths <-  T*sqrt(MSE*(1/nis[pairs$first] + 1/nis[pairs$second] + 1/nis[pairs$third] + 1/nis[pairs$fourth]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  #dimnames(val) <- list(paste(("mu",pairs$first,"+ mu",pairs$second)/2 "- mu",pairs$third,
  #                                 sep=""), c("Lower", "Diff","Upper"))
  val
}

# scheffe's for c.2
scheffeCI3 <- function(fitted, nis, df, MSE, conf.level=.95){
  r <- length(fitted)
  pairs <- all.pairs(r)
  diffs <- (fitted[pairs$first] + fitted[pairs$second] + fitted[pairs$third]) - (3*fitted[pairs$fourth])  #adjusted for contrast
  T <- sqrt((r-1)*qf(conf.level,r-1,df))
  hwidths <-  T*sqrt(MSE*(1/nis[pairs$first] + 1/nis[pairs$second] + 1/nis[pairs$third] + 1/nis[pairs$fourth]))
  val <- cbind(diffs - hwidths, diffs, diffs + hwidths)
  #dimnames(val) <- list(paste(("mu", pairs$first," + mu", pairs$second)/2 " - mu", pairs$third,
  #                                  sep=""), c("Lower", "Diff","Upper"))
  val
}
 
model.means # calculated above, this is just a call
model.len<- tapply(y, chemical,length); model.len
dfMSE= boltAnova$Df; dfMSE2
dfMSE2= dfMSE[3]; dfMSE2
MSE1= boltAnova$"Mean Sq"; MSE1
MSE2= MSE1[3];MSE2
bonferroniCI3(model.means, model.len, dfMSE2, MSE2, conf=.95)
# 1 -5.052374  -0.8 3.452374
# 1 -6.052374  -1.8 2.452374
# 1 -6.252374  -2.0 2.252374
# 2 -5.252374  -1.0 3.252374
# 2 -5.452374  -1.2 3.052374
# 3 -4.452374  -0.2 4.052374
scheffeCI3(model.means, model.len2, dfMSE2, MSE2, conf=.95)
# 1 -4.700985  -0.8 3.100985
# 1 -5.700985  -1.8 2.100985
# 1 -5.900985  -2.0 1.900985
# 2 -4.900985  -1.0 2.900985
# 2 -5.100985  -1.2 2.700985
# 3 -4.100985  -0.2 3.700985

detach(dataFrame)
###
#e.)
# H_0: The data is normally distributed
# H_a: The data is non-normally distributed

#Test for normality
plot(fitted.values(boltModel), residuals(boltModel))
qqnorm(residuals(boltModel))

# a review of the plots show only small departures from nomality. Fail to reject the H_0, and conclude
# at this time, the data does not significantly depart from normality, thus I fail to reject the null hypothesis, that
# this data is normally distributed. 

##########################
# problem 3.) 4.7
# refer to written portion for model, assumptions and goal
getwd()
prob3<- read.table("prob_3.txt", header=TRUE)

str(prob3)

tip= as.factor(prob3$tip); tip
coupon= as.factor(prob3$coupon); coupon
y.1= prob3$y

prob3.001<- data.frame(tip, coupon, y.1)
str(prob3.001)
attach(prob3.001)
###

prob3.fit<- lm(y.1~ tip + coupon, data= prob3.001)
summary(prob3.fit)

prob3Anova<-anova(prob3.fit)
summary(prob3Anova)
prob3Anova
# Analysis of Variance Table
# 
# Response: y.1
#           Df Sum Sq  Mean Sq F value    Pr(>F)    
# tip        3  0.825 0.275000  30.938 4.523e-05 ***
# coupon     3  0.385 0.128333  14.438 0.0008713 ***
# Residuals  9  0.080 0.008889                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# The Model F-value of 14.44 implies the model is significant. 

#treatment means
pro3Means<- tapply(y.1, tip, mean)
# 1     2     3     4 
# 9.400 9.425 9.725 9.950 

mu.01= pro3Means[1]; mu.01 # 9.40
mu.02= pro3Means[2]; mu.02 # 9.43
mu.03= pro3Means[3]; mu.03 # 9.73
mu.04= pro3Means[4]; mu.04 # 9.95

mu.01- mu.02; # -0.025
mu.01- mu.03; # -0.325
mu.01- mu.04; # -0.55 
mu.02- mu.03; # -0.3
mu.02- mu.04; # -0.525
mu.03- mu.04; # -0.225
# The Model F-value of 14.44 implies the model is significant, and p-vlaue significant at the .001 level varifiessignificant.
# There is a difference between the means of the four tips.
# the evidence does not support the claim the all four treatment mean do not affect the response. Thus, I reject the null hypothesis. 

# b.) 
# Fisher's LSD pairwise intervals
dfMSE.1= prob3Anova$Df; dfMSE.1
dfMSE.2= dfMSE[3]; dfMSE.2
MSE.1= prob3Anova$"Mean Sq"; MSE.1
MSE.2= MSE.1[3]; MSE.2


# b.) 
fisherLsdTest<- LSD.test(y.1,tip, dfMSE2, MSE2, alpha= .025, group= FALSE) # group is false, gives us comparison between treatment means
fisherLsdTest
# $statistics
# Mean       CV  MSerror
# 9.625 14.00351 1.816667
# 
# $parameters
# Df ntr  t.value
# 12   4 2.560033
# 
# $means
# y.1       std r      LCL          UCL Min  Max
# 1 9.400 0.2160247 4 7.674745 11.12525 9.2  9.7
# 2 9.425 0.1258306 4 7.699745 11.15025 9.3  9.6
# 3 9.725 0.2217356 4 7.999745 11.45025 9.5 10.0
# 4 9.950 0.2081666 4 8.224745 11.67525 9.7 10.2
# 
# $comparison
# Difference    pvalue sig.       LCL      UCL
# 1 - 2     -0.025 0.9795041      -2.464878 2.414878
# 1 - 3     -0.325 0.7389961      -2.764878 2.114878
# 1 - 4     -0.550 0.5745489      -2.989878 1.889878
# 2 - 3     -0.300 0.7583393      -2.739878 2.139878
# 2 - 4     -0.525 0.5918416      -2.964878 1.914878
# 3 - 4     -0.225 0.8173516      -2.664878 2.214878

# we can see for the above analysis that the mean of tip 4 is significantly different than the mean of tips 1, 2, 3

# c.) 
plot(fitted.values(prob3.fit), residuals(prob3.fit))
qqnorm(residuals(prob3.fit))

# a review of the plots does not indicate major departures from normality assumption

# 4.) 4.22
#data 
#input data from the book (into the variable from right to left) as column 1, 2, 3 etc, for all columns
# as an example, 8,11,4,6,4 is column 1, column 2 starts with 7, and so on!
prod<- c(8,11,4,6,4,7,2,9,8,2,1,7,10,6,3,7,3,1,6,8,3,8,5,10,8)
ingredients<- c("A","C","B","D","E","B","E","A","C","D", "D","A","C","E","B", "C","D","E","B", "A", "E", "B", "D", "A", "C")
batch<- c(rep("b1",1), rep("b2",1), rep("b3",1), rep("b4",1), rep("b5",1))
day<- c(rep("d1",5), rep("d2",5), rep("d3",5),rep("d4",5), rep("d5",5))


#building dataframe
prob.4<- data.frame(day, batch, ingredients, prod)
#call
prob.4


#verifying the matrix of latin letters
matrix(prob.4$ingredients, 5,5)
#     [,1] [,2] [,3] [,4]
# [1,] "A"  "B"  "C"  "D" 
# [2,] "B"  "C"  "D"  "A" 
# [3,] "C"  "D"  "A"  "B" 
# [4,] "D"  "A"  "B"  "C" 


#verifying the response matrix
matrix(prob.4$prod, 5,5)
#       [,1] [,2] [,3] [,4] [,5]
# [1,]    8    7    1    7    3
# [2,]   11    2    7    3    8
# [3,]    4    9   10    1    5
# [4,]    6    8    6    6   10
# [5,]    4    2    3    8    8

latinSquareMeans<- tapply(prod, ingredients, mean)
latinSquareMeans
# A   B   C   D   E 
# 8.4 5.6 8.8 3.4 3.2 

prod4Fit<- lm(prod ~ batch+ day+ ingredients, prob.4)
summary(prod4Fit)

prodAnova<- anova(prod4Fit)
prodAnova
# Analysis of Variance Table
#
# Response: prod
#              Df Sum Sq Mean Sq F value    Pr(>F)    
# batch        4  15.44   3.860  1.2345 0.3476182    
# day          4  12.24   3.060  0.9787 0.4550143    
# ingredients  4 141.44  35.360 11.3092 0.0004877 ***
# Residuals   12  37.52   3.127                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# call Bon and Scheffe functions, built earlier

#a.) 
modelLen <- tapply(prod, ingredients,length); modelLen
dfMSE=prodAnova$Df; dfMSE
dfMSE2=dfMSE[4]; dfMSE2
MSE1=prodAnova$"Mean Sq"; MSE1
MSE2=MSE1[4]; MSE2
bonferroniCI(latinSquareMeans, modelLen, dfMSE2, MSE2, conf=.95)
#               Lower Diff    Upper
# mu1 - mu2 -1.4569516  2.8 7.056952
# mu1 - mu3 -4.6569516 -0.4 3.856952
# mu1 - mu4  0.7430484  5.0 9.256952
# mu1 - mu5  0.9430484  5.2 9.456952
# mu2 - mu3 -7.4569516 -3.2 1.056952
# mu2 - mu4 -2.0569516  2.2 6.456952
# mu2 - mu5 -1.8569516  2.4 6.656952
# mu3 - mu4  1.1430484  5.4 9.656952
# mu3 - mu5  1.3430484  5.6 9.856952
# mu4 - mu5 -4.0569516  0.2 4.456952
scheffeCI(latinSquareMeans, modelLen, dfMSE2, MSE2, conf=.95)
#                 Lower Diff     Upper
# mu1 - mu2 -1.2378862  2.8 6.8378862
# mu1 - mu3 -4.4378862 -0.4 3.6378862
# mu1 - mu4  0.9621138  5.0 9.0378862
# mu1 - mu5  1.1621138  5.2 9.2378862
# mu2 - mu3 -7.2378862 -3.2 0.8378862
# mu2 - mu4 -1.8378862  2.2 6.2378862
# mu2 - mu5 -1.6378862  2.4 6.4378862
# mu3 - mu4  1.3621138  5.4 9.4378862
# mu3 - mu5  1.5621138  5.6 9.6378862
# mu4 - mu5 -3.8378862  0.2 4.2378862
TukeyHSD(aov(prod ~ batch+ day+ ingredients,data= prob.4 ),conf.level=0.95)
# $batch
#       diff       lwr      upr     p adj
# b2-b1  1.0 -2.564608 4.564608 0.8936609
# b3-b1  0.6 -2.964608 4.164608 0.9816047
# b4-b1  2.0 -1.564608 5.564608 0.4225127
# b5-b1 -0.2 -3.764608 3.364608 0.9997349
# b3-b2 -0.4 -3.964608 3.164608 0.9960012
# b4-b2  1.0 -2.564608 4.564608 0.8936609
# b5-b2 -1.2 -4.764608 2.364608 0.8166339
# b4-b3  1.4 -2.164608 4.964608 0.7232162
# b5-b3 -0.8 -4.364608 2.764608 0.9489243
# b5-b4 -2.2 -5.764608 1.364608 0.3365811
# 
# $day
#       diff       lwr      upr     p adj
# d2-d1 -1.0 -4.564608 2.564608 0.8936609
# d3-d1 -1.2 -4.764608 2.364608 0.8166339
# d4-d1 -1.6 -5.164608 1.964608 0.6212723
# d5-d1  0.2 -3.364608 3.764608 0.9997349
# d3-d2 -0.2 -3.764608 3.364608 0.9997349
# d4-d2 -0.6 -4.164608 2.964608 0.9816047
# d5-d2  1.2 -2.364608 4.764608 0.8166339
# d4-d3 -0.4 -3.964608 3.164608 0.9960012
# d5-d3  1.4 -2.164608 4.964608 0.7232162
# d5-d4  1.8 -1.764608 5.364608 0.5188508
# 
# $ingredients
#     diff        lwr        upr     p adj
# B-A -2.8 -6.3646078  0.7646078 0.1539433
# C-A  0.4 -3.1646078  3.9646078 0.9960012
# D-A -5.0 -8.5646078 -1.4353922 0.0055862
# E-A -5.2 -8.7646078 -1.6353922 0.0041431
# C-B  3.2 -0.3646078  6.7646078 0.0864353
# D-B -2.2 -5.7646078  1.3646078 0.3365811
# E-B -2.4 -5.9646078  1.1646078 0.2631551
# D-C -5.4 -8.9646078 -1.8353922 0.0030822
# E-C -5.6 -9.1646078 -2.0353922 0.0023007
# E-D -0.2 -3.7646078  3.3646078 0.9997349

# b.)

#(L1= c.1- c.2, or c.1= c.2)
# H_0: c.1= c.2
# H_a: c.1 != c.2

contrastcoeff= c(1/2,1/2,0,-1/2,-1/2); contrastcoeff
contrastSquared= sum(contrastcoeff^2); contrastSquared
meansum= c(latinSquareMeans[1], latinSquareMeans[2], latinSquareMeans[3], latinSquareMeans[4], latinSquareMeans[5]); meansum 
tnum= sum(contrastcoeff*meansum); tnum
tStat= tnum/sqrt(3.127/5*contrastSquared); tstat
criticalT = -qt(p=0.025,df=12); criticalT
#tStat = 4.678674 > criticalT = 2.178813
# the evidence does not supoprt the claim that contrast.1 = contrast.2 in L1. I reject the null hyupothesis



# (L2= c.11- c.21, or c.11= c.21)
# H_0: c.11= c.21
# H_a: c.11 != c.21
contrastcoeff.001= c(1/2,1/2,-1/3,-1/3,-1/3); contrastcoeff.001
contrastSquared.001 = sum(contrastcoeff^2); contrastSquared.001
meansum.001= c(latinSquareMeans[1], latinSquareMeans[2], latinSquareMeans[3], latinSquareMeans[4], latinSquareMeans[5]); meansum.001 
tnum.001= sum(contrastcoeff.001*meansum.001); tnum.001
tStat.001= tnum.001/sqrt(3.127/5*contrastSquared); tStat.001
criticalT.001= -qt(p=0.025,df=20); criticalT.001
# tStat.001 = 2.360412 > criticalT.001 = 2.085963
# the evidence does not supports the claim that c.11 = c.21, thus I reject the null hypothesis


# c.)
# missing value estimation function I found online at Columbia Univeristy
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}


prod<- c(NA, 11,4,6,4,7,2,9,8,2,1,7,10,6,3,7,3,1,6,8,3,8,5,10,8)
ingredients<- c("A","C","B","D","E","B","E","A","C","D", "D","A","C","E","B", "C","D","E","B", "A", "E", "B", "D", "A", "C")
batch<- c(rep("b1",1), rep("b2",1), rep("b3",1), rep("b4",1), rep("b5",1))
day<- c(rep("d1",5), rep("d2",5), rep("d3",5),rep("d4",5), rep("d5",5))

prob.4.1<- data.frame(day, batch, ingredients, prod)
attach(prob.4.1)
matrix(prob.4.1$prod, 5,5)
#missing value annotated by an NA
#       [,1] [,2] [,3] [,4] [,5]
# [1,]   NA    7    1    7    3
# [2,]   11    2    7    3    8
# [3,]    4    9   10    1    5
# [4,]    6    8    6    6   10
# [5,]    4    2    3    8    8

missingValue<- random.imp(prod) 
missingValue # [1] 10 11  4  6  4  7  2  9  8  2  1  7 10  6  3  7  3  1  6  8  3  8  5 10  8

matrix(missingValue, 5,5)
#     [,1] [,2] [,3] [,4] [,5]
# [1,]   10    7    1    7    3
# [2,]   11    2    7    3    8
# [3,]    4    9   10    1    5
# [4,]    6    8    6    6   10
# [5,]    4    2    3    8    8
# we can see this function estimates a value of 10, were I removed the 8

prodNew= c(10,11,4,6,4,7,2,9,8,2,1,7,10,6,3,7,3,1,6,8,3,8,5,10,8)
newDataframe<- data.frame(day, batch, ingredients, prodNew)
latinSquareMeans.001<- tapply(prodNew, ingredients, mean)
latinSquareMeans.001
# A   B   C   D   E 
# 8.8 5.6 8.8 3.4 3.2 

#the treatment mean of A raised from 8.4 to 8.8

newFit<- lm(prodNew~ batch+ day+ ingredients, newDataframe)
summary(newFit)

newAnova<- anova(newFit)
newAnova
# Analysis of Variance Table
 
# Response: prodNew
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# batch        4  13.36    3.34  1.0637 0.4161257    
# day          4  15.76    3.94  1.2548 0.3403000    
# ingredients  4 152.16   38.04 12.1146 0.0003552 ***
# Residuals   12  37.68    3.14                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

modelLennew<- tapply(prodNew, ingredients,length); modelLennew
dfMSEnew= newAnova$Df; dfMSEnew
dfMSE2new= dfMSEnew[4]; dfMSE2new
MSE1new= newAnova$"Mean Sq"; MSE1new
MSE2new= MSE1new[4]; MSE2new
bonferroniCI(latinSquareMeans.001, modelLennew, dfMSE2new, MSE2new, conf=.95)
#               Lower Diff    Upper
# mu1 - mu2 -1.066019  3.2 7.466019
# mu1 - mu3 -4.266019  0.0 4.266019
# mu1 - mu4  1.133981  5.4 9.666019
# mu1 - mu5  1.333981  5.6 9.866019
# mu2 - mu3 -7.466019 -3.2 1.066019
# mu2 - mu4 -2.066019  2.2 6.466019
# mu2 - mu5 -1.866019  2.4 6.666019
# mu3 - mu4  1.133981  5.4 9.666019
# mu3 - mu5  1.333981  5.6 9.866019
# mu4 - mu5 -4.066019  0.2 4.466019

scheffeCI(latinSquareMeans.001, modelLennew, dfMSE2new, MSE2new, conf=.95)
#               Lower Diff     Upper
# mu1 - mu2 -0.8464866  3.2 7.2464866
# mu1 - mu3 -4.0464866  0.0 4.0464866
# mu1 - mu4  1.3535134  5.4 9.4464866
# mu1 - mu5  1.5535134  5.6 9.6464866
# mu2 - mu3 -7.2464866 -3.2 0.8464866
# mu2 - mu4 -1.8464866  2.2 6.2464866
# mu2 - mu5 -1.6464866  2.4 6.4464866
# mu3 - mu4  1.3535134  5.4 9.4464866
# mu3 - mu5  1.5535134  5.6 9.6464866
# mu4 - mu5 -3.8464866  0.2 4.2464866

TukeyHSD(aov(prodNew~ batch+ day+ ingredients, data= newDataframe),conf.level=0.95)
# $batch
#       diff     lwr    upr     p adj
# b2-b1  0.6 -2.9722 4.1722 0.9817474
# b3-b1  0.2 -3.3722 3.7722 0.9997372
# b4-b1  1.6 -1.9722 5.1722 0.6230311
# b5-b1 -0.6 -4.1722 2.9722 0.9817474
# b3-b2 -0.4 -3.9722 3.1722 0.9960338
# b4-b2  1.0 -2.5722 4.5722 0.8943686
# b5-b2 -1.2 -4.7722 2.3722 0.8177345
# b4-b3  1.4 -2.1722 4.9722 0.7246893
# b5-b3 -0.8 -4.3722 2.7722 0.9492947
# b5-b4 -2.2 -5.7722 1.3722 0.3384517
# 
# $day
#       diff     lwr    upr     p adj
# d2-d1 -1.4 -4.9722 2.1722 0.7246893
# d3-d1 -1.6 -5.1722 1.9722 0.6230311
# d4-d1 -2.0 -5.5722 1.5722 0.4244632
# d5-d1 -0.2 -3.7722 3.3722 0.9997372
# d3-d2 -0.2 -3.7722 3.3722 0.9997372
# d4-d2 -0.6 -4.1722 2.9722 0.9817474
# d5-d2  1.2 -2.3722 4.7722 0.8177345
# d4-d3 -0.4 -3.9722 3.1722 0.9960338
# d5-d3  1.4 -2.1722 4.9722 0.7246893
# d5-d4  1.8 -1.7722 5.3722 0.5207705
# 
# $ingredients
#     diff        lwr        upr     p adj
# B-A -3.2 -6.7722001  0.3722001 0.0873089
# C-A  0.0 -3.5722001  3.5722001 1.0000000
# D-A -5.4 -8.9722001 -1.8277999 0.0031347
# E-A -5.6 -9.1722001 -2.0277999 0.0023408
# C-B  3.2 -0.3722001  6.7722001 0.0873089
# D-B -2.2 -5.7722001  1.3722001 0.3384517
# E-B -2.4 -5.9722001  1.1722001 0.2648667
# D-C -5.4 -8.9722001 -1.8277999 0.0031347
# E-C -5.6 -9.1722001 -2.0277999 0.0023408
# E-D -0.2 -3.7722001  3.3722001 0.9997372

contrastcoeff= c(1/2,1/2,0,-1/2,-1/2); contrastcoeff
contrastSquared= sum(contrastcoeff^2); contrastSquared
meansum.002= c(latinSquareMeans.001[1], latinSquareMeans.001[2], latinSquareMeans.001[3], latinSquareMeans.001[4], latinSquareMeans.001[5]); meansum.002 
tnum= sum(contrastcoeff*meansum.002); tnum
tStat.001= tnum/sqrt(3.14/5*contrastSquared); tStat.001
criticalT.001 = -qt(p=0.025,df=12); criticalT.001
#tStat = 4.921356 > criticalT = 2.178813
# the evidence does not supoprt the claim that contrast.1 = contrast.2 in L1. I reject the null hyupothesis



# (L2= c.11- c.21, or c.11= c.21)
# H_0: c.11= c.21
# H_a: c.11 != c.21
contrastcoeff.001= c(1/2,1/2,-1/3,-1/3,-1/3); contrastcoeff.001
contrastSquared.003 = sum(contrastcoeff^2); contrastSquared.001
meansum.003= c(latinSquareMeans.001[1], latinSquareMeans.001[2], latinSquareMeans.001[3], latinSquareMeans.001[4], latinSquareMeans.001[5]); meansum.003 
tnum.001= sum(contrastcoeff.001*meansum.003); tnum.001
tStat.002= tnum.001/sqrt(3.14/5*contrastSquared); tStat.002
criticalT.001= -qt(p=0.025,df=20); criticalT.001
# tStat.002 = 2.607898 > criticalT.001 = 2.085963
# the evidence does not supports the claim that c.11 = c.21, thus I reject the null hypothesis

# 5.) 4.23
rep<- c(10,7,5,10,14,18,10,10,7,11,11,12,8,8,9,14);
latin<- c("C","B","A","D","D","C","B","A","A","D","C","B","B","A","D","C");
ord<- c(rep("b1",1), rep("b2",1), rep("b3",1), rep("b4",1));
oper<- c(rep("d1",4), rep("d2",4), rep("d3",4),rep("d4",4));

prob.5<- data.frame(ord,oper,latin,rep);        
prob.5

attach(prob.5)
matrix(latin, 4,4)
#     [,1] [,2] [,3] [,4]
# [1,] "C"  "D"  "A"  "B" 
# [2,] "B"  "C"  "D"  "A" 
# [3,] "A"  "B"  "C"  "D" 
# [4,] "D"  "A"  "B"  "C" 

matrix(rep, 4,4)
#       [,1] [,2] [,3] [,4]
# [1,]   10   14    7    8
# [2,]    7   18   11    8
# [3,]    5   10   11    9
# [4,]   10   10   12   14

prob5Fit<- lm(rep~ oper+ ord+ latin, data= prob.5);
summary(prob5Fit)

prob5Anova<- anova(prob5Fit);
prob5Anova

#Analysis of Variance Table
# Response: rep
#           Df Sum Sq Mean Sq F value   Pr(>F)   
# oper       3   51.5 17.1667  9.8095 0.009926 **
# ord        3   18.5  6.1667  3.5238 0.088519 . 
# latin      3   72.5 24.1667 13.8095 0.004213 **
# Residuals  6   10.5  1.7500                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# the f-value/p-value for operator and latin(method) of assembly are significant at the .01 (.05 too) level, and order of assembly at the .1 level. I would reject the null hypotheiss 
# that all mean values equal zero, 

# 10.) 4.35
#Graeco-Latin Square Design
#input data from the book as column 1, 2, 3 etc.  

rep.002<- c(26,18,20,15,10,16,21,12,15,24,19,18,16,22,17,16,11,25,14,17,13,21,13,17,14); 
latin.002<- c("A","B","C","D","E","B","C","D","E","A","C","D","E","A","B","D","E","A","B","C","E","A","B","C","D");
graeco.002<- c("α","γ","ε","β","δ","β","δ","α","γ","ε","γ","ε","β","δ","α","δ","α","γ","ε","β","ε","β","δ","α","γ");
batch.002<- c(rep("b1",1),rep("b2",1),rep("b3",1),rep("b4",1),rep("b4",1));
acid.002<- c(rep("a1",5),rep("a2",5), rep("a3",5),rep("a4",5),rep("a4",5));

prob.10<- data.frame(acid.002, batch.002, graeco.002, latin.002, rep.002);
prob.10

attach(prob.10)
matrix(latin.002, 5,5);
#     [,1] [,2] [,3] [,4] [,5]
# [1,] "A"  "B"  "C"  "D"  "E" 
# [2,] "B"  "C"  "D"  "E"  "A" 
# [3,] "C"  "D"  "E"  "A"  "B" 
# [4,] "D"  "E"  "A"  "B"  "C" 
# [5,] "E"  "A"  "B"  "C"  "D" 

matrix(graeco.002, 5,5);
#     [,1] [,2] [,3] [,4] [,5]
# [1,] "α"  "β"  "γ"  "δ"  "ε" 
# [2,] "γ"  "δ"  "ε"  "α"  "β" 
# [3,] "ε"  "α"  "β"  "γ"  "δ" 
# [4,] "β"  "γ"  "δ"  "ε"  "α" 
# [5,] "δ"  "ε"  "α"  "β"  "γ" 

matrix(rep.002, 5,5);
#       [,1] [,2] [,3] [,4] [,5]
# [1,]   26   16   19   16   13
# [2,]   18   21   18   11   21
# [3,]   20   12   16   25   13
# [4,]   15   15   22   14   17
# [5,]   10   24   17   17   14

prob10Fit<- lm(rep.002~ acid.002+ batch.002+ graeco.002+ latin.002, data= prob.10);
summary(prob10Fit)

prob10Anova<- anova(prob10Fit)
prob10Anova

# Analysis of Variance Table

# Response: rep.002
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# acid.002    3   21.9    7.30  1.4777 0.2792576    
# batch.002   3    9.9    3.30  0.6680 0.5906769    
# graeco.002  4   12.0    3.00  0.6073 0.6665661    
# latin.002   4  342.8   85.70 17.3482 0.0001703 ***
# Residuals  10   49.4    4.94                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# the F-value for acid, batch and graeco treatment are not significant at the 0.05 level. Therefore I can fail to reject the null hypothesis that the these means do not differ
# however, the latin treatment are statistically profile, and those I would reject, the null that it differs from the other mean values. However, I fail to reject the overall
# hull, because I do not have at least two means that do not differ from zero. 

rep.003<- c(.40,.20,1.14,1.08,1.11,1.04,1.11,1.34,1.16,.57,1.32,1.73,.88,.80,1.38,1.55); 
latin.003<- c("2","3","1","4","3","4","2","1","4","1","3","2","1","2","4","3");
infants.003<- c(rep("b1",1),rep("b2",1),rep("b3",1),rep("b4",1));
week.003<- c(rep("a1",4),rep("a2",4), rep("a3",4),rep("a4",4));

prob.12<- data.frame(infants, week, latin.003, rep.003);
prob.12

matrix(latin.003, 4,4)
#     [,1] [,2] [,3] [,4]
# [1,] "2"  "3"  "4"  "1" 
# [2,] "3"  "4"  "1"  "2" 
# [3,] "1"  "2"  "3"  "4" 
# [4,] "4"  "1"  "2"  "3" 

matrix(rep.003, 4,4)
#     [,1] [,2] [,3] [,4]
# [1,] 0.40 1.11 1.16 0.88
# [2,] 0.20 1.04 0.57 0.80
# [3,] 1.14 1.11 1.32 1.38
# [4,] 1.08 1.34 1.73 1.55


prob12Fit<- lm(rep.003~ infants+ week+ latin.003, data= prob.12)
summary(prob12Fit)

prob12Anova<- anova(prob12Fit)
prob12Anova
# Analysis of Variance Table

# Response: rep.003
#            Df  Sum Sq Mean Sq F value  Pr(>F)  
# infants    3 1.44077 0.48026  9.2095 0.01156 *
# week       3 0.64222 0.21407  4.1051 0.06674 .
# latin.003  3 0.07762 0.02587  0.4961 0.69822  
# Residuals  6 0.31289 0.05215               
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# the rows infant are significant at the 0.05 level and columns week at the 0.1 level. The treatments, however, are not statistically significant at the 0.05 leverl, 
# with a F-vaule of 0.4961 and p-value of 0.69822. Thus, the evidence does not support the claim of the mean treatment level equal to zero. Thus, I reject the null hypothesis. 

# b.) 
# analysis of model replication

replications(rep.003~ infants+ week+ latin.003, data= prob.12)
rep.003<- c(.40,.20,1.14,1.08,1.11,1.04,1.11,1.34,1.16,.57,1.32,1.73,.88,.80,1.38,1.55
            ,1.55,.11,.22,.53,.89,1.05,.96,1.25,.16,.68,1.45,.61,.55,.98,.82,1.91
            ,.27,.50,.32,.09,1.16,.70,1.63,.30,.59,.93,.55,1.34,.45,.96,.79,1.09
            ,.73,.64,-0.03,1.05,1.21,1.38,1.04,1.11,1.21,.82,.57,1.00,.77,.79,.55,.50);
latin.003<- c("2","3","1","4","3","4","2","1","4","1","3","2","1","2","4","3");
week<- c(rep("b1",4),rep("b2",4),rep("b3",16),rep("b4",16));
infants<- c(rep("a1",1),rep("a2",1), rep("a3",1),rep("a4",1));

prob.12b<- data.frame(infants, week, latin.003, rep.003, rep.004, rep.005, rep.006)
prob.12b

prob12bFit.002<- lm(rep.004~ infants+ week+ latin.003, data= prob.12b);
prob12bFit.003<- lm(rep.005~ infants+ week+ latin.003, data= prob.12b);
prob12bFit.004<- lm(rep.006~ infants+ week+ latin.003, data= prob.12b);

prob12bAnova.002<- anova(prob12bFit.002)
prob12bAnova.002
prob12bAnova.003<- anova(prob12bFit.003)
prob12bAnova.003
prob12bAnova.004<- anova(prob12bFit.004)
prob12bAnova.004
#with replication, the F-values and P-values appear to get better, and at least all variables all statistically significant, therefore I reject the null hypothesis, and conclude that row, column and treatment 
#mean are sugnufucantly differnet. 

# Yes my answers are different that original calculated. 

