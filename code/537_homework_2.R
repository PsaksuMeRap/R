# Jason Rich
# H/W # 2
# STAT 537


#setwd("/Users/jasonrich/Documents/GitHub/R/code")
#getwd()

# problem # 2
#reading in the data, with SBP(Y) as the response and QUET, AGE, SMKER as the predictors. n = 32
#istalling reqired libraries
chooseCRANmirror()
install.packages(c("car","MASS"))
library(car)


sbp<-c(135, 122, 130, 148, 146, 129, 162, 160, 144, 180, 166, 138, 152, 138, 140, 
       134, 145, 142, 135, 142, 150, 144, 137, 132, 149, 132, 120, 126, 161, 170, 
       152, 164)
quet<- c(2.876, 3.251, 3.100, 3.768,2.979,2.790, 3.668, 3.612, 2.368, 4.637, 3.877,
         4.032,4.116,3.673, 3.562, 2.998, 3.360, 3.024, 3.171, 3.401, 3.628, 3.751, 
         3.296, 3.210,3.301, 3.017, 2.789, 2.956, 3.800, 4.132, 3.962, 4.010)
age<-c(45, 41, 49, 52, 54, 47, 60, 48, 44, 64,59, 51, 64, 56, 54, 50, 49, 46, 57, 
       56, 56, 58, 53, 50, 54, 48, 43, 43, 63, 63, 62, 65)
smker<-c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 
         0, 1, 0, 1, 0, 0) 

scatterplot(quet, sbp, xlab="QUET", ylab="SBP", title("SBP/QUET"))
#the data fits quiet well with only a few outlyers
scatterplot(age, sbp, xlab="AGE", ylab="SBP", title("SBP/AGE"))
#appears to be more data below the fit line than above, with a few more outlyers than the previous
scatterplot(age, quet, xlab="AGE", ylab="QUET", title("QUET/AGE"))
#this line seems to fit well too, with only a few outlyers
scatterplot(quet, sbp, xlab="QUET", ylab="SBP", main=("SBP/QUET"))

#2.b.1
lsfit(quet,sbp, wt=NULL, intercept = TRUE, tolerance = 1e-07, yname ="SBP")
# $coefficients
# SBP
# Intercept 70.57640
# X         21.49167
# 
# $residuals
# SBP
# [1,]   2.613558
# [2,] -18.445818
# [3,]  -7.200576
# [4,]  -3.557011
# [5,]  11.399916
# [6,]  -1.538159
# [7,]  12.592156
# [8,]  11.795689
# [9,]  22.531325
# [10,]   9.766729
# [11,]  12.100397
# [12,] -19.230811
# [13,]  -7.036112
# [14,] -11.515302
# [15,]  -7.129727
# [16,]  -1.008426
# [17,]   2.211590
# [18,]   6.432791
# [19,]  -3.726485
# [20,]  -1.669568
# [21,]   1.451823
# [22,]  -7.191652
# [23,]  -4.412943
# [24,]  -7.564660
# [25,]   7.479598
# [26,]  -3.416768
# [27,] -10.516667
# [28,]  -8.105776
# [29,]   8.755256
# [30,]  10.620022
# [31,]  -3.726395
# [32,]   7.242005

#?lm
d1<-lm(formula= sbp ~ quet)
plot(d1)
beta.1.hat = 21.49167 
beta.1.hat
summary(d1)
# Call:
#   lm(formula = sbp ~ quet)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -19.231  -7.145  -1.604   7.798  22.531 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   70.576     12.322   5.728 2.99e-06 ***
#   quet          21.492      3.545   6.062 1.17e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.812 on 30 degrees of freedom
# Multiple R-squared:  0.5506,  Adjusted R-squared:  0.5356 
# F-statistic: 36.75 on 1 and 30 DF,  p-value: 1.172e-06

#2.b.2
scatterplot(quet, sbp, xlab="QUET", ylab="SBP", title("SBP/QUET"))
#the data does not fit as well as it could. R-squared is lower than we like (almost a flat slope), but there is room for improvement. 
#Multiple R-squared:  0.5506,  Adjusted R-squared:  0.5356. The relationship between X and Y may not be a linear relationship.

#2.b.3
#H0: slope = 0
#Ha: slope != 0

d1$coeff
qnorm(.95) # critical value
# 1.644854
c.i.plus<-21.492 +1.644854*3.545
c.i.plus 
c.i.minus<-21.492 -1.644854*3.545
c.i.minus
#CI(15.66099, 27.32301)
t.s<-b1/3.545
t.s
#6.062623
#or
#?predict
predict(d1, slope=data.frame(c(21.492)), interval="confidence", level=0.95)
#My test statistic is greater than my critical value, and value is contained within the CI's. The evidence does support the claim
# that slope is zero. Therefore I fail to reject the H0.

#2.b.4
#NO

#2.b.5 
predict(d1, newdata=data.frame(c(21.492)), interval="prediction", level=0.95,se.fit=TRUE, conf.int=FALSE)

