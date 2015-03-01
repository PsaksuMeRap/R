#?paste
paste("Jason Rich, Home Work #2", date())
"Jason Rich, Home Work #2 Tue Sep 30 23:40:45 2014"

#--Residual standard error: 9.812 =sigma(e)= RMSE--> (Residual standard error: 9.812)^2 = MSE*df=SSE

setwd("/Volumes/LaCie/GitHub/R/code")
getwd()

#I prefer glm to lm, unless asking for an ANOVA table, lm() is better  

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

scatterplot(quet, sbp, xlab="QUET", ylab="SBP", title("prob:2"))
#the data fits quiet well with only a few outlyers
scatterplot(age, sbp, xlab="AGE", ylab="SBP", title("prob:2"))
#appears to be more data below the fit line than above, with a few more outlyers than the previous
scatterplot(age, quet, xlab="AGE", ylab="QUET", title("prob:2"))
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
#               Estimate Std. Error t value Pr(>|t|)        |--Std. Error--|
# (Intercept)   70.576     12.322   5.728  2.99e-06 ***      |B(0) = 12.322|
#   quet        21.492      3.545   6.062  1.17e-06 ***      |B(1) = 3.545|
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.812 on 30 degrees of freedom --Residual standard error: 9.812 =sigma(e)= RMSE--> (Residual standard error: 9.812)^2 = MSE*df=SSE
# Multiple R-squared:  0.5506,  Adjusted R-squared:  0.5356 
# F-statistic: 36.75 on 1 and 30 DF,  p-value: 1.172e-06

#2.b.2
scatterplot(quet, sbp, xlab="QUET", ylab="SBP", title("prob2.b"))
#the data does not fit as well as it could. R-squared is lower than we like (almost a flat slope), but there is room for improvement. 
#Multiple R-squared:  0.5506,  Adjusted R-squared:  0.5356. The relationship between X and Y may not be a linear relationship.

#2.b.3
#two-sided test 
#H0: beta.1 = 0
#Ha: beta.1 != 0

d1$coeff
#(Intercept)        quet 
#70.57640    21.49167 

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
prd_001<-predict(d1, newdata=data.frame(c(21.492)), interval="confidence", level=0.95)
prd_001
#My test statistic is greater than my critical value, and value is contained within the CI's, yet zero is not. 
#The evidence does support the claim that slope is zero. Therefore I reject the H0.

#2.b.4
#NO
library(car)
#2.b.5 


predict(d1, se.fit= TRUE, ci.fit = TRUE, pi.fit = TRUE, level=0.95 )
# $fit
# 1        2        3        4        5        6        7        8        9       10       11       12       13       14 
# 132.3864 140.4458 137.2006 151.5570 134.6001 130.5382 149.4078 148.2043 121.4687 170.2333 153.8996 157.2308 159.0361 149.5153 
# 15       16       17       18       19       20       21       22       23       24       25       26       27       28 
# 147.1297 135.0084 142.7884 135.5672 138.7265 143.6696 148.5482 151.1917 141.4129 139.5647 141.5204 135.4168 130.5167 134.1058 
# 29       30       31       32 
# 152.2447 159.3800 155.7264 156.7580 
# 
# $se.fit
# [1] 2.649855 1.860783 2.114377 2.086020 2.385796 2.887257 1.911921 1.837242 4.181013 4.580732 2.323030 2.719697 2.955181 1.919446
# [15] 1.786640 2.340058 1.758126 2.279208 1.981213 1.740276 1.856720 2.053148 1.809128 1.918214 1.804168 2.295385 2.890092 2.442504
# [29] 2.151115 3.001290 2.533499 2.660088
# 
# $df
# [1] 30
# 
# $residual.scale
# [1] 9.811597

#2.b.6
#mean of quet
xbar <- mean(quet)
xbar #3.441094
var(quet)

#critical value
t <- qt(.975, 30)
t # 2.042272

#(x0- xbar)^2
top <- (3.4-xbar)^2
sigma.e.hat<-  9.812 #Residual standard error: 9.812 from the lm() call
f <- 1+1/32
f
#sxx = sum(x(i)-x_bar)^2, or (sigma.e.hat/se(b(1)_hat)^2
#ssx = var(x)*(n-1)--variance of X * the degress of freedom -1
sxx<-(sigma.e.hat/3.545)^2
sxx #7.66095
#standard error
se <- sigma.e.hat*sqrt(f+ (top/sxx))
se #9.965198

y.hat<- 70.576 + (21.492*3.4)
y.hat # 143.6488
pred.inv <-144.5312 + 21.492*(3.4) - (2.042272*0.5048391)
pred.inv 
pred.inv.plus <-144.5312 + 21.492*(3.4) + (2.042272*0.5048391)
pred.inv.plus #144.679

#prediction interval = (142.617,144.679), where the mean of Y is closers to the upper limit of the prediction band. 
#the PI's verify that our data and straight line regression may not be the best model for this data

#2.b.7
#yes, the assumption of normality, and homoskedasticity
#######################

#2.c

lsfit(age,quet, wt=NULL, intercept = TRUE, tolerance = 1e-07, yname ="SBP")
# $coefficients
# SBP
# Intercept 0.38645187
# X         0.05736417
# 
# $residuals
# SBP
# [1,] -0.091839375
# [2,]  0.512617292
# [3,] -0.097296042
# [4,]  0.398611458
# [5,] -0.505116875
# [6,] -0.292567708
# [7,] -0.160301875
# [8,]  0.472068125
# [9,] -0.542475208
# [10,]  0.579241458
# [11,]  0.106062292
# [12,]  0.719975625
# [13,]  0.058241458
# [14,]  0.074154792
# [15,]  0.077883125
# [16,] -0.256660208
# [17,]  0.162703958
# [18,] -0.001203542
# [19,] -0.485209375
# [20,] -0.197845208
# [21,]  0.029154792
# [22,]  0.037426458
# [23,] -0.130752708
# [24,] -0.044660208
# [25,] -0.183116875
# [26,] -0.122931875
# [27,] -0.064111042
# [28,]  0.102888958
# [29,] -0.200394375
# [30,]  0.131605625
# [31,]  0.018969792
# [32,] -0.105122708
# 
# $intercept
# [1] TRUE
# 
# $qr
# $qt
# [1] -19.46576580   2.22170462  -0.11536240   0.41229469  -0.47026726  -0.33180046  -0.06195309   0.44341857  -0.61345754
# [10]   0.71992302   0.19382788   0.72307566   0.19892302   0.13017080   0.11273274  -0.26414337   0.14463760  -0.05101948
# [19]  -0.41861017  -0.14182920   0.08517080   0.11460886  -0.10648629  -0.05214337  -0.14826726  -0.15158143  -0.14567657
# [28]   0.02132343  -0.07029600   0.26170400   0.13848497   0.04614205
# 
# $qr
# Intercept             X
# [1,] -5.6568542 -3.012275e+02
# [2,]  0.1767767  3.872983e+01
# [3,]  0.1767767  7.773532e-02
# [4,]  0.1767767  2.756502e-04
# [5,]  0.1767767 -5.136413e-02
# [6,]  0.1767767  1.293751e-01
# [7,]  0.1767767 -2.062835e-01
# [8,]  0.1767767  1.035552e-01
# [9,]  0.1767767  2.068348e-01
# [10,]  0.1767767 -3.095630e-01
# [11,]  0.1767767 -1.804636e-01
# [12,]  0.1767767  2.609554e-02
# [13,]  0.1767767 -3.095630e-01
# [14,]  0.1767767 -1.030039e-01
# [15,]  0.1767767 -5.136413e-02
# [16,]  0.1767767  5.191543e-02
# [17,]  0.1767767  7.773532e-02
# [18,]  0.1767767  1.551950e-01
# [19,]  0.1767767 -1.288238e-01
# [20,]  0.1767767 -1.030039e-01
# [21,]  0.1767767 -1.030039e-01
# [22,]  0.1767767 -1.546437e-01
# [23,]  0.1767767 -2.554424e-02
# [24,]  0.1767767  5.191543e-02
# [25,]  0.1767767 -5.136413e-02
# [26,]  0.1767767  1.035552e-01
# [27,]  0.1767767  2.326547e-01
# [28,]  0.1767767  2.326547e-01
# [29,]  0.1767767 -2.837431e-01
# [30,]  0.1767767 -2.837431e-01
# [31,]  0.1767767 -2.579232e-01
# [32,]  0.1767767 -3.353829e-01
# 
# $qraux
# [1] 1.176777 1.284294
# 
# $rank
# [1] 2





#?lm
d2<-lm(formula= quet ~ age)
plot(d2)
beta.2.hat = 0.05736417 
beta.2.hat
summary(d2)
#call:
# lm(formula = quet ~ age)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.54248 -0.16601 -0.02293  0.10368  0.71998 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.38645    0.41769   0.925    0.362    
# age          0.05736    0.00778   7.373 3.25e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3013 on 30 degrees of freedom
# Multiple R-squared:  0.6444,  Adjusted R-squared:  0.6326 
# F-statistic: 54.37 on 1 and 30 DF,  p-value: 3.253e-08

#2.c.2
scatterplot(age,quet, xlab="QUET", ylab="SBP", title("prob:2.c.2"))
#the data does not fit as well as it could. R-squared is lower than we like (almost a flat slope), but there is room for improvement. 
#Multiple R-squared:  0.6444,  Adjusted R-squared:  0.6326. The relationship between X and Y may not be a linear relationship.

#2.c.3
#H0: slope = 0
#Ha: slope != 0

d2$coeff
#Intercept)         age 
#0.38645187  0.05736417 

qnorm(.95) # critical value
# 1.644854
c.i.plus<-0.05736417 +1.644854* 0.00778 
c.i.plus 
c.i.minus<-0.05736417 -1.644854* 0.00778 
c.i.minus
#CI(0.04456721, 0.07016113)
t.s<-0.05736417/0.00778
t.s
#1] 7.373287
#or
#?predict
prd_002<-predict(d2, newdata=data.frame(c(0.05736417)), interval="confidence", level=0.95)
prd_002
#My test statistic is greater than my critical value, and value is contained within the CI's. The evidence does support the claim
# that the slope is zero. Therefore I fail to reject the H0.

#2.c.4
#NO
##################################

#2.d

lsfit(age,sbp, wt=NULL, intercept = TRUE, tolerance = 1e-07, yname ="SBP")
# $coefficients
# SBP
# Intercept 59.09162
# X          1.60450
# 
# $residuals
# SBP
# [1,]   3.705875
# [2,]  -2.876125
# [3,]  -7.712125
# [4,]   5.474375
# [5,]   0.265375
# [6,]  -5.503125
# [7,]   6.638375
# [8,]  23.892375
# [9,]  14.310375
# [10,]  18.220375
# [11,]  12.242875
# [12,]  -2.921125
# [13,]  -9.779625
# [14,] -10.943625
# [15,]  -5.734625
# [16,]  -5.316625
# [17,]   7.287875
# [18,]   9.101375
# [19,] -15.548125
# [20,]  -6.943625
# [21,]   1.056375
# [22,]  -8.152625
# [23,]  -7.130125
# [24,]  -7.316625
# [25,]   3.265375
# [26,]  -4.107625
# [27,]  -8.085125
# [28,]  -2.085125
# [29,]   0.824875
# [30,]   9.824875
# [31,]  -6.570625
# [32,]   0.615875
# 
# $intercept
# [1] TRUE
# 
# $qr
# $qt
# [1] -817.5922157   62.1420178   -8.0610448    4.9184112   -0.4286182   -5.7140154    5.5302937   23.6124699   14.3065286
# [10]   16.8362349   11.2038084   -3.4080742  -11.1637651  -11.7756476   -6.4286182   -5.7345595    6.9389552    8.9594993
# [19]  -16.4491623   -7.7756476    0.2243524   -9.1226770   -7.7551035   -7.7345595    2.5713818   -4.3875301   -8.0199567
# [28]   -2.0199567   -0.4902504    8.5097496   -7.8167357   -0.8372798
# 
# $qr
# Intercept             X
# [1,] -5.6568542 -3.012275e+02
# [2,]  0.1767767  3.872983e+01
# [3,]  0.1767767  7.773532e-02
# [4,]  0.1767767  2.756502e-04
# [5,]  0.1767767 -5.136413e-02
# [6,]  0.1767767  1.293751e-01
# [7,]  0.1767767 -2.062835e-01
# [8,]  0.1767767  1.035552e-01
# [9,]  0.1767767  2.068348e-01
# [10,]  0.1767767 -3.095630e-01
# [11,]  0.1767767 -1.804636e-01
# [12,]  0.1767767  2.609554e-02
# [13,]  0.1767767 -3.095630e-01
# [14,]  0.1767767 -1.030039e-01
# [15,]  0.1767767 -5.136413e-02
# [16,]  0.1767767  5.191543e-02
# [17,]  0.1767767  7.773532e-02
# [18,]  0.1767767  1.551950e-01
# [19,]  0.1767767 -1.288238e-01
# [20,]  0.1767767 -1.030039e-01
# [21,]  0.1767767 -1.030039e-01
# [22,]  0.1767767 -1.546437e-01
# [23,]  0.1767767 -2.554424e-02
# [24,]  0.1767767  5.191543e-02
# [25,]  0.1767767 -5.136413e-02
# [26,]  0.1767767  1.035552e-01
# [27,]  0.1767767  2.326547e-01
# [28,]  0.1767767  2.326547e-01
# [29,]  0.1767767 -2.837431e-01
# [30,]  0.1767767 -2.837431e-01
# [31,]  0.1767767 -2.579232e-01
# [32,]  0.1767767 -3.353829e-01
# 
# $qraux
# [1] 1.176777 1.284294
# 
# $rank
# [1] 2


#?lm
d3<-lm(formula= sbp ~ age)
plot(d3)
beta.3.hat = 1.60450
beta.3.hat
summary(d3)
#Call:
# lm(formula = sbp ~ age)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -15.548  -6.990  -2.481   5.765  23.892 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  59.0916    12.8163   4.611 6.98e-05 ***
#   age           1.6045     0.2387   6.721 1.89e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.245 on 30 degrees of freedom
# Multiple R-squared:  0.6009,  Adjusted R-squared:  0.5876 
# F-statistic: 45.18 on 1 and 30 DF,  p-value: 1.894e-07

#2.d.2
scatterplot(age,sbp, xlab="AGE", ylab="SBP", title("prob:2.d.2"))
#the data does not fit as well as it could. R-squared is lower than we like (almost a flat slope), but there is room for improvement. 
#Multiple R-squared:  0.6009,  Adjusted R-squared:  0.5876. The relationship between X and Y may not be a linear relationship.

#2.d.3
#H0: beta.1 = 0
#Ha: beta.1 != 0

d3$coeff
# (Intercept)     age 
# 59.09162     1.60450 

pnorm(.975) # critical value
# 1.644854
c.i.plus<-1.60450 +1.644854* 0.2387 
c.i.plus 
c.i.minus<-1.60450 -1.644854* 0.2387 
c.i.minus
#CI(1.211873, 1.997127); zero is not included
t.s<-1.60450/0.2387
t.s
#6.721827
#or
#?predict
prd_002<-predict(d2, newdata=data.frame(c(0.05736417)), interval="confidence", level=0.95)
prd_002
#My test statistic is greater than my critical value, and value is contained within the CI's, yet zero is not. 
#The evidence does support the claim that the slope is zero. Therefore reject the H0.

#2.d.4
#NO

#################################
#2.e.1
d4<- lm(formula = sbp~smker)
d4
# Call:
#   lm(formula = sbp ~ smker)
# 
# Coefficients:
#   (Intercept)        smker  
# 140.800        7.024 

#2.e.2
cmp<- lsfit(smker,sbp, wt=NULL, intercept = TRUE, tolerance = 1e-07, yname ="SBP")
cmp
n.smk<- c(135, 122, 130, 148, 152, 138, 135, 142, 144, 137, 132, 120, 161, 152, 164) 
mean(n.smk) #140.8

smk<- c(146, 129, 162, 160, 144, 180, 166, 138, 140, 134, 145, 142, 150, 149,132, 126, 170)
mean(smk) #147.8235

#beta_not = 140.800 compared to the mean of the SBP for non_smokers 140.8: this says the beta_not is the mean of SBPfor non_smokers
#which says the line of regression runs the through the mean of non-smokers
#the comparison between the vaules of beta_not and beta_one, is the difference between the mean of non_smokers, and the mean of smokers
#the relationship between sbp and smoking appears to closely correlated to the linear, straight model. 

#2.e.3

#H0: beta.1 = 0
#Ha: beta.1 != 0

d5<- lm(formula = sbp~smker)
summary(d5)
c.i.plus<-7.024 +1.644854* 5.023 
c.i.plus #15.2861
c.i.minus<-7.024 -1.644854* 5.023 
c.i.minus #-1.238102
#(-1.238102,15.2861 )
#zero is within the CI, thus, the evidence does support the claim that the slope is zero. Therefore I fail to reject the H0.

#2.E.4
t.test(n.smk, smk, level=0.95)
#Welch Two Sample t-test

# data:  n.smk and smk
# t = -1.4129, df = 29.963, p-value = 0.168 (high p-value)
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -17.17585   3.12879
# sample estimates:
#   mean of x mean of y 
# 140.8000  147.8235 

#although the two CI are not equal, zero is within both CL's. Therefore, in this scenario, the t.test 
#and the calculated CI produce the same result

###################
#4

library(caret)
library(car)
diy<- c(26.20, 33.00, 17.50, 25.25, 20.30, 31.90, 21.10, 22.70, 10.70, 22.10, 18.60, 35.50, 38.00, 
       30.00, 19.70, 41.10, 39.60, 25.15)

iqx<- c(110, 89, 102, 98, 110, 98, 122, 119, 120, 92, 116, 85, 73, 90, 104, 82, 134, 114 )

#4.a
scatterplot(iqx, diy, xlab="IQ", ylab="DI", title("4.a"))
#the plotted regression line is decreasing as iqx increases
d6<-lm(formula= diy~iqx)
summary(d6)
# Call:
#   lm(formula = diy ~ iqx)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.7013  -4.7454   0.0903   3.6447  20.6838 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  52.2729    12.0485   4.339 0.000508 ***
#   iqx          -0.2489     0.1154  -2.157 0.046535 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.704 on 16 degrees of freedom
# Multiple R-squared:  0.2253,  Adjusted R-squared:  0.1769 
# F-statistic: 4.654 on 1 and 16 DF,  p-value: 0.04653

#4.b

#because Y_hat is an estimation of Y|X. Therefore, we do not have a plotted value for IQ=0 ,and thus the value of beat_not_hat 
#estimates the value of y_hat|x_not.

#4.c

slope_1 <- -0.2489
sd_y.x <- 7.704
sd_x <- 16.192
sqt_n <- sqrt(17)
dom <- sd_x*sqt_n
se_slope_1 <- sd_y.x/dom  
se_slope_1 
#0.1153961


#critical value (t-97.5%, df= 16)
qt(.975, 16)
#2.119905

#confidence intervals
ci_slope <- -0.2489 - (2.119905*0.1153961)
#-0.4935288
ci_slope_2 <- -0.2489 + (2.119905*0.1153961)
#-0.004271231

#4.d

#H0: slope = 0
#Ha: slope != 0

#zero is not within the CI. Thus, at this time, the evidence does not support the claim that the slope = zero.
#Therefore, I reject the H0, that slope(beta_one_hat)=0

#4.e
diy_001<- c(26.20, 33.00, 17.50, 25.25, 20.30, 31.90, 21.10, 22.70, 10.70, 22.10, 18.60, 35.50, 38.00, 
        30.00, 19.70, 41.10, 25.15)

iqx_001<- c(110, 89, 102, 98, 110, 98, 122, 119, 120, 92, 116, 85, 73, 90, 104, 82, 114 )


#full dataset
scatterplot(iqx, diy, xlab="IQ", ylab="DI", title("4.e"))

#comparison

#ommitted dat
scatterplot(iqx_001, diy_001, xlab="IQ_001", ylab="DIY001", title("DIY_001/IQX_001"))
#the outlier appears have the effect of pushing the regression line down. By ommittting the outlier,the line appears to fit the data 
# better, than with the outlier, as seen with an increase in R-squared of the two regressions  

d7 <- lm(formula= diy_001~iqx_001)
# Call:
#   lm(formula = diy_001 ~ iqx_001)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.0505 -2.0768 -0.4287  4.4309  6.6680 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 70.84605    8.59576   8.242 5.95e-07 ***
#   iqx_001     -0.44407    0.08394  -5.291 9.06e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.933 on 15 degrees of freedom
# Multiple R-squared:  0.6511,  Adjusted R-squared:  0.6278 
# F-statistic: 27.99 on 1 and 15 DF,  p-value: 9.065e-05

#4.f

#H0: slope = 0
#Ha: slope != 0


slope_2 <- -0.4407
sd_y.x_2 <- 4.933
sd_x_2 <- 14.693
sqt_n_2<- sqrt(14)
dom_2 <- sd_x*sqt_n
se_slope_2 <- sd_y.x_2/dom_2  
se_slope_2 
#0.07389008

#critical value 
qt(.975, 15)
#2.13145
#confidence intervals
ci_slope_3<- -0.4407 - (2.13145*0.07389008)
#-0.598193
ci_slope_4 <- -0.4407 + (2.13145*0.07389008)
#-0.283207

#zero is not within the CI. Thus, at this time, the evidence does not support the claim that the slope = zero.
#Therefore, I reject the H0, that slope(beta_one_hat)=0


#6
atst <- c(586.00, 461.75, 491.10, 565.00, 462.00, 532.10, 477.60,515.20, 493.00, 528.30, 575.90,532.5, 530.5)
length(atst) #13
age_01 <- c(4.4, 14.00, 10.10, 6.70, 11.50, 9.60, 12.40, 8.90, 11.10, 7.75, 5.50, 8.60, 7.20)


#?lm
d8 <- lm(formula=atst~ age_01)
summary(d8)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  646.483     12.918   50.05 2.49e-14 ***
#   age_01       -14.041      1.368  -10.26 5.70e-07 ***


#?anova.glm
anova(d8)
scatterplot(age_01, atst, xlab="AGE", ylab="ATST", title("Nume:6"))


#6.b
#No, the assumptions appear to valid

#6.c
#H0: slope = 0
#Ha: slope != 0


slope_3 <- -0.4407
sd_y.x_3 <- 13.15
sd_x_3 <- 1.368
sqt_n_3<- sqrt(12)
dom_3 <- sd_x*sqt_n
se_slope_3 <- sd_y.x_3/dom_3  
se_slope_3 
#0.1969703

#t-stat
ts_b <- (-14.041 -0)/0.1969703
ts_b
#-71.28486

#|T| >= t(n-2),(1-alpha) reject the H0
#|-71.28486| > 2.200985 decision rule reject the H0

#critical value 
qt(.975, 11)
#2.200985
#######################
#from problem 2--Need to change the number, but the formulas are good

#se(beta.1.hat)=t(n-2)*sigma.e.hat/sqrt(Sxx)
#mean of quet
xbar <- mean(quet)
xbar #3.441094
var(quet)

#critical value
t <- qt(.975, 30)
t # 2.042272

#(x0- xbar)^2
top <- (3.4-xbar)^2
sigma.e.hat<-  9.812 #Residual standard error: 9.812 from the lm() call
f <- 1+1/32
f
#sxx = sum(x(i)-x_bar)^2, or (sigma.e.hat/se(b(1)_hat)^2
#ssx = var(x)*(n-1)--variance of X * the degress of freedom -1
sxx<-(sigma.e.hat/3.545)^2
sxx #7.66095
#standard error
se <- sigma.e.hat*sqrt(f+ (top/sxx))
se #9.965198

#--CI's for 6, 
ci_slope_5<- -14.041 - ()
#--14.47453
ci_slope_6 <- -14.041 + ()
#-13.60747
#zero is not within the CI. Thus, at this time, the evidence does not support the claim that the slope = zero.
#Therefore, I reject the H0, that slope(beta_one_hat)=0


#6.e
#yes,because zero is not within the CI

#6.f
pred <- predict(d8, newdata=data.frame(age=c(10)), ci.fit=TRUE, level= 0.95)
pred
# 1        2        3        4        5        6        7        8        9       10       11       12       13 
# 584.7027 449.9087 504.6688 552.4083 485.0113 511.6893 472.3743 521.5180 490.6277 537.6652 569.2576 525.7303 545.3878 

#8

sal<- c(10455, 9680, 7300, 9388, 12496, 11812, 9224, 11725, 11320, 12000, 12500, 13310, 12150, 6200, 11522, 8000, 
       12548, 7700, 10028, 13176, 13255, 13004, 8000, 8224, 10750, 11669, 12322, 11002, 10666, 10839)
length(sal)
cgpa <- c(2.58, 2.31, 2.47, 2.52, 3.22, 3.37, 2.43, 3.08, 2.78, 2.98, 3.55, 3.64, 3.72, 2.24, 2.70, 2.30, 2.83, 2.37,
          2.52, 3.22, 3.55, 3.55, 2.47, 2.47, 2.78, 2.78, 2.98, 2.58, 2.58, 2.58)
length(cgpa)

d9 <- glm(formula=sal~cgpa)
summary(d9)
# Call:
#   lm(formula = sal ~ cgpa)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2365.8  -819.1   161.9   862.5  1836.1 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    418.1     1334.8   0.313    0.756    
# cgpa          3637.4      464.7   7.827 1.59e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1122 on 28 degrees of freedom
# Multiple R-squared:  0.6863,  Adjusted R-squared:  0.6751 
# F-statistic: 61.27 on 1 and 28 DF,  p-value: 1.586e-08

scatterplot(cgpa, sal, xlab="SAL", ylab="CGPD", title("Nume:8"))

#the regression line does not appear to fit the data very well

#8.b
#No: independence,linearity, and homoskedasticity are all violated

#8.c (obtain a 95% CI for beta.1)
#beta.1.hat = sxy/sxx(from the regression output)+- t(n-2)|(a/2)*se(beta.1.hat) 
#critical values 

qt(0.975, 28)
#2.048407

slope_4 <- 3637.4 
sd_y.x_4 <- sqrt(1122)
sd_x_4 <- 464.7
sqt_n_4<- sqrt(29)
dom_4 <- sd_x*sqt_n
se_slope_4 <- sd_y.x_4/dom_4  
se_slope_4 
#0.5017316

#confidence intervals
ci_slope_7<- 3637.4 - (1.701131*0.5017316)
#3636.546
ci_slope_8 <- 3637.4 + (1.701131*0.5017316)
#3638.254

#CI:(3636.546,3638.254)

#8.d
#No, because, beta_one=4000 is outside of the confidence intervals

#8.e
# pred_conf <- predict(d8, se.fit=TRUE, ci.fit=TRUE, pi.fit=TRUE, level=0.95) 
# pred_conf
# 
# $fit
# 1        2        3        4        5        6        7        8        9       10       11       12       13 
# 584.7027 449.9087 504.6688 552.4083 485.0113 511.6893 472.3743 521.5180 490.6277 537.6652 569.2576 525.7303 545.3878 
# 
# $se.fit
# [1] 7.342500 7.682869 3.916632 4.869396 4.946841 3.722501 5.849428 3.654187 4.594955 4.062921 6.082558 3.701167 4.445893
# 
# $df
# [1] 11
# 
# $residual.scale
# [1] 13.15238

#8.f
#H0: mu=11500
#Ha: mu!=11500

#?anova
anova(d9)

qt(0.975, 28)
#2.048407

mean(cgpa)
#2.838333
mean(sal)
#10742.17

mse <- 1/28 * 1259141 
mse
#44969.32
rmse <- sqrt(mse)
rmse
#212.0597

#confidence intervals
ci_slope_mum<- 11500 - (2.048407*6.3302)
#11487.03
ci_slope_mup <- 11500 + (2.048407*6.3302)
#11512.97

#at this time, the evidence does support fact that the mu(y|x) does equal 11500. Thus, I fail to reject the H0.

#############################
#page 104-105 
#2

x <- c(-2, -1, 0, 1, 2) 
y <- c(4, 1, 0, 1, 4)

#2.a
# y(i)=b0+b1(sqrt(x(i))
# y(i) and the square of x(i)

#2.b
#y(i)_hat = beta(0)_hat = Y_bar

#2.c
#r = Sxx/Sxy*beta(1)_hat.If beta(1)_hat = 0, r=0

#2.d
#because the mathematical calculateion for the test of H0:rho = 0, ais the same test as beat(1)_hat = 0.
#thu, if we fail to reject the H0:rho=0, we can conjecture that we fail to reject the H0: beta(1)_hat =0


#3

xi <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 20) 
yi <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 20)

d10 <- lm(formula =yi~xi) 
summary(d9)
plot(xi, yi)
abline(d9)
#or
scatterplot(xi, yi, xlab="X", ylab="Y", title("Nume:3 p.105"))

# R^2 = 0.9601
r =sqrt( 0.9601)
r = #0.9798469, from this analysis, the data appears to have a correleary relationship, even with outliers at the 10th obs
  
#3.b
mse_3 <-  11.87903/8
mse_3 #1.484879

res_se_3 <- sqrt(1.484879)
res_se_3 #1.218556 checks is good with the linear model summary

sd_yi <- sd(yi) #5.750362
sd_xi <- sd(xi)

beta_1_hat.3 <- r * sd_yi/sd_xi  
beta_1_hat.3 #0.9798469

var_yi <- var(yi) 
#var_yi = 33.06667
var_xi <- var(xi)
var_xi # 33.06667
mse_yi <- (19/18)* (var_yi - (beta_1_hat.3 ^2 * var_xi)) 
mse_yi #1.392658

#the TS for rho =r(sqrt(n-2))/sqrt(1-r^2) which has a t-distribution with n-2 df. 
#when H0: rho=0, the formula become equivalent to 
#TS = beta_1_hat -0/sd(beta_1_hat), where beta_1 at 0 = 0
# and beta_1 = rho*sd(y)/sd(x). If rho = 0, sp does beta_1, and so on. 

#3.c
#H0: rho = 0
#Ha: rho != 0


qt(.975, 8)
#2.306004
ts_rho <- r*(sqrt(8)/sqrt(1-r^2))
ts_rho #13.87448

# the evidence does not support the claim that the H0 equals zero. Thus,I reject the H0 that rho = 0.

#3.d
#because the data is clustered into the lower left cornor of the graph. The outlier is driving the perception of linearity,
#when data looks more quadratic in nature.

#problem 8

case <- seq(1:20)
case

run_time <- c(195, 215, 243, 162, 185, 231, 234, 166, 253, 196, 220, 168, 207, 225, 169, 215, 147, 230, 208, 172)
run_size <- c(175, 189, 344, 88, 114, 338, 271, 173, 284, 277, 337, 58, 146, 277, 123, 227, 63, 337, 146, 68)

length(run_time)
length(run_size)

#8.1
scatterplot(run_size, run_time, xlab= "Run Time", ylab= "Run Size", title("Nume: 8.1"))

#8.2
p8 <- lm(formula= run_time~run_size)
summary(p8)
# Call:
#   lm(formula = run_time ~ run_size)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -28.597 -11.079   3.329   8.302  29.627 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 149.74770    8.32815   17.98 6.00e-13 ***
#   run_size      0.25924    0.03714    6.98 1.61e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.25 on 18 degrees of freedom
# Multiple R-squared:  0.7302,  Adjusted R-squared:  0.7152 
# F-statistic: 48.72 on 1 and 18 DF,  p-value: 1.615e-06

#8.3
#?aov
#?anova
 anova(p8)
# Analysis of Variance Table
# 
# Response: run_time
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# run_size   1 12868.4 12868.4  48.717 1.615e-06 ***
#   Residuals 18  4754.6   264.1                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#8.4
scatterplot(run_size, run_time, xlab= "Run Time", ylab= "Run Size", title("Nume: 8.4"))
#8.5 & 6

pred_8 <- predict(p8, newdata=data.frame(mean=c(350)), interval="confidence", level=0.95)
pred_8
#       fit      lwr      upr
#1  195.1152 187.2000 203.0305
# 2  198.7447 191.0450 206.4443
# 3  238.9273 225.4549 252.3998
# 4  172.5611 160.8529 184.2693
# 5  179.3014 169.0456 189.5572
# 6  237.3719 224.2825 250.4613
# 7  220.0026 210.6487 229.3565
# 8  194.5968 186.6389 202.5546
# 9  223.3727 213.3984 233.3471
# 10 221.5580 211.9261 231.1900
# 11 237.1126 224.0865 250.1387
# 12 164.7838 151.2147 178.3529
# 13 187.5972 178.8097 196.3847
# 14 221.5580 211.9261 231.1900
# 15 181.6346 171.8338 191.4354
# 16 208.5959 200.7107 216.4811
# 17 166.0800 152.8317 179.3284
# 18 237.1126 224.0865 250.1387
# 19 187.5972 178.8097 196.3847
# 20 167.3762 154.4448 180.3077
pred_9 <- predict(p8, newdata=data.frame(mean=c(350)), interval="prediction", level=0.95)
pred_9
#       fit      lwr      upr
# 1  195.1152 160.0646 230.1659
# 2  198.7447 163.7421 233.7472
# 3  238.9273 202.2204 275.6343
# 4  172.5611 136.4643 208.6578
# 5  179.3014 143.6493 214.9536
# 6  237.3719 200.8038 273.9400
# 7  220.0026 184.5993 255.4058
# 8  194.5968 159.5365 229.6570
# 9  223.3727 187.8005 258.9450
# 10 221.5580 186.0803 257.0358
# 11 237.1126 200.5671 273.6581
# 12 164.7838 128.0413 201.5263
# 13 187.5972 152.3394 222.8550
# 14 221.5580 186.0803 257.0358
# 15 181.6346 146.1107 217.1585
# 16 208.5959 173.5520 243.6397
# 17 166.0800 129.4547 202.7053
# 18 237.1126 200.5671 273.6581
# 19 187.5972 152.3394 222.8550
# 20 167.3762 130.8644 203.8881

