
#paste("Jason Rich", date(), "Test 4")
Jason Rich Wed Nov 12 21:19:51 2014 Test 4


#setwd("/Users/jasonrich/Documents/GitHub/R/Data")
#getwd()


####################################################
#              The set up
####################################################

#?strsplit
#?read.table

d<- read.table("housing.txt", sep=" ", col.names=c("house", "price", "taxes", "bath", "lotsiz", "livspc", "grg", "rm", "bdrm", "age", "frpl"), 
                 fill = FALSE, strip.white=TRUE)

attach(d)

if (F) {
  nrows(d)
  dim(d)
  names(d)
  ls(d)
  View(d)
  str(d)
  summary(d)
  head(d)
}

str(d)
dim(d)
is.data.frame(d)
summary(d)

n <- 26
#n = 11

#drops linked cloumn ("house")
drops <- c("house")
d.1<- d[,!(names(d) %in% drops)]

#other option for dropping factors for the model 
# drop.factors.levels<- function(x){
#                       i.fac<- sapply(y.test, is.factor)
#                       x[, i.fac]<- lapply(x[,i.fac], factor)
#                       return(x)
#                              }

summary(d.1)
View(d.1)
detach(d)

attach(d.1)
View(d.1)

#####################################################################
# System set-up and package install
#####################################################################


chooseCRANmirror()

pkgs<- install.packages()[,1]
pkgs.need<- c("car","MASS", "caret", "ggplot2", "leaps", "segmented")
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
#####################################################################


####################################################
#          Questions 1
####################################################               
#a.) The correlation matrix
?cor
cor(d.1, method = 'pearson')

#           price      taxes       bath     lotsiz     livspc        grg         rm       bdrm         age      frpl
# price   1.0000000  0.9375417  0.9047678  0.7333111  0.9199394  0.4216940  0.6918413 0.66635726 -0.24912181 0.5180665
# taxes   0.9375417  1.0000000  0.8965988  0.7842098  0.9267894  0.5035747  0.8059279 0.70933998 -0.36955991 0.4424523
# bath    0.9047678  0.8965988  1.0000000  0.6745040  0.9230654  0.3831723  0.7577017 0.73957400 -0.22063770 0.4640427
# lotsiz  0.7333111  0.7842098  0.6745040  1.0000000  0.7123521  0.3364187  0.5846073 0.48682380 -0.40531204 0.4777907
# livspc  0.9199394  0.9267894  0.9230654  0.7123521  1.0000000  0.4363865  0.8189356 0.79072938 -0.20554354 0.4267740
# grg     0.4216940  0.5035747  0.3831723  0.3364187  0.4363865  1.0000000  0.6374502 0.60556995 -0.13460954 0.1946183
# rm      0.6918413  0.8059279  0.7577017  0.5846073  0.8189356  0.6374502  1.0000000 0.91171920 -0.11764058 0.3997565
# bdrm    0.6663573  0.7093400  0.7395740  0.4868238  0.7907294  0.6055699  0.9117192 1.00000000  0.03802739 0.2931510
# age    -0.2491218 -0.3695599 -0.2206377 -0.4053120 -0.2055435 -0.1346095 -0.1176406 0.03802739  1.00000000 0.1434669
# frpl    0.5180665  0.4424523  0.4640427  0.4777907  0.4267740  0.1946183  0.3997565 0.29315098  0.14346690 1.0000000



#b.) PRESS function to calculate PRESS residual, PRESS Statistic, and RMS_PRESS
#pr <- resid(fit.1)/(1 - lm.influence(fit.1)$hat) --PRESS residuals (Montgomery 2013, p.471)

r <- resid(fit.1)
r
pr <- r/(1 - lm.influence(fit.1)$hat)
pr
r.2 <- sum(r^2) #249.2003
pr.2 <- sum(pr^2) #709.4283
# RMS_PRESS = sqrt(PRESS/n)
rms.p.2<- sqrt(pr.2/n) 
rms.p.2 #5.223572


#c.) The full Model
#?lm

fit.1 <- lm(price ~  taxes + bath + lotsiz + livspc + grg + rm + bdrm + age + frpl, data = d.1)
summary(fit.1)
plot(fit.1)


#d.) simple(one-variable) straight-line model

fit.2 <- lm(price ~ taxes, data = d.1)
summary(fit.2)
# Call:
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14.2372  -2.3312  -0.3405   3.6460   8.3220 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    2.991      2.827   1.058    0.301    
# taxes          4.951      0.375  13.203 1.69e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 5.173 on 24 degrees of freedom
# Multiple R-squared:  0.879,  Adjusted R-squared:  0.8739 
# F-statistic: 174.3 on 1 and 24 DF,  p-value: 1.687e-12

#------------------------------------------

fit.3 <- lm(price ~ livspc, data = d.1)
summary(fit.3)
# Call:
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.4075  -4.1859   0.1451   4.8577  10.0296 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.713      3.344   0.512    0.613    
# livspc        23.719      2.063  11.495 3.03e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 5.83 on 24 degrees of freedom
# Multiple R-squared:  0.8463,  Adjusted R-squared:  0.8399 
# F-statistic: 132.1 on 1 and 24 DF,  p-value: 3.027e-11

#------------------------------------------

fit.4 <- lm(price ~ rm, data = d.1)
summary(fit.4)
# Call:
#  Residuals:
#   Min      1Q  Median      3Q     Max 
# -24.928  -5.195  -1.195   6.205  26.972 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -17.070     11.885  -1.436    0.164    
# rm             8.111      1.728   4.694 9.04e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 10.74 on 24 degrees of freedom
# Multiple R-squared:  0.4786,  Adjusted R-squared:  0.4569 
# F-statistic: 22.03 on 1 and 24 DF,  p-value: 9.041e-05

#------------------------------------------

fit.5 <- lm(price ~ bdrm, data = d.1)
summary(fit.5)
# Call:
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -21.069  -5.625  -2.372   6.075  24.738 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -5.806     10.202  -0.569 0.574572    
# bdrm          13.194      3.014   4.378 0.000202 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 11.09 on 24 degrees of freedom
# Multiple R-squared:  0.444,  Adjusted R-squared:  0.4209 
# F-statistic: 19.17 on 1 and 24 DF,  p-value: 0.0002019

#------------------------------------------

fit.6 <- lm(price ~ frpl, data = d.1)
summary(fit.6)
# Call:
# Residuals:
#   Min     1Q Median     3Q    Max 
# -18.94  -6.50  -2.50   4.00  35.96 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   32.900      2.998  10.974 7.77e-11 ***
#   frpl          16.037      5.405   2.967  0.00671 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 12.72 on 24 degrees of freedom
# Multiple R-squared:  0.2684,  Adjusted R-squared:  0.2379 
# F-statistic: 8.804 on 1 and 24 DF,  p-value: 0.006707

## for this answer, the model with price as the response and taxes as the predictor is the best one variable model with a Multiple R-squared:  0.879,  Adjusted R-squared:  0.8739, 
#however, the truly predict house prices, I would also add, from the avaiable variables, living space which has an Multiple R-squared:  0.8463,  and an Adjusted R-squared:  0.8399.

# e.) C(p) criterion model selection

View(d.1)
sel.1 <- leaps(x=d.1[,2:10], y=d.1[,1], names=names(d.1)[2:10], method="Cp")
sel.1
#6     TRUE     TRUE  FALSE   TRUE   TRUE  TRUE   FALSE FALSE  TRUE  TRUE <- 7.070, p= 7
price=intercept taxes	bath	lotsiz	livspc	grg	 rm	   bdrm	  age	  frpl


#bonus f.) 

plot(fitted(fit.1), residuals(fit.1))
#?abline
abline(h=0, lty=2)
keep <- fitted(fit.1) < 70
keep
lines(lowess(fitted(fit.1), residuals(fit.1)), lty = 3)
lines(lowess(fitted(fit.1)[keep], residuals(fit.1)[keep]), lty = 2)

#other options to consider when analyzing outlyers in the data
#all OBS
# plot(cooks.distance(fit.1), type = "h")
# plot(hat(model.matrix(fit.1)), type = "h")
# plot(rstudent(fit.1), type = "h")

#OBS 9 and 10 removed
# plot(cooks.distance(fit.1)[keep], type = "h")
# plot(hat(model.matrix(fit.1)[keep]), type = "h")
# plot(rstudent(fit.1)[keep], type = "h")

###################################################
#                Questions 2
###################################################
#set-up, read-in the data, plotsw, and lines

n.1 <- 23
c.v <- qt(.975, df = n.1-2)
# c.v = 2.079614

pd<- read.table("patient.txt", sep=" ", col.names=c("y", "x1", "x2", "x3"), fill = FALSE, strip.white=TRUE)
#another method --read.table("~/documents/GitHub/R/data/patient.txt" header = T)
view(pd)
str(pd)
dim(pd)
is.data.frame(pd)
summary(pd)
#using the same pacakage and libraries as problem one
attach(pd)
plot(pd)

fit.2 <-lm(y~ x1 + x2 + x3, data=pd)
summary(fit.2)

#checking for outlyers in the data
plot(fitted(fit.2), residuals(fit.2))
abline(h=0, lty=1)
keep.1 <- fitted(fit.2) < 100
keep.1
lines(lowess(fitted(fit.2), residuals(fit.2)), lty = 3)
lines(lowess(fitted(fit.2)[keep.1], residuals(fit.2)[keep.1]), lty = 2)

#.a)
#Confidence Intertvals for parameters B.hat(0), B_hat(1), B_hat(2), and B_hat(3)
#set-up
#intercept
b0 <- 162.8759
se.b0 <- 25.7757

#C.I (B.0)
left.0 <- b0-(c.v*se.b0)
right.0 <-b0+(c.v*se.b0)
left.0; right.0
#(109.2724, 216.4794)

#beta hat 1
b1 <- -1.2103
se.b1 <- 0.3015

#C.I (B.1)
left.1 <- b1-(c.v*se.b1)
right.1 <-b1+(c.v*se.b1)
left.1; right.1
#(-1.8373, -0.5832)

#beta hat 2
b2 <- -0.6659
se.b2 <- 0.8210

#C.I (B.2)
left.2 <- b2-(c.v*se.b2)
right.2 <-b2+(c.v*se.b2)
left.2; right.2
#(-2.37326, 1.04146)

#beta hat 3
b3 <- -8.6130
se.b3 <- 12.2413

#C.I (B.3)
left.3 <- b3-(c.v*se.b3)
right.3 <-b3+(c.v*se.b3)
left.3; right.3
#(-34.07018, 16.84418)

#.b)
#refer to the test for results
f.stat <- ((4133-2012)/3)/(2012/19)
f.stat
qf(.95, 3, 19)
#################################################################
#c.) 
#ANOVA
fit.3 <- glm(formula = y ~ x1 + x2 + x3 , data = pd)
fit.3
aov(fit.3)

aov.full <- aov(glm(y~ x1 + x2 + x3), type = "3")
aov.full
aov.1 <- aov(glm(y~ x2), type = "3")
aov.1
aov.2 <- aov(glm(y~ x1 + x2), type = "3")
aov.2
aov.3 <- aov(glm(y ~ x1), type = "3")
aov.3
aov.4 <- aov(glm(y~ x3), type = "3")
aov.4

ssr.1 <- ((3678.436 + 402.784) -(2120.7))/2
f.1 <- ssr.1/2011.6
f.1

ssr.2 <- ((3678.436 + 402.784 + 52.414) - 2120.7)/3
ssr.2
f.2 <- ssr.2/2011.6
f.2

#################################################################
#d.) Model Selection, drop x3|x1,x2
#?dropterm
#full model
#refer to the test documents for the values of the F-test
glm.0 <- glm(formula = y ~ x1 + x2 + x3, data = pd)
summary(glm.0)
dropterm(glm.0, test="F")

glm.1 <- update(glm.0, . ~ . - x3)
summary(glm.1)
dropterm(glm.1, test = "F")
qf(.95, df1 = 1, df2 = 19)
anova(glm.0, glm.1, test = "F")
# Analysis of Deviance Table
# Model 1: y ~ x1 + x2 + x3
# Model 2: y ~ x1 + x2
# Resid.   Df     Resid. Dev-Df Deviance   F     Pr(>F)
# 1        19     2011.6                          
# 2        20     2064.0 -1    -52.414   0.4951  0.4902

#to verify the selection make above, I will use fwd, bckwd and step-wise selection, and check the results against those in the "dropterm" command.

attach(pd)
?step

#backward
step.sel.0 <- step(glm(formula = y ~ x1 + x2 + x3, data = pd) 
                    ,direction = "backward", trace= 1, keep = NULL
                      ,steps = 1000, k = 2)

# Start:  AIC=178.11
# y ~ x1 + x2 + x3
# 
#         Df Deviance     AIC
# - x3    1   2064.0    176.70
# - x2    1   2081.2    176.89
# <none>      2011.6    178.11
# - x1    1   3718.3    190.24
# 
# Step:  AIC=176.7
# y ~ x1 + x2
# 
#         Df  Deviance    AIC
# <none>      2064.0    176.70
# - x2    1   2466.8    178.80
# - x1    1   4024.6    190.06

#forward
step.sel.1 <- step(glm( y ~ 1, data = pd), direction = "forward"
                    ,scale = 0, scope =~ x1 + x2 + x3, trace= 1 
                      ,keep = NULL, steps = 1000, k = 2)

# Start:  AIC=197.79
# y ~ 1
# 
#         Df  Deviance    AIC
# + x1    1    2466.8   178.80
# + x3    1    3915.9   189.43
# + x2    1    4024.6   190.06
# <none>       6145.2   197.79
# 
# Step:  AIC=178.8
# y ~ x1
# 
#         Df  Deviance    AIC
# + x2    1    2064.0   176.70
# + x3    1    2081.2   176.89
# <none>       2466.8   178.80
# 
# Step:  AIC=176.7
# y ~ x1 + x2
# 
#         Df   Deviance   AIC
# <none>        2064.0  176.70
# + x3    1     2011.6  178.11

#step-wise
#note, for stepwise, you can start with either the null model or the full model
step.sel.1 <- step(glm( y ~ x1 + x2 + x3, data = pd), direction = "both"
                    ,scale = 0, trace= 1, keep = NULL, steps = 1000
                      ,k = 2)

# Start:  AIC=178.11
# y ~ x1 + x2 + x3
# 
#         Df Deviance    AIC
# - x3    1   2064.0    176.70
# - x2    1   2081.2    176.89
# <none>      2011.6    178.11
# - x1    1   3718.3    190.24
# 
# Step:  AIC=176.7
# y ~ x1 + x2
# 
#         Df  Deviance    AIC
# <none>      2064.0    176.70
# + x3    1   2011.6    178.11
# - x2    1   2466.8    178.80
# - x1    1   4024.6    190.06

#################################################################
#e.) drop x2, x3|x1
#starting our full model
summary(glm.0)
# Call:
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -16.954   -7.154    1.550    6.599   14.888  
# 
# Coefficients:
#             Estimate  Std. Error t value  Pr(>|t|)    
#(Intercept) 162.8759    25.7757   6.319    4.59e-06 ***
#x1           -1.2103     0.3015  -4.015    0.00074 ***
#x2           -0.6659     0.8210  -0.811    0.42736    
#x3           -8.6130    12.2413  -0.704    0.49021    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for gaussian family taken to be 105.8729)
# 
# Null deviance: 6145.2  on 22  degrees of freedom
# Residual deviance: 2011.6  on 19  degrees of freedom
# AIC: 178.11
# Number of Fisher Scoring iterations: 2

glm.2 <- update(glm.0, . ~ . -x2 - x3)
summary(glm.2)
dropterm(glm.2, test="F")

anova(glm.0, glm.2, test = "F")
# Analysis of Deviance Table
# Model 1: y ~ x1 + x2 + x3
# Model 2: y ~ x1
# Resid.   Df     Resid. Dev-Df Deviance   F    Pr(>F)
# 1        19     2011.6                          
# 2        21     2466.8 -2   -455.2     2.1497  0.144

qf(.95, df1 = 2, df2 = 20)

detach(pd)

#################################################################
#                     Question 3
#################################################################

prob.3 <- read.table("~/documents/GitHub/R/data/prob_3.txt", header =T)
attach(prob.3)

#a.)
?cor.test
#correlation y~x1
cor.test(x1, y, data = prob.3, method = "pearson")
#0.9839041 


#correlation y~x2
cor.test(x2, y, data = prob.3, method = "pearson")
#2.123157e-15 

#b.)
fit.4 <- lm(formula = y ~ x1 + x2, data = prob.3 )
summary(fit.4)

#hyp testing
#full model anova
m.1 <- aov(glm(formula = y ~ x1 + x2, data = prob.3))
summary(m.1)

m.2 <- aov(glm(formula= y~1, data = prob.3))
summary(m.2)

ssr.p1 <- ((0.00513 + 0.16941) - (0.175))/2
ssr.p1/0.00045 
qf(.975, df1 = 2,df2 = 4)

#c.) refer to the scan

#d.)
summary(fit.4)
fit.5 <- lm(formula = y ~ x1, data = prob.3)
summary(fit.5)
