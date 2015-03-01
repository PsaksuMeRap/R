paste("Jason Rich", date(), "Homework 3")
"Jason Rich Sat Oct 10 11:20:28 2014  Homework 3"

#setwd(../)
#getwd()

################ CHAPTER 8 #################

#problem 1.)
chooseCRANmirror()
install.packages(c("ggplot2","car", "MASS"))
library(ggplot2)
library(caret)
library(MASS)

#variables
#1-> SBP = systolic blood pressure
#2-> QUET = body size
#3-> AGE
#4-> SMKER = smoking history: 1 if smoker, 0 o/w


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
############################################################
#Model(1): SBP(Y)=B(0) + B(1)AGE(X(1)) 
glm.001<- glm(sbp ~ age)
glm.001
summary(glm.001)
# Call:
#   glm(formula = sbp ~ age)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -15.548   -6.990   -2.481    5.765   23.892  
# 
# Coefficients:
#                Estimate    Std. Error t value Pr(>|t|)    
# (Intercept)     59.0916    12.8163   4.611 6.98e-05 ***
#   age            1.6045     0.2387   6.721 1.89e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 85.47795)
# 
# Null deviance: 6426.0  on 31  degrees of freedom
# Residual deviance: 2564.3  on 30  degrees of freedom
# AIC: 237.09
# 
# Number of Fisher Scoring iterations: 2

anova.glm.001<- aov(glm(formula=sbp ~ age))
anova.glm.001
summary(anova.glm.001)

#              Df Sum Sq Mean Sq F value   Pr(>F)    
#   age          1   3862    3862   45.18 1.89e-07 ***
#   Residuals   30   2564      85                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


lm.001<-lm(sbp~age)
summary(lm.001)
# Multiple R-squared:  0.6009,  Adjusted R-squared:  0.5876 
# F-statistic: 45.18 on 1 and 30 DF,  p-value: 1.894e-07
############################################################
#Model(2): SBP(Y)=B(0) + B(1)*AGE(X(1))+B(2)*SMKER(X(2)) 
glm.002<- glm(sbp ~ age + smker)
glm.002
summary(glm.002)
# Call:
#   glm(formula = sbp ~ age + smker)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -10.639   -5.518   -1.637    4.900   19.616  
# 
# Coefficients:
#               Estimate    Std. Error t value Pr(>|t|)    
# (Intercept)     48.0496    11.1296   4.317 0.000168 ***
#   age            1.7092     0.2018   8.471 2.47e-09 ***
#   smker         10.2944     2.7681   3.719 0.000853 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 59.87188)
# 
# Null deviance: 6426.0  on 31  degrees of freedom
# Residual deviance: 1736.3  on 29  degrees of freedom
# AIC: 226.61
# 
# Number of Fisher Scoring iterations: 2

anova.glm.002<- aov(glm(formula=sbp ~ age + smker))
anova.glm.002
summary(anova.glm.002)
#                Df Sum Sq Mean Sq F value   Pr(>F)    
#   age          1   3862    3862   64.50 7.41e-09 ***
#   smker        1    828     828   13.83 0.000853 ***
#   Residuals   29   1736      60                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lm.002<-lm(sbp~age + smker)
summary(lm.002)
# Residual standard error: 7.738 on 29 degrees of freedom
# Multiple R-squared:  0.7298,  Adjusted R-squared:  0.7112 
# F-statistic: 39.16 on 2 and 29 DF,  p-value: 5.746e-09

############################################################
#Model(3): SBP(Y)=B(0) + B(1)*AGE(X(1))+B(2)*SMKER(X(2)+B3*QUET(X(3))) 
glm.003<- glm(sbp ~ age + smker + quet)
glm.003
summary(glm.003)
# Call:
#   glm(formula = sbp ~ age + smker + quet)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -13.5420   -6.1812   -0.7282    5.2908   15.7050  
# 
# Coefficients:
#               Estimate   Std. Error t value Pr(>|t|)    
# (Intercept)    45.1032    10.7649   4.190 0.000252 ***
#   age           1.2127     0.3238   3.745 0.000829 ***
#   smker         9.9456     2.6561   3.744 0.000830 ***
#   quet          8.5924     4.4987   1.910 0.066427 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 54.86225)
# 
# Null deviance: 6426.0  on 31  degrees of freedom
# Residual deviance: 1536.1  on 28  degrees of freedom
# AIC: 224.69
# 
# Number of Fisher Scoring iterations: 2

anova.glm.003<- aov(glm(formula=sbp ~ age + smker + quet))
anova.glm.003
summary(anova.glm.003)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#   age       1   3862    3862  70.388  3.99e-09 ***
#   smker     1    828     828  15.093  0.000571 ***
#   quet      1    200     200   3.648  0.066427 .  
# Residuals  28   1536      55                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lm.003<-lm(sbp~age + smker + quet)
summary(lm.003)
# Residual standard error: 7.407 on 28 degrees of freedom
# Multiple R-squared:  0.7609,  Adjusted R-squared:  0.7353 
# F-statistic: 29.71 on 3 and 28 DF,  p-value: 7.602e-09

############################################################
#A1.) 
coef(glm.003)
y.hat.011<-45.103192 + (1.212715*50) + (9.945568*1) + (8.592449*3.5)
y.hat.011 #145.7581

#A2.) 
y.hat.012<-45.103192 + (1.212715*50) + (9.945568*0) + (8.592449*3.5)
y.hat.012 #135.8125

#A3.)
y.hat.013<-45.103192 + (1.212715*50) + (9.945568*1) + (8.592449*3.0)
y.hat.013 #141.4619

#As quet increases from 3.0 to 3.5, a smokers sbp increases from 141.5 to 145.8. That is, a smoker whose weight increaes [quet(100(weight/height))] also risks sbp increase.  

Ok

#B.) 

#R^2 = sum(y.i.hat - y.bar)^2/sum(y.1 - y.bar)^2

#model 1: R^2 = 0.6009
#model 2: R^2 = 0.7298  
#model 3: R^2 = 0.7609

# using both R^2 and model simplicity, model 2 offers us the best explanation of association. 

Nice
############################################################


#problem 2.) 

#A.)
x.1<- 2.8
x.2<-7.0
y.hat.021<- -.0635 + (23.451*x.1) -(7.073*x.2)   # based on the data, replace -.0635 to -.635
y.hat.021 #16.0883

#The level of pathology is approximately nine units smaller than that of patient #5. 

#B.) 
rsqr.021<- 1-(12255.3128/13791.1698)
rsqr.021 #0.1113652

rsqr.022<- 1-(13633.3225/13791.1698)
rsqr.022 #0.01144553

rsqr.023<- 1-(11037.2985/13791.1698)
rsqr.023 #0.1996837

# using both R^2 and model simplicity, model 1 offers us the best explanation of association. 

Okay

#problem 3.) 

#A and B.)

y.hat.031<- 77.983 + (0.417*70) + (5.217*30)
y.hat.031 #263.683

rsqr.031<- 1-(42806.2254/145377.0400)
rsqr.031 #0.7055503

y.hat.032<- 199.2975 + (1.622*70) 
y.hat.032 # 312.8375

rsqr.032<- 1-(135145.3138/145377.0400)
rsqr.032 #0.07038062


y.hat.032<- 102.5751 + (5.321*30)
y.hat.032 #262.2051

rsqr.033<- 1-(43444.3743/145377.0400)
rsqr.033 # 0.7011607

# A.) model 1 is the closest y.hat value to the predicted value of y for patient 4.
# B.) Furthermore, model 1 has a higher r^2 value than the other two models. Using both R^2 and model simplicity, model 1 offers us the best explanation of association. 

ok


#problem 4.) 

#A.)
rsqr.041<- 1-(537.4036/1855.2020)
rsqr.041 #0.7103261

rsqr.042<- 1-(431.9700/1855.2020)
rsqr.042 #0.7671574

rsqr.043<- 1-(367.3426/1855.2020)
rsqr.043 #0.8019932

#Using both R^2 and model simplicity, model 3 offers us the best explanation of association, with an R^2 of 0.8019932.

#B.)

rsqr.044<- 1-(337.0571/1855.2020)
rsqr.044 #0.8183178

#The addition of the X(1) independent variable adds another element of explantion. The increase is R^2 to 0.82 indicates that X(1) contains explanatory information WRT Y.

#C.) 
rsqr.045<- 1-(364.630721/1855.2020)
rsqr.045 #0.803455
# yes, the addtion of the X(4) variable does lead to an improvement over the model with only X(2) and X(3), although I would NOT say a large improvement. 
#The interaction term, however, does add a predictive element missing from the model with only X(2) and X(3).
Add "NOT" above
Predictive element of interaction is useful only if X2 and X3 are independent.
Also look at the AIC, and AdjRsq
 
#problem 5.)

#see attached written answer

Ok

#problem 11.) 


#A.)
#sales= beta(0) + beta(1)tae + beta(2)pae +e(i)


#B.)
sales<- c(4.0, 8.0, 2.0, 8.0, 5.0, 4.0)
tae<- c(1.5, 4.5, 0, 5.0, 3.0, 1.5)
pae<- c(5.0, 5.0, 0, 1.0, 1.0, 1.5)

lm.111<- lm(sales~tae + pae)
summary(lm.111)
# Call:
#   lm(formula = sales ~ tae + pae)
# 
# Residuals:
#   1       2       3       4       5       6 
# -0.2051  0.2230  0.1778  0.1052 -0.5135  0.2127 
# 
# Coefficients:
#               Estimate      Std. Error t value Pr(>|t|)    
# (Intercept)    1.82222    0.30399   5.994 0.009297 ** 
#   tae          1.19064    0.09191  12.955 0.000993 ***
#   pae          0.11938    0.08136   1.467 0.238558    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3845 on 3 degrees of freedom
# Multiple R-squared:  0.9846,  Adjusted R-squared:  0.9744 
# F-statistic: 96.04 on 2 and 3 DF,  p-value: 0.001907

# for every $1000 increase in tae $1800 in sale revenue. Where a $1000 in pae only derive a $119 increase in sales. 

#C.) 
#R^2 =  0.9846. the model appears to fit well, with the X's regressed on Y. 

#D.)

y.hat.111<- 1.82222 + (1.19064*5.0) + (0.11938*1.0)
y.hat.111 #7.8948 predicted sales are $7.9 million 


############### CHAPTER 9 ######################

#problem 1.)
#H(0): B(0)=B(1)=0
#H(a): at least one is not equal to zero
summary(lm.001)
 
# Residual standard error: 9.245 on 30 degrees of freedom
# Multiple R-squared:  0.6009,  Adjusted R-squared:  0.5876 
# F-statistic: 45.18 on 1 and 30 DF,  p-value: 1.894e-07

#the evidence does not supports the claim of the H(0) at the alpha=.5 significance level. Thus, I reject the H(0)


#H(0): B(0)=B(1)=B(2)=0
#H(a): at least one is not equal to zero
summary(lm.002)

# Residual standard error: 7.738 on 29 degrees of freedom
# Multiple R-squared:  0.7298,  Adjusted R-squared:  0.7112 
# F-statistic: 39.16 on 2 and 29 DF,  p-value: 5.746e-09

#the evidence does not supports the claim of the H(0) at the alpha=.5 significance level. Thus, I reject the H(0)


#H(0): B(0)=B(1)=B(2)=B(3)=0
#H(a): at least one is not equal to zero
summary(lm.003)

# Residual standard error: 7.407 on 28 degrees of freedom
# Multiple R-squared:  0.7609,  Adjusted R-squared:  0.7353 
# F-statistic: 29.71 on 3 and 28 DF,  p-value: 7.602e-09

#the evidence does not supports the claim of the H(0) at the alpha=.05 significance level. Thus, I reject the H(0)

#B.) Answer is the same as in 8A. 

Best model??

#problem 3.) 

#n=4
mse.001<-422.292/18
mse.001 #23.46067

#H(0): B1=0
#H(a): B1!=0
f.x.1<-981.326/mse
f.x.1 #41.82856
#F-stat
?? I do not see y=how you got the F value. Please explan more and compare with solution

pf(.05, 1, 20) #0.1746685
#the evidence does not supports the claim of the H(0) at the alpha=.05 significance level. Thus, I reject the H(0)


mse.002<-422.292/19
mse.002 #22.22589

#H(0): B2=0|X1
#H(a): B2!=0|X1
f.x.2<-190.232/mse.002
f.x.2 #8.559026
Same issue again with the F-value

#F-stat

pf(.05, 1, 19) #0.1745501
#the evidence does not supports the claim of the H(0) at the alpha=.05 significance level. Thus, I reject the H(0)


mse.003<-422.292/20
mse.003 #21.1146

#H(0): B3=0|X1,X2
#H(a): B3!=|X1,X2
f.x.3<-37.982/mse.003
f.x.3 #1.79885
pf(.05, 1, 18) #0.1744185

#the evidence does not supports the claim of the H(0) at the alpha=.05 significance level. Thus, I reject the H(0)

#B.)
#H(0): B2=B3=0|X1
#H(a):  at least 1 B(i)|X1 != 0

#Careful in writing the alternative. The description from the writing is not exact.

f<- (((981.326+190.232+129+431)-981.326)/3)/442.292
f #0.5654123

pf(.05, 1, 18) #0.1744185
#the evidence does not supports the claim of the H(0) at the alpha=.05 significance level. Thus, I reject the H(0)

#C.)
#H(0): B1=B2=B3=0
#H(a):  at least 1 B(i) != 0

#the appropriate test is the partial F-test. Unfortunately, this test can't be accomplished because the necessary SS(reduced) calucations for the models in question,
#are not provided.

#D.) Based on the test conducted, model three, X3|X1, X2 is the most appropriate model at the alpha=.05 level.

#problem 5.)

y.095<- c(47,38,47,39,44,64,58,49,55,52,49,47,40,42,63,40,59,56,76,67,57,57,42,54,60,33,55,36,36,42,41,42,39,27,31,39,56,40,58,43,40,46)
length(y.095)#42

x.195<- c(287,236,255,135,121,171,260,237,261,397,295,261,258,280,339,161,324,171,265,280,248,192,349,263,223,316,
          288,256,318,270,262,264,325,388,260,284,326,248,285,361,248,280)
length(x.195)#42

x.295<-c(111,135,98,63,46,103,227,157,266,167,164,119,145,247,168,68,92,56,240,306,93,115,408,103,102,274,130,149,180,134,154,86,148,191,123,135,236,92,153,126,226,176)
length(x.295)#42

x.395<-c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1)
length(x.395)#42

########################## MODELS ##########################

############## model:1 #############

#H(0): B(1)=0
#H(a): B(1)!= 0

y.hat.1<-lm(formula=y.095~x.195)
summary(y.hat.1)
#Residual standard error: 10.69 on 40 degrees of freedom
# Multiple R-squared:  0.01002,  Adjusted R-squared:  -0.01473 
F-statistic: 0.4049 on 1 and 40 DF,  p-value: 0.5282

#the evidence does supports the claim of the H(0) at the alpha=.05 significance level. Thus, I fail to reject the H(0)


############## model:2 #############
#H(0): B(2)=0
#H(a): B(2)!= 0

y.hat.2<-lm(formula=y.095~x.295)
summary(y.hat.2)
# Residual standard error: 10.71 on 40 degrees of freedom
# Multiple R-squared:  0.004625,  Adjusted R-squared:  -0.02026 
F-statistic: 0.1859 on 1 and 40 DF,  p-value: 0.6687

#the evidence does supports the claim of the H(0) at the alpha=.05 significance level. Thus, I fail to reject the H(0)


############## model:3 #############
#H(0): B(3)=0
#H(a): B(3)!= 0

y.hat.3<-lm(formula=y.095~x.395)
summary(y.hat.3)

# Residual standard error: 9.847 on 40 degrees of freedom
# Multiple R-squared:  0.1594,  Adjusted R-squared:  0.1383 
F-statistic: 7.583 on 1 and 40 DF,  p-value: 0.008822

#the evidence does not supports the claim of the H(0) at the alpha=.05 significance level. Thus, I reject the H(0)


#B.)
############## model:4 #############
#H(0): B(1)=B(2)=B(3)=0
#H(a): at least 1 B(j)!= 0

y.hat.4<-lm(formula=y.095~x.195+x.295+x.395)
summary(y.hat.4)

# Residual standard error: 9.992 on 38 degrees of freedom
# Multiple R-squared:  0.1777,  Adjusted R-squared:  0.1128 
F-statistic: 2.737 on 3 and 38 DF,  p-value: 0.05682

#the evidence does supports the claim of the H(0) at the alpha=.05 significance level. Thus, I fail to reject the H(0)

#C.)

############## Model ###############
#Y=B(0)+B(1)X(1)+B(2)X(2)+B(3)X(3)+e(i)
#vs'
#Y=B(0)+B(1)X(1)+B(2)X(2)+B(3)X(3)+B(4)[X(1)X(3)]+B(5)[X(2)X(3)]+e(i)

#H(0): B(4)=B(5)=0
#H(a): at least one B(j)!=0 (j=4,5)


model.lm1<-lm(y~x1+x2+ x3, data=data5)
model.lm2<-lm(y~x1+x2+ x3+ x4+ x5, data=data5)
anova(model.lm1, model.lm2)
#to test the null hypothesis: x4=x5=0;


#Another option is to do it as follows:
model.lm1<-lm(y~x1+x2+ x3+ x4+ x5, data=data5)
model.lm2<-update(a.lm ~. -x4-x5, data=data5) # this update deletes the terms that are not needed in the model.
anova(model.lm1, model.lm2, test="F")



f.B4<- .47
f.B5<-.25
f.B4.B5<-(f.B4 + f.B5)/2
f.B4.B5 #0.36

pf(.025, 2, 36) #0.02467317

#the evidence does supports the claim of the H(0) at the alpha=.05 significance level. Thus, I fail to reject the H(0)
#since the test is not rejected, I can concluded that both interection do not significantly add to the model ability to predict Y, and thus, X3 does not have a significant
#association with  Y when X(3) = 1, and nulls the interaction term when equal to 0. 

#D.) 
#Y=B(0)+B(1)X(1)+B(2)X(2)+e(i)
#vs'
#Y=B(0)+B(1)X(1)+B(2)X(2)+B(3)X(3)+e(i)

#H(0): B(3)=0
#H(a): B(3)!=0

y.hat.6<-lm(formula=y.095~x.195+x.295)
y.hat.4
#partial F-test
anova(y.hat.6, y.hat.4)

# Analysis of Variance Table
# 
# Model 1: y.095 ~ x.195 + x.295
# Model 2: y.095 ~ x.195 + x.295 + x.395
#       Res.Df    RSS     Df  Sum of Sq      F    Pr(>F)  
# 1     39      4478.2                              
# 2     38      3793.9    1    684.37     6.8547  0.01263 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#6.8547; 0.01263
pf(.05, 2, 39) #0.0487097
#the evidence does not supports the claim of the H(0) at the alpha=.05 significance level. Thus, I reject the H(0)


#problem 10

sales<- c(4.0, 8.0, 2.0, 8.0, 5.0, 4.0)
tae<- c(1.5, 4.5, 0, 5.0, 3.0, 1.5)
pae<- c(5.0, 5.0, 0, 1.0, 1.0, 1.5)

lm.109<- lm(sales~tae + pae)


#A.)
anova(lm.109)
# Analysis of Variance Table
# 
# Response: sales
#           Df  Sum Sq Mean Sq  F value    Pr(>F)    
# tae        1 28.0716 28.0716 189.9192 0.0008269 ***
# pae        1  0.3183  0.3183   2.1532 0.2385578    
# Residuals  3  0.4434  0.1478                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#tae appears to predict sale better than pae. This is aligned with my earlier analysis

#B.)

#Y=B(0)+B(1)X(1)
#Y=B(0)+B(1)X(1)+B(2)X(2)

####  X(1) model
y.hat.9.10.1<-lm(formula=sales ~ tae)
anova(y.hat.9.10.1)
# Analysis of Variance Table
# 
# Response: sales
#           Df  Sum Sq Mean Sq F value   Pr(>F)    
# tae        1 28.0716 28.0716  147.42 0.000264 ***
# Residuals  4  0.7617  0.1904                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

######## FULL MODEL ####################
y.hat.9.10.2<-lm(formula=sales ~ tae+pae)
anova(y.hat.9.10.2)
# Analysis of Variance Table
# 
# Response: sales
#           Df  Sum Sq Mean Sq  F value    Pr(>F)    
# tae        1 28.0716  28.0716 189.9192  0.0008269 ***
# pae        1  0.3183  0.3183   2.1532   0.2385578    
# Residuals  3  0.4434  0.1478                       
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#### X(2) Model
y.hat.9.10.3<-lm(formula=sales ~ pae)
anova(y.hat.9.10.3)
# Analysis of Variance Table
# 
# Response: sales
# Df  Sum Sq Mean Sq F value Pr(>F)
# pae        1  3.5838  3.5838  0.5677 0.4931
# Residuals  4 25.2496  6.3124 



#add-in-first with X(1)
anova(y.hat.9.10.1, y.hat.9.10.2)
# Analysis of Variance Table
# 
# Model 1: sales ~ tae
# Model 2: sales ~ tae + pae
#       Res.Df   RSS      Df   Sum of Sq   F     Pr(>F)
# 1      4      0.76169                           
# 2      3      0.44342    1   0.31827   2.1532   0.2386


#add-in-first with X(2)
anova(y.hat.9.10.3, y.hat.9.10.2)
# Analysis of Variance Table
# 
# Model 1: sales ~ pae
# Model 2: sales ~ tae + pae
#       Res.Df     RSS    Df    Sum of Sq      F    Pr(>F)    
# 1      4        25.2496                                 
# 2      3         0.4434  1    24.806      167.83  0.000993 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#D.)
#added-in-last
anova(y.hat.9.10.2, y.hat.9.10.1)
# Analysis of Variance Table
# 
# Model 1: sales ~ tae + pae
# Model 2: sales ~ tae
#       Res.Df     RSS  Df  Sum of Sq   F   Pr(>F)
# 1      3      0.44342                           
# 2      4      0.76169 -1  -0.31827  2.1532 0.2386

anova(y.hat.9.10.2, y.hat.9.10.3)
# Analysis of Variance Table
# 
# Model 1: sales ~ tae + pae
# Model 2: sales ~ pae
#       Res.Df     RSS    Df    Sum of Sq      F    Pr(>F)    
# 1      3        0.4434                                 
# 2      4        25.2496 -1   -24.806 1      67.83 0.000993 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#E.)
# It appears that X(1) has the greater predicting power, or better association to SALES(Y). The added in last shows that when I remove pae from the model, the SS reduces by .032
#However, when I add in last tae, SS is by 24.81. The significance of this reduction indicates that both predictor are necessary to accurately predict sales. 
