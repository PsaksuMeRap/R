
## Stat 437-537   ##
## Norou Diawara  ##
## R file         ##
## Stat 437-537   ##

help()
q()

2+2
exp(0); exp(9)
log(100, base 10)
pi

# Homework 1 

# Problem 4 Chapter 3
qchisq(0.99, 7)
pchisq(14,12)

# Problem 6 Chapter 3
qf(0.95, 6, 24)
pf(2.9, 5, 40)
1-pf(2.9, 5, 40)

# Problem 12 Chapter 3
qt(.975, 27)
qt(.025, 27) # and change the sign associated
pt(1.41, 30)
1-pt(1.41, 30)

# Assuming normal distribution, take
qnorm(0.5) ; qnorm(.975)
pnorm(1.28); pnorm(2.5)

# Problem 16 Chapter 3
data1<-c(132, 145, 124, 122, 165, 144, 151)
data2<-c(141,139, 172, 131, 150, 125)
n1<-length(data1); n2<-length(data2)
xbar1<-mean(data1); xbar2<-mean(data2);
xbar1; xbar2

s1<-sd(data1); s2<-sd(data2); s1; s2
xbar1+qnorm(0.975)*s1/sqrt(n1)
xbar1-qnorm(0.975)*s1/sqrt(n1)

sp<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
sp^2; sp
t=(xbar1-xbar2)/(sp * sqrt(n1^(-1)+n2^(-1) )); t

# t.test performs a one or two sample t-test
t.test(data1, data2)
t.test(data1, data2, var.equal=TRUE)
pt(3.29, df=11)
pt(0.29, df=11)

var.test(data1, data2) # Test for equality of variances.
# Equality of variances means that the ratio of the two variances is 1.
# So the null hypothesis says that: Ho: the ratio of variances is 1
# The alternative says that: Ha: the ratio variance is not 1.
# The test has a p-value of 0.85. So Do not reject the Ho.
# and proceed with a test with equality of variance assumed.
# The sample estimate of the ratio is: 0.8646


# Data on systolic blood pressure (sbp) and age of 30 individuals available in textbook page 51

sby<-c(144, 220, 138, 145, 162, 142, 170, 124, 158, 154, 162, 150, 140, 110, 128, 
130, 135, 114, 116, 124, 136, 142, 120, 120, 160, 158, 144, 130, 125, 175)
age<-c(39, 47, 45, 47, 65, 46, 67, 42, 67, 56, 64, 56, 59, 34, 42, 48, 45, 17,
20, 19, 36, 50, 39, 21, 44, 53, 63, 29, 25, 69)

plot(age, sby)
plot(age, sby, xlab="age of the individuals", ylab="systolic blood pressure", title("Scatter plot data"))

# Here is another example of data: salt concentration in surface streams (Y variable) 
# and percentage of watershed area consisting of paved roads(X variable)
salt<-c(3.8, 5.9, 14.1, 10.4, 14.6, 14.5, 15.1, 11.9, 15.5, 9.3, 
15.6, 20.8, 14.6, 16.6, 25.6, 20.9, 29.9, 19.6, 31.3, 32.7)
road<-c(0.19, 0.15, 0.57, 0.40, 0.70, 0.67, 0.63, 0.47, 0.75, 0.60,
0.78, 0.81, 0.78, 0.69, 1.30, 1.05, 1.52, 1.06, 1.74, 1.62)

plot(road, salt)  # The scatter plot suggests a linear model to understand the mechanism betwen x and y

# Group of 26 adults surveyed for their weight and blood pressure
# Data is available in the text file called 'adult'
weight <- c( 165 ,167 , 180  ,155 ,212  ,175  ,190 , 210  ,200  ,149  ,158  ,169  ,170  , 172 ,159 , 168 ,174  ,183, 215 ,195  , 180 , 143  , 240 , 235  ,192  , 187)
l=length(weight); l
m=mean(weight); s=sd(weight); m; s;
blood.pressure <- c( 130 , 133 , 150 , 128 , 151 , 146 , 150, 140 , 148 , 125 , 133 , 135 , 150 , 153, 128 , 132 , 149 , 158 , 150 , 163 , 156, 124 , 170 , 165 , 160 ,159)

adults <-c(165, 130, 167, 133, 180, 150, 155, 128, 212, 151, 175, 146, 190, 150, 210, 140, 200, 148, 149, 125, 158, 133, 169, 135, 170, 150, 172, 153, 159, 128, 168, 132, 174, 149, 183, 158, 215, 150, 195, 163, 180, 156, 143, 124, 240, 170, 235, 165, 192, 160, 187, 159)

t.test(adults, mu=175, alternative="greater") # data adult has both weight and blood pressure. So not so good.

t.test(weight, mu=175, alternative="greater")
?pnorm
pnorm(175, 182.4231, sqrt(26) )# To find the prob that P(Xbar< 175) under normal assumption with mean 182.42  and n=26

# the p-value very large: do not reject Ho



mean(weight)
var(weight)
sd(weight)
length(weight)
hist(weight)
stem(weight)
boxplot(weight)


t.test(weight, mu=180, alternative="greater")

t.test(weight, mu=175, alternative="greater")

#Instead of "greater", one can use "less" or "two.sided"(default)

#To build confidence interval

t.test(weight)
t.test(weight, conf.level=0.90)


t.test(weight, blood.pressure)

plot(weight, blood.pressure)

plot(weight, blood.pressure)
model<-lm(weight~ blood.pressure)
summary(model)
pred<-predict(result, interval="prediction", level=0.99)
pred

line<-lm(weight~ blood.pressure)
plot(line)
line$coeff
coeff(line)


x=rnorm(1000, mean=0, sd=1)
y=dnorm(x)
plot(x,y)


#One way to use a data set that has been saved in a text file. Change the location.

data1=scan("C:/Documents and Settings/Norou Diawara/Desktop/Stat 437-537/adults.txt")
data1=read.table("C:/Documents and Settings/Norou Diawara/Desktop/Stat 437-537/adults.txt", header=TRUE, sep=" ")

Data<-c(139, 118, 164, 151, 182, 140, 134, 142, 109, 130, 107, 155, 88, 95, 104)




weigths <- c( 165 ,167 , 180  ,155 ,212  ,175  ,190 , 210  ,200  ,149  ,158  ,169  ,170  , 172 ,159 , 168 ,174  ,183, 215 ,195  , 180 , 143  , 240 , 235  ,192  , 187)

blood.pressure <- c( 130 , 133 , 150 , 128 , 151 , 146 , 150, 140 , 148 , 125 , 133 , 135 , 150 , 153, 128 , 132 , 149 , 158 , 150 , 163 , 156, 124 , 170 , 165 , 160 ,159)

adults <-c(165, 130, 167, 133, 180, 150, 155, 128, 212, 151, 175, 146, 190, 150, 210, 140, 200,
148, 149, 125, 158, 133, 169, 135, 170, 150, 172, 153, 159, 128, 168, 132, 174, 149, 183, 158,
215, 150, 195, 163, 180, 156, 143, 124, 240, 170, 235, 165, 192, 160, 187, 159)


t.test(weight, mu=175, alternative="greater")

# the p-value very large: do not reject Ho

t.test(adults, mu=200, alternative="greater")

# the p-value small: reject Ho

mean(weight)
var(weight)
sd(weight)
length(weight)
hist(weight)
stem(weight)
boxplot(weight)
boxplot(weight, blood.pressure)

pie(weight)     # Not interesting at all.
# So let's look at a categorical data the number of cars per houseolds
cars<-c(3, 4, 1, 1, 3, 4 ,3 ,3 ,1, 3, 2, 1, 2, 1 ,2 ,3 ,2 ,3 ,1 ,1 ,1 ,1 ,4 ,3 ,1)
mean(cars)
table=table(cars)
pie(table)

mean(weight) + c(-1, 1) * qnorm(0.975) * sd(weight) / sqrt(length(weight))

#(assuming normal distribution)

t.test(weight)

# Using t-distr, write:

mean(weight) + c(-1, 1) * qnorm(0.975) * sd(weight) / sqrt(length(weight))

mean(weight) + c(-1, 1) * qt(0.025, df=25)* sd(weight) / sqrt(length(weight))

mean(weight) + c(-1, 1) * qt(0.975, df=25)* sd(weight) / sqrt(length(weight))

t.test(weight, mu=180, alternative="greater")

t.test(weight, mu=175, alternative="greater")

# Instead of "greater", one can use "less" or "two.sided"(default)

# To build confidence interval

t.test(weight)
t.test(weight, conf.level=0.90)

t.test(weight, blood.pressure)

plot(weight, blood.pressure)

plot(weight, blood.pressure)
result<-lm(weight~ blood.pressure)
summary(result)
pred<-predict(result, interval="prediction", level=0.99)
pred

cor(blood.pressure, weight)

line<-lm(weight~ blood.pressure)
plot(line)
line$coeff
coeff(line)
line2= glm(weight~ blood.pressure)
plot(weight~ blood.pressure)
abline(line2)

x=rnorm(1000, mean=0, sd=1)
y=dnorm(x)
plot(x,y)


# One way to use a data set that has been saved in a text file. Change the location.

data<-read.table("G:/Stat 437-537/adults.txt", header=TRUE)
attach(data)  this makes the variables easy to use
model1=lm(x~y)
model1
model2=lm(y~x)
model2
summary(model1)

data1=scan("G:/Stat 437-537/adults.txt")
data1=read.table("G:/Stat 437-537/adults.txt", header=TRUE, sep=" ")

data<-read.table("F:/Stat 437-537/boil.txt", header=TRUE)
data<-read.table("F:/Stat 437-537/boiling.txt", header=TRUE)
attach(data)
plot(boil, pres)

options(digits=8)
model1<-lm(boil~pres)
model2<-lm(pres~boil)
model2
summary(model2)
cov(pres, boil = NULL, use = "all.obs", method = "pearson")

cor(pres, boil , use = "all.obs", method = "pearson")

cor(pres, boil , use = "all.obs", method = "kendall")
cor(pres, boil , use = "all.obs", method = "spearman")

summary(model1)

anova.lm(model1)

anova.lm(model1)

anova.lm(model2)

# Confidence interval for slope
coef(model1)[2] + c(-1,1)*qt(.975,29)*(.0393)
# Confidence interval for intercept
coef(model1)[1] + c(-1,1)*qt(.975,29)*(0.77323)
# Confidence and Prediction intervals
clm<-predict(model1, interval="confidence",level=.99 )
plot(data, main=" det vs hgt", xlab="det", ylab="hgt")
abline(coef(model1))

cli<- predict(model1, interval="prediction", level=.99 )
lm.model=lm(hgt~det)
summary(lm.model)
plot(det,hgt)
abline(lm.model)





data<-read.table("G:/Stat 437-537/prob_c.txt", header=TRUE)
attach(data)
model1<-lm(ght~det)

# To get the SSP
hgt
det
data=data.frame(hgt=hgt, group=factor(det))
data
fit=lm(hgt~group, data)
fit
anova(fit)


data<-read.table("G:/Stat 437-537/poly.txt", header=TRUE)
attach(data)
names(data)
model1<-lm(y~x)
model2<-lm(y~x+I(x^2))
model3<-lm(y~x+I(x^2)+I(x^3))
model4<-lm(y~x+I(x^2)+I(x^3)+I(x^4))

#	anova(model1, model2, model3, model4)
# This allows us to compare the models

# Analysis of Variance Table

# Model 1: y ~ x
# Model 2: y ~ x + I(x^2)
# Model 3: y ~ x + I(x^2) + I(x^3)
# Model 4: y ~ x + I(x^2) + I(x^3) + I(x^4)


M3<-lm(y~poly(x,3))  #  does not work.

# You could also set
q=x^2
# and write the model2 as
model2<-lm(y~ x+q)
# generalization with other models can be done also.

# We need to compute the PRESS statistic or more exactly the RMSE_PRESS.

hat.ii <- lm.influence(model1)$hat
y.hat<-fitted(model1)
cv1 <- mean(((y - y.hat) / (1 - hat.ii))^2)
press1 <- sum(((y - y.hat) / (1 - hat.ii))^2)
rmse_press<-sqrt(PRESS/length(y))

# You will find the value 2.719

# Now repeat that for model 2, model 3, and model 4.
# Then compare the estimated RMSE_PRESS

m1.lm<-lm(y~x, data=data)
summary(m1.lm)  gives the coefficients, Rsquare, residuals,
fitted(m1.lm)
resid(m1.lm)
summary.aov(m1.lm)


data<-read.table("G:/Stat 437-537/reading.txt", header=TRUE)
attach(reading)
names(data)
model <- lm(score ~ age + tvhrs
anova(model)
summary(model1)
predict(model1, interval="confidence", level=.99 )
predict(model1, interval="prediction", level=.99 )

plot(score.resid(model1), xlab="age", ylab="Residuals")
plot(score.resid(model1), xlab="reading", ylab="Residuals")
qqnorm(studres(model1)
qqline(studres(model1))

var.test(score, age)


f.test.lm(model1)
f.test.lm(lm(y~x1 + x2), model1)


Model Selection
data<-read.table("G:/Stat 437-537/univ_salary.txt", header=TRUE)
attach(data)
names(data)
model<-lm(salary~diver + yrbr + yrdg+ paper+books)
summary(model)
step(model)
step(model, direction="forward")
step(model, direction="backward")
step(model1, direction="both")


# About Logistic Regression

data<-read.table("F:/Stat 437-537/logit-current.txt", header=TRUE)
attach(data)
y=r/n
model1<-glm(y ~ current, family=binomial(link=logit))

logit.out<-glm(r/n ~ current, family=binomial(link=logit))

summary(logit.out)

# The intercept and slope estimates for beta0 and beta1 are the same as in SAS. However the std errors of the estimates and the AIC are different.


# For Stat 435-535

urban <-c(133 , 205 , 279 , 134 , 205 , 284 , 155 , 206,  284, 170 , 214 , 284 , 175 , 217 , 330 , 179 , 222 ,181, 222 , 184 , 227 , 188 , 227 , 189 , 228 , 190 , 234, 196 , 234 , 197 , 236 , 199 , 239 , 200 , 241,  200, 242 , 201 , 244 , 201 , 249 , 204 , 252 , 205 , 273)



rural <-c(95 , 144 , 189 , 108 , 145 , 192 ,  108 , 145 , 194, 114 , 148 , 197 , 115 , 152 , 204 , 124 , 152 , 220, 129 , 155 , 223, 129 , 157 , 226 , 131 , 158 , 231, 131 ,158, 135 , 162 , 136 , 165 , 136 , 166 , 139, 171 , 140 , 172 , 142 , 173 , 142 , 174 , 143 , 175, 143 , 180 , 144 , 181)


production <- read.table("E:/Stat 437-537/production.txt",header=TRUE)
attach(production)

par(mfrow=c(1,1))
plot(production$RunSize,production$RunTime,xlab="Run Size", ylab="Run Time")


m1 <- lm(RunTime~RunSize)
summary(m1)

plot(production$RunSize,production$RunTime,xlab="Run Size", ylab="Run Time")
abline(lsfit(production$RunSize,production$RunTime))

#t-value on page 23
tval <- qt(1-0.05/2,18)
tval

round(confint(m1,level=0.95),3) # rounding up to 3 decimal places

predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="confidence",level=0.95)
predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="prediction",level=0.95)

anova(m1)
detach(production)




#Data were obtained from 21 days of the operation of a plant for the oxidation of ammonia to nitric acid. 
#There are three independent variables. 
#x1 is air flow. This represents the rate of operation of the plant - the nitric oxides produced are absorbed in a counter current absorption tower. 
#x2 is the cooling water inlet temperature. This is the temperature of the cooling water that circulates through the coils in the absorption tower. 
#x3 is the acid concentration. This is 10 (concentration of the acid circulating -5) 
#y is 10 times the percentage of the ingoing ammonia to the plant that escapes from the absorption column unabsorbed. This is an inverse measure of the overall efficiency of the plant. 
data <- read.table("E:/Stat 437-537/multreg.txt",header=TRUE)

attach(data)
data
exmp.lm<-lm(y~x1 + x2 + x3)
summary(exmp.lm)
predict(exmp.lm,data.frame(x1=65,x2=21,x3=87),se.fit=T)
anova(exmp.lm)
out<-lm(y~x3 + x2 + x1)
anova(out)
par(mfrow=c(1,2))
plot(exmp.lm)


#Data describes values from 24 rabbits.
#Each of 24 rabbits was given an injection of insulin and the percent reduction in blood sugar after a given time period was noted. 
#Two different methods of preparing the insulin and three different dosage levels were used. 
#The three dosage levels were 2.3, 3.6 and 6.5. 
data <- read.table("F:/Stat 437-537/2wayanova.txt",header=TRUE)
attach(data)
# gl is a function in R to generate levels
Preparation<-gl(2,12,24)
Preparation
Dose<-gl(3,4,24)
Dose
Reduction
anova(lm(Reduction~Preparation*Dose))
#next we find the means of Reduction forthe 2x3=6 cells
sapply(split(Reduction,interaction(Preparation,Dose)),mean)
#next,the mean of Reduction for the 2 levels of #Preparation 
sapply(split(Reduction,Preparation),mean)
#next, the mean of Reduction for the 3 levels of Dose
sapply(split(Reduction,Dose),mean)
#next a plot that lets us check graphically for interaction
interaction.plot(Dose,Preparation,Reduction,xlab="Dose",
ylab="Mean of Reduction")
