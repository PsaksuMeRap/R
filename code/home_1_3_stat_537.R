## R file         ##
## Stat 437-537   ##

#setwd(../)
#geted()


# to find the value of of the inverse CDF always use "q"

# Homework 1
#Problem 3 Chapter 3
#P(Z >=-1)=?
pnorm(0.00, -1)
#[1] 0.8413447
#P(Z<= X)=.20
qnorm(.20)
#[1] -0.8416212

# Problem 4 Chapter 3
#P(chisq(7)>= X)=.01
qchisq(0.99, 7)
#[1] 18.47531
#P(chisq(12)<= 14)=?
pchisq(14,12)
#[1] 0.6992917

#Problem 5
#P(T(13)<X)=.10
qt(.10, 13)
#[1] -1.350171
#P(|T(28)|>= 2.05)
1-pt(2.05, 28)
#[1] 0.02491697

# Problem 6 Chapter 3
#For the F distro, remember that the first numnber is the df of the numerator, and the 2nd is the df of the denominator
#P(F(6,24)>= X)=.05
qf(0.95, 6, 24)
#[1] 0.02491697
#P(F(5,40) >= 2.9)=?
pf(2.9, 5, 40)
#[1] 0.9748583
1-pf(2.9, 5, 40)
#[1] 0.02514165 --is the answer to the question, and is the chart used to find the answer in the back of the book-- 


# Problem 12 Chapter 3
qt(.975, 27)
qt(.025, 27) # and change the sign associated
# Assuming normal distribution, take
qnorm(0.5) ; qnorm(.975)
pnorm(1.28); pnorm(2.5)

# Problem 16 Chapter 3
d1<-c(132, 145, 124, 122, 165, 144, 151)
d2<-c(141,139, 172, 131, 150, 125)
n1<-length(d1); n2<-length(d2)
xbar1<-mean(d1); xbar2<-mean(d2);
xbar1; xbar2

s1<-sd(d1); s2<-sd(d2); s1; s2
xbar1+qnorm(0.975)*s1/sqrt(n1)
xbar1-qnorm(0.975)*s1/sqrt(n1)
# “d” returns the height of the probability density function
# “p”	returns the cumulative density function
# “q”	returns the inverse cumulative density function (quantiles)
# “r”	returns randomly generated numbers

sp<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
sp^2; sp
t=(xbar1-xbar2)/(sp * sqrt(n1^(-1)+n2^(-1) )); t

# t.test performs a one or two sample t-test
t.test(d1, d2)
t.test(d1, d2, var.equal=TRUE)
pt(3.29, df=11)
pt(0.29, df=11)
pt(0.52, df=11)
2*(1-pt(0.52, df=11))

var.test(d1, d2) # Test for equality of variances.
# Equality of variances means that the ratio of the two variances is 1.
# So the null hypothesis says that: Ho: the ratio of variances is 1
# The alternative says that: Ha: the ratio variance is not 1.
# The test has a p-value of 0.85. So Do not reject the Ho.
# and proceed with a test with equality of variance assumed.
# The sample estimate of the ratio is: 0.8646


# data on systolic blood pressure (sbp) and age of 30 individuals available in textbook page 51

sby<-c(144, 220, 138, 145, 162, 142, 170, 124, 158, 154, 162, 150, 140, 110, 128,
130, 135, 114, 116, 124, 136, 142, 120, 120, 160, 158, 144, 130, 125, 175)
age<-c(39, 47, 45, 47, 65, 46, 67, 42, 67, 56, 64, 56, 59, 34, 42, 48, 45, 17,
20, 19, 36, 50, 39, 21, 44, 53, 63, 29, 25, 69)

plot(age, sby)
plot(age, sby, xlab="age of the individuals", ylab="systolic blood pressure", title("Scatter plot d"))

# Here is another example of d: salt concentration in surface streams (Y variable)
# and percentage of watershed area consisting of paved roads(X variable)
salt<-c(3.8, 5.9, 14.1, 10.4, 14.6, 14.5, 15.1, 11.9, 15.5, 9.3,
15.6, 20.8, 14.6, 16.6, 25.6, 20.9, 29.9, 19.6, 31.3, 32.7)
road<-c(0.19, 0.15, 0.57, 0.40, 0.70, 0.67, 0.63, 0.47, 0.75, 0.60,
0.78, 0.81, 0.78, 0.69, 1.30, 1.05, 1.52, 1.06, 1.74, 1.62)

plot(road, salt)  # The scatter plot suggests a linear model to understand the mechanism betwen x and y

# Group of 26 adults surveyed for their weight and blood pressure
# d is available in the text file called 'adult'
weight <- c( 165 ,167 , 180  ,155 ,212  ,175  ,190 , 210  ,200  ,149  ,158  ,169  ,170  , 172 ,159 , 168 ,174  ,183, 215 ,195  , 180 , 143  , 240 , 235  ,192  , 187)
l=length(weight); l
m=mean(weight); s=sd(weight); m; s;
blood.pressure <- c( 130 , 133 , 150 , 128 , 151 , 146 , 150, 140 , 148 , 125 , 133 , 135 , 150 , 153, 128 , 132 , 149 , 158 , 150 , 163 , 156, 124 , 170 , 165 , 160 ,159)

adults <-c(165, 130, 167, 133, 180, 150, 155, 128, 212, 151, 175, 146, 190, 150, 210, 140, 200, 148, 149, 125, 158, 133, 169, 135, 170, 150, 172, 153, 159, 128, 168, 132, 174, 149, 183, 158, 215, 150, 195, 163, 180, 156, 143, 124, 240, 170, 235, 165, 192, 160, 187, 159)

t.test(adults, mu=175, alternative="greater") # d adult has both weight and blood pressure. So not so good.

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


#One way to use a d set that has been saved in a text file. Change the location.

d1=scan("G:/Stat 437-537/adults.txt")
d1=read.table("G:/Stat 437-537/adults.txt", header=TRUE, sep=" ")

d<-c(139, 118, 164, 151, 182, 140, 134, 142, 109, 130, 107, 155, 88, 95, 104)




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
# So let's look at a categorical d the number of cars per houseolds
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


# One way to use a d set that has been saved in a text file. Change the location.

d<-read.table("G:/Stat 437-537/adults.txt", header=TRUE)
attach(d)  this makes the variables easy to use
model1=lm(x~y)
model1
model2=lm(y~x)
model2
summary(model1)

d1=scan("G:/Stat 437-537/adults.txt")
d1=read.table("G:/Stat 437-537/adults.txt", header=TRUE, sep=" ")

d<-read.table("G:/Stat 437-537/boil.txt", header=TRUE)
attach(d)
plot(boil, pres)
model1<-lm(boil~pres)
model2<-lm(pres~boil)
cov(pres, boil = NULL, use = "all.obs", method = "pearson")

cor(pres, boil , use = "all.obs", method = "pearson")

cor(pres, boil , use = "all.obs", method = "kendall")
cor(pres, boil , use = "all.obs", method = "spearman")

summary(model1)

anova.lm(model1)

anova.lm(model1)

anova.lm(model2)

Confidence interval for slope

coef(model1)[2] + c(-1,1)*qt(.975,29)*(.0393)

Confidence interval for intercept

coef(model1)[1] + c(-1,1)*qt(.975,29)*(0.77323)

Confidence and Prediction intervals

clm<-predict(model1, interval="confidence",level=.99 )
plot(d, main=" det vs hgt", xlab="det", ylab="hgt")
abline(coef(model1))

cli<- predict(model1, interval="prediction", level=.99 )
lm.model=lm(hgt~det)
summary(lm.model)
plot(det,hgt)
abline(lm.model)





d<-read.table("G:/Stat 437-537/prob_c.txt", header=TRUE)
attach(d)
model1<-lm(ght~det)

# To get the SSP
hgt
det
d=d.frame(hgt=hgt, group=factor(det))
d
fit=lm(hgt~group, d)
fit
anova(fit)


d<-read.table("G:/Stat 437-537/poly.txt", header=TRUE)
attach(d)
names(d)
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

m1.lm<-lm(y~x, d=d)
summary(m1.lm)  gives the coefficients, Rsquare, residuals,
fitted(m1.lm)
resid(m1.lm)
summary.aov(m1.lm)


d<-read.table("G:/Stat 437-537/reading.txt", header=TRUE)
attach(reading)
names(d)
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
