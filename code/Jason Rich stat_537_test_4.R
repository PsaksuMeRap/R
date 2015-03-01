
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
pkgs.need<- c("car","MASS", "caret", "ggplot2", "leaps", "segmented", "stats")
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
library(stats)

#####################################################################


####################################################
#          Questions 1
####################################################               
#a.) The correlation matrix
?cor
cor(d.1, method = 'pearson')



#b.) PRESS function to calculate PRESS residual, PRESS Statistic, and RMS_PRESS
#pr <- resid(fit.1)/(1 - lm.influence(fit.1)$hat) --PRESS residuals (Montgomery 2013, p.471)

fit.1 <- lm(price ~  taxes + bath + lotsiz + livspc + grg + rm + bdrm + age + frpl, data = d.1)
summary(fit.1)
plot(fit.1)
r <- resid(fit.1)
r
pr <- r/(1 - lm.influence(fit.1)$hat)
pr
r.2 <- sum(r^2) 
pr.2 <- sum(pr^2) 
# RMS_PRESS = sqrt(PRESS/n)
rms.p.2<- sqrt(pr.2/n) 
rms.p.2 


#c.) The full Model
#?lm

fit.1 <- lm(price ~  taxes + bath + lotsiz + livspc + grg + rm + bdrm + age + frpl, data = d.1)
summary(fit.1)
plot(fit.1)

####### FWD BKWD and STEP-WISE selections methods are below

# 1.) -- C(p) criterion model selection. Using C(p) selection criteria, choose the model where the C(p) ≈ p; where p = parameters in the model
#   1.a) -- C(p) = SSE(model with p-parameters)/MSE(full model) - (n-2p)
# 2.) -- another mehtod: AIC (Alaike Information Criteria) = nlog(SSE/n) + 2p: (want model with a small AIC)
# 3.) -- another method: BIC (Bayes Information Criteria) aka SBC (Schwarz's Bayesian criterion)  --> nlog(SSE/n) + p*log(n): (want model with a small AIC)


#?leaps can be used to find "method=c("Cp", "adjr2", "r2")
# ?AIC --> for AIC() and BIC() 
# SSE you can use the following code if you just want a quick 
# SSE <- sum(resid(model))^2)
View(d.1)
sel.1 <- leaps(x=d.1[,2:10], y=d.1[,1], names=names(d.1)[2:10], method="Cp")
sel.1


w.1<- 1/std.1^2  # weight
# lm.2<- lm(formula=y~x, weights = w.1)
lm.2

sel.1 <- leaps(x=d.1[,2:10], y=d.1[,1], names=names(d.1)[2:10], method=c("adjr2", "r2")
AIC(lm.2)
BIC(lm.2)
summary(lm.2)

#residual plots
plot(fitted(fit.1), residuals(fit.1))
#?abline
abline(h=0, lty=2)
keep <- fitted(fit.1) < 70
keep
lines(lowess(fitted(fit.1), residuals(fit.1)), lty = 3)
lines(lowess(fitted(fit.1)[keep], residuals(fit.1)[keep]), lty = 2)

#other options to consider when analyzing outlyers in the data
# ASSESSING OUTLIERS
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 

# ?leveragePlots
leveragePlots(fit) # leverage plots (METHOD 1)

#PLOTTING LEVEAGE POINT (METHOD 2)
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

?model.matrix # creates a design (or model) matrix.
# plot(hat(model.matrix(fit.1)), type = "h")

# ASSESSING OUTLIERS
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fit) # leverage plots

######## INFLUENTIAL OBS
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit,  id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# ?cooks.distance
# plot(cooks.distance(fit.1)[keep], type = "h")

########## NORMALITY OF RESIDUALS
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
# require(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

plot(rstudent(fit.1)[keep], type = "h") #ANOTHER METHOD FOR PLOTTING STUDENT RESID

########## COLLINEARITY
# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

############HOMOSKEDASTICITY
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

###################################################
#                Questions 2
###################################################
#set-up, read-in the data, plots, and lines

n.1 <- 23
c.v <- qt(.975, df = n.1-2)
# c.v = 2.079614

# pd<-read.table("E:/Stat 437-537/data patient satisf.txt")
# names(pd) <- c("y","x1","x2","x3")

pd<- read.table("patient.txt", sep=" ", col.names=c("y", "x1", "x2", "x3"), fill = FALSE, strip.white=TRUE)
#another method --read.table("~/documents/GitHub/R/data/patient.txt" header = T)
#view(pd)
#str(pd)
dim(pd)
is.data.frame(pd)
summary(pd)
#using the same pacakage and libraries as problem one
attach(pd)
plot(pd)

fit.2 <-lm(y~ x1 + x2 + x3, data=pd)
summary(fit.2)

anova(fit.2)
aov(fit.2)
#checking for outliers in the data
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
# the order the x2 was put into the model is different than fit.2 
fit.3 <-lm(y~ x1 + x3, data=pd)
summary(fit.3)

anova(fit.3)
aov(fit.3)

##### PARTIAL F TEST ####
# The test carried out by anova here is called a partial F-test.
fit.4<-lm(y~x1+x2+ x3+ x4+ x5, data=data5)
fit.5<-update(a.lm ~. -x4-x5, data=data5) # this update deletes the terms that are not needed in the model.
anova(fit.4, fit.5, test="F")

# F crital value
qf(.95, 3, 19)

# reject the H0 if F-stat < F-distro(table) 
#################################################################
#c.) 
#ANOVA


#set the  contrast for type II and type III testing
options(contrasts=c(unordered="contr.sum", ordered="contr.poly"))

#for type III SSE use library (cars) and fun = ANOVA type = "III" 

fit.3 <- glm(formula = y ~ x1 + x2 + x3 , data = pd)
fit.3
aov(fit.3)

#type I
anova(lm(y~ x1 + x2 + x3))

# type II
Anova(lm(y~ x1 + x2 + x3), type = "II")

#type 3
Anova(lm(y~ x1 + x2 + x3), type = "III")

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

#to verify the selection make above, I will use fwd, bckwd and step-wise selection, and check the results against those in the "dropterm" command.

attach(pd)
?step

#backward
step.sel.0 <- step(glm(formula = y ~ x1 + x2 + x3, data = pd) 
                    ,direction = "backward", trace= 1, keep = NULL
                      ,steps = 1000, k = 2)


#forward
step.sel.1 <- step(glm( y ~ 1, data = pd), direction = "forward"
                    ,scale = 0, scope =~ x1 + x2 + x3, trace= 1 
                      ,keep = NULL, steps = 1000, k = 2)


#step-wise
#note, for stepwise, you can start with either the null model or the full model
step.sel.1 <- step(glm( y ~ x1 + x2 + x3, data = pd), direction = "both"
                    ,scale = 0, trace= 1, keep = NULL, steps = 1000
                      ,k = 2)


#################################################################
#e.) drop x2, x3|x1
#starting our full model
summary(glm.0)

glm.2 <- update(glm.0, . ~ . -x2 - x3)
summary(glm.2)
dropterm(glm.2, test="F")

anova(glm.0, glm.2, test = "F")

#1-a F-distro
qf(.95, df1 = 2, df2 = 20)

#clean-up
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
#correlation y~x2
cor.test(x2, y, data = prob.3, method = "pearson")

#b.)
fit.4 <- lm(formula = y ~ x1 + x2, data = prob.3 )
summary(fit.4)

#hyp testing
#full model anova
m.1 <- aov(glm(formula = y ~ x1 + x2, data = prob.3))
summary(m.1)

# null model anova 
m.2 <- aov(glm(formula= y~1, data = prob.3))
summary(m.2)

#ssm calculation
ssr.p1 <- ((0.00513 + 0.16941) - (0.175))/2
ssr.p1/0.00045 
qf(.975, df1 = 2,df2 = 4)


######### LOGIT example ##########

# Since the response is a binomial variable, a logistic regression can be done as follows... 
glm.out = glm(seen ~ W * C * CW, family=binomial(logit), data=gorilla)
summary(glm.out)

## type added-in first anova using chi^2, can change test = "F"
anova(glm.out, test="Chisq")

# Analysis of Deviance Table
# Model: binomial, link: logit
# Response: seen
# Terms added sequentially (first to last)
