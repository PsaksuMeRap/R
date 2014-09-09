## Stat 437-537   ##
## Norou Diawara  ##
## R file         ##
## Stat 437-537   ##



?paste
paste("Today is", date())
# Let's start with some operators: they are also functions
a<-6  # To assign a value to the letter a. It is an assigment operator.
      # Multiple assignments are also available such as in: 'b<-a<-6'
7-> c # One could do it the reverse way also
b = 8 # another way to assign a value to the letter.
## the letters c and t are used often in built in objects. It is not recommended to use them
a+b
'+' (a,b)
a*b
a-b
a--b # It is a good idea to use space or parenthesis though
a^b
sqrt(a*pi)
log(a,3)  # log( x, base)   computes the log of x with base b

log3p(a)  # the difference in colors shows that there is some problem
log1p(a)  # computes log(1+a)
log2(a)   # computes the log base 2 of a
log(100, base=10)   # one easy way to do it
log10(a)
log5(a)
logb(a,5) # This is the syntax in S-Plus
## Let's open S-Plus commands and compare a little of the headings. There are more options
exp(a)    # computes the exponential of a
log(a,b)
logb(2,4)

help(log) # is another way to get help
help("log")
?log      # is one way to get help
apropos("log")
help("loglin")

date
options('width') # gives the page width that you are working on
help(library='name')
digits

q() # is a quick way to terminate your session


library(foreign)#Allows data to be imported from SPSS
library(car)# Companion to Applied Regression [Fox (2002)] Commands
library(sm)# Bowman and Azzalini (1997) commands
library(xtable)# Generate LaTeX code for R model objects
library(splines) # load library that will generate polynomial splines


library(MASS)# Load Venables and Ripley MASS library
chem  # This is a data set that is included in the MASS library
mean(chem) # finds the mean of the data set
## It would be nice to assign a name to that mean.
d<-mean(chem)
e=mean(chem)
e; d
var(chem)
std(chem)
std.dev(chem)
?std
help("standard deviation")
library(help=MASS)  # gives description of library and the data sets



x<-1:10  # creates regular sequence between 1 and 10
seq(1,10, by=1) # another method to do it
seq(0,2, length=10)  # creates a sequence of 10 numbers from 0 to 2
10:1 # is another way
-10:1
x<-c(x,11) # takes x, adds 11 to it, and replaces x by it
x<-c(x, c(12,13)) # takes x and adds another vector c(12, 13)
x[3]  # to obtain the 3rd element of x
x[-3]  # to delete the 3rd element of x
x[3:7] # to obtain the those elements with position from 3 to 7
x[-c(3:7)]  # delete all those positions, and give the others

# Some useful functions associated with any vector
x  # let's thing of this as x(1), x(2), ..., x(15)
x/2
x+y # is possible if they have the same length
c(1,2,3,4)+c(5,6)
c(1,2,3,4)+c(5,6,7,8)
length(x)   # it will be 13
x[-1] # we know this one
x[-1]-x[-length(x)] #  delete x[-1] as a vector with x[-length(x)]
x[-length(x)]
a=6
b
a==b  # Do we have a equals b?
a<b
a>b
a<=b
rm(a)

# Here is a tricky one
(1/49)*49
(1/49)*49==1  # what is the problem
(1/49)*49-1  # rounding errors
eps<- 1e-12# to set the maximum difference allowed
abs((1/49)*49==1)<=eps


sin(x)   # finds the sine of each element of x
sin(x)^2
sin(x)^2+cos(x)^2

sum(x)
mean(x)  # it is better to assign a value mn<-mean(x)
var(x)  # these are examples of built in functions: hist, quantile, prod are some others.
# there are other ones, for glm, ANOVA, 
help.search("ANOVA")

sd<-sqrt(var(x)) # this makes sd a function, so that you could assign values to it.
# Sometimes, one needs to write "sd<-function(x) {sqrt(var(x))}"
# Although there are lots of built in functions in R, sometimes, you might need to create your own
power<-function(x)
{
power2<-x^2
power3<-x^3
return(list("sqr"=power2,"cub"= power3))
}
power(4)

power(4)$ sqr
# function are usually called FUN in the codes.
f=function(x,c){sqrt(1+c*x^2)}
f(12,3)
f=function(x){abs(x-2)/(6*x+1)}
plot(f, 0,2)
?integrate
integrate(function(x) x**2, 0, 5)


sd(x)
sd(y)
rm(mn) # allows to remore the name mn as a name, and maybe use "mn" for something else
sqrt(-3)   # NaN is shown when value undefined or missing


# On inequalities 

sign<-(x<0) # returns a logical the values of x that are negative
sign

x=c(-1:5, 1)
sign<-(x<0)
sign


y<-seq(1,3,0.1) # creates a sequence from 1 to 3 with a 0.1 increment

chardata=c("w", "a", "b", "c")
chardata
is.numeric(chardata)  # way to check if data is numeric or not
chardata>"a"     # alphabetic inequality
chardata[chardata>"a"]
length(chardata[chardata>"a"])
mode(chardata)

x<-1:10 # creates a set of numbers, not a vector yet 
dim(x)  # dimenson does not exist yet
dim(x)<-c(2,5)  # creates a vector or rather a 2 by 5 matrix
x # is a matrix filled by row, not by column
#  To do that, let's change x back to a sequence and use the matrix function
x<-1:10
x<-matrix(x,2,5)
x
z<-as.vector(x)
z
w<-matrix(x,2,5, byrow=T)
w
dim(w)<-c(3,5) # error message
x
x[2,3]
x[,3] # to access the 3rd column
x[,-3] # everything except the 3rd column
x[, c(2,5)]    # gives the 2nd and 5th column
x[2,c(2,5)]      # gives the intersection of 2nd row, and the 2nd and 5th column
x<-cbind(x,c(11:12))  # add another column, adjoin column
x
x<-rbind(x,13:18) # allows to adjoin row
x
t(x)  # to get the transpose of x
apply(x,1,mean) # gives mean of each row
apply(x,2,mean)  # gives the mean of each column


jsum<-function(x)
{
jsum<-0
for (i in 1:length(x)) {jsum<-jsum+x[i]
}
return(jsum)
} 
jsum(x)

# Let's look at more about matrices
a<-matrix(c(1,2,3,4),2,2)
b<-matrix(c(5,6,7,8),2,2)
det(a)
t(a) # gives the transpose of a
a%*%b # is the product
eigen(a)
?solve
solve(a) # gives the inverse of the matrix
# Choleski decomposition is obtained by taking 'chol(a)'
# The QR decomposition is obtained from 'qr(a)'
solve(a,b) # find a matrix c such that a*c=b
crossprod(a)
crossprod(a,b)
dim(a)
apply(a,1,sum)
apply(a,2,sum)
apply(a,c(1,2),sum)

# Let's look a simple data example
# Group of 26 adults surveyed for their weight and blood pressure
adults <-c(165, 130, 167, 133, 180, 150, 155, 128, 212, 151, 175, 146, 190, 150, 210, 140, 200, 148, 149, 125, 158, 133, 169, 135, 170, 150, 172, 153, 159, 128, 168, 132, 174, 149, 183, 158, 215, 150, 195, 163, 180, 156, 143, 124, 240, 170, 235, 165, 192, 160, 187, 159)


t.test(weights, mu=175, alternative="greater")

# the p-value very large: do not reject Ho

t.test(adults, mu=200, alternative="greater")

# the p-value small: reject Ho


weights <- c( 165 ,167 , 180  ,155 ,212  ,175  ,190 , 210  ,200  ,149  ,158  ,169  ,170  , 172 ,159 , 168 ,174  ,183, 215 ,195  , 180 , 143  , 240 , 235  ,192  , 187)

blood.pressure <- c( 130 , 133 , 150 , 128 , 151 , 146 , 150, 140 , 148 , 125 , 133 , 135 , 150 , 153, 128 , 132 , 149 , 158 , 150 , 163 , 156, 124 , 170 , 165 , 160 ,159)

mean(weights)
var(weights)
sd(weights)
length(weights)
hist(weights)
stem(weights)
boxplot(weights)
boxplot(weights, blood.pressure)
pie(weights)
summary(weights)


t.test(weights, mu=180, alternative="greater")

t.test(weights, mu=175, alternative="greater")

# Instead of "greater", one can use "less" or "two.sided"(default)

# To build confidence interval

t.test(weights)
t.test(weights, conf.level=0.90)


t.test(weights, blood.pressure)

plot(weights, blood.pressure)

plot(weights, blood.pressure)
result<-lm(weights~ blood.pressure)
summary(result)
pred<-predict(result, interval="prediction", level=0.99)
pred

line<-lm(weights~ blood.pressure)
plot(line)
line$coeff
coeff(line)


x=rnorm(1000, mean=0, sd=1)
qnorm(0.26) # qnorm(p,mu=0,sd=1)
pnorm(3)    #pnorm(q, mu=0, sd=1) gives values such that P(X<q)=p
dnorm(2) # dnorm(x, mu=0, sd=1) gives the pdf of the std normal
y=dnorm(x)
plot(x,y)

a<-runif(3)
sort(a)
sort(rnorm(100))[9500]

# One way to use a data set that has been saved in a text file. Change the location.


data1=read.table("E:\Stat 405-505\adults.txt", header=TRUE) # This is not working.
?read.table
# Also from http://cran.r-project.org/doc/manuals/R-data.pdf, om page 5, there
# are explanations as to how read.table and scan work.
# Change the backward slash,  and add the header since there are row labels.


data1=read.table("E:/Stat 405-505/adults.txt", header=TRUE)
data1
attach(data1)  # This is useful, as it allows you to manipulate the data as you wish.
x ; mean(x)

# scan is another way to read the data.
# in the txt file, please delete the first line that has the names x and y.
# Those names will be called in the scan option, under list(x=0, y=0)
data1=scan("E:/Stat 405-505/adults.txt", what=list(x=0, y=0))
data1
# If x was a character variable, use what=list(x="0", y=0)


Data<-c(139, 118, 164, 151, 182, 140, 134, 142, 109, 130, 107, 155, 88, 95, 104)

# A little data simulation example
?paste
paste("Today is", date())
paste("A little simulation with std normal" ,"for today", sep=",") # This allows to paste 2 characters
paste("Today is", "August 27", collapse="+") # after collapse, you can replace '+' with '&'

# There are other things to try: substring, substr, arg(substr)

x1<-rnorm(300, 0.5) # creates data from normal mean 0.5, and size 300
args(rnorm)
x2<-rnorm(700, mean=0, sd=1)
x<-c(x1,x2) # mixture of 2 normals 0.3 X1+ 0.7 X2

nsample<-rnorm(500)
hist(nsample)
xbar<-mean(nsample)
xbar
sd<-sqrt(var(nsample))
sd
# let's verify the empirical rule
length(sqrt(length(nsample))[abs(nsample-xbar)< sd])/500 # the idea is to count how many observations fall withing 1 sd from the mean

x<-rnorm(10)
y<-rnorm(10)

x>y
x
x<-seq(0,pi, 0.01)
y<-sin(x)
xy<-plot(y~x)

# r: random number generator
# p: probability function  F(x)=P(X=<x)
# d: density function f(x)
# q: quantile function F^{-1}(x)

# beta=Beta distr, binom=Binomial distr, chisq=ChiSquare distr
# gamma=Gamma distr, lnorm=lognormal distr, pois=Poisson distr
# t=T-distr, unif=Uniform distr...

# Simple graphs in R using the function command.
f<-function(x){
y<-abs(4-3*x)/(6*x+1)
return(y)
}
f(3)
plot(f, -10,20)

# Looking at the graph in TI 86, we might think that the limits at infinity are 0, when in fact they are -0.5 and 0.5
# Also, one could think that the asymptote is at x=0 looking at TI 86, when in fact it is -1/6.
# So this R free software is quite good.


# Building a Simple Linear Regression
# The height's example by Weisberg is a nice one.
# The question asked is to find out what is the relation between heights of mothers and the heights of their daugters
# with up tu 2 daughters per mother. (data collection due to K. Pearson) with all daughters 18 years old or more
# and all mothers less than 65 years of age.

# Go to R windows: "packages" menu: load: "install packages" and choose "arl3"
# alr3 is the 3rd edition of "Applied Linear Regression" of Weisberg, Sanford, 2005
library(alr3)
data(heights)
attach(heights) # allows to see the variables in the data set called heights
                # and make the columns visible
heights
# data is in inches unit
# A scatter plot is a good start for the data
plot(Mheight,Dheight,xlim=c(55,75),ylim=c(55,75))

# Here the plot is for some the values of x and y between 55 and 75 inches.
# The option "pch" allows for the ploting character
?pch
plot(Mheight,Dheight,xlim=c(55,75),ylim=c(55,75),pch=20)

# "cex" is another option that controls the size of the plotting symbol: here 0.3 times the size.

plot(Mheight,Dheight,xlim=c(55,75),ylim=c(55,75),pch=20,cex=.3)

# This next function allows to select the points to appear
# the "|" is for "or"
sel <- (57.5 < Mheight) & (Mheight <= 58.5) |
       (62.5 < Mheight) & (Mheight <= 63.5) |
       (67.5 < Mheight) & (Mheight <= 68.5)

plot(Mheight[sel],Dheight[sel],xlim=c(55,75),ylim=c(55,75),pch=20,cex=.3,
        xlab="Mheight",ylab="Dheight")

par(mfrow=c(1,2)) # to get several graphs in one window with 1 row and 2 columns
# par(mfrow=c(2,2)) puts 4 graphs in a picture (or matrix form)
plot(Mheight,Dheight,bty="l",cex=.3,pch=20)

# This next function adds one or several lines to the plot.
?abline
#abline(intercept, slope, horizontal line, vertical line,)
abline(0,1,lty=2) # lty gives the line dashed when =2 and solid when =1 

fit=lm(Dheight~Mheight)
names(fit)
fit
fit$coefficients
fit$coef
fit$resid
fit$effects
abline(lm(Dheight~Mheight),lty=1) # One can add 'col="red"  '

# Here is another data that is very famous
# 17 observed data of boiling point and barometer pressure
# Temp is the boiling temp of water in degree F, 
# Pressure is the atmospheric pressure in mercury
data(forbes)
attach(forbes)
forbes  # We are not interested in variable "Pressure", but rather in variable "Lpres"
# Forbes suggested that the log of the pressure is the one related to the temp.
par(mfrow=c(1,2))
plot(Temp,Pressure,xlab="Temperature",ylab="Pressure",bty="l")
plot(Temp,Lpres,xlab="Temperature",ylab="Lpress",bty="l")
# The difference since the data is in fact 100*log(Pressure), not just log(Pressure)
m0 <- lm(Pressure~Temp);
abline(m0)
m1<-lm(Lpres~Temp) # m1 is a list containing the coeff, the residuals, fitted values,..
help("lm")
help("glm") ; help("aov"); help("survival")
abline(m1)

# The next 4 operators allow to see information about the model. The last 2 are the most useful.
print(m1)
summary(m1)
anova(m1)
summary.lm(m1)

m3<-lm(Lpres~1, data=forbes) # model with intercept only
summary(m3)

m4<-lm(Lpres ~ 1 + Temp) # This is another way to add the intercept and the slope
# m4 is same as m0

m5<-lm(Lpres~Temp-1) # fits the regression model without the intercept
m5
forbes1 <- forbes[,c(1,3)] # Pick the data of interest
fmeans<- mean(forbes1)
fmeans



m1 <- lm(logb(Pressure)~Temp)
m2 <- lm(logb(Pressure,10)~Temp)
anova(m1)

apply(forbes,2, mean) # This is a useful function. It applies the function of your choice to the data column
apply(forbes,1, mean)  # or to the row

var(forbes)   # gives the variance-covariance of the data. You can also use 'cov(forbes)'
cor(forbes)    # gives the correlation structure
# from here, one can get R^2 between Temp and Lpres which is 0.99747^2=0.99
# or also from the model statement.


m1<-lm(Lpres~Temp)
myreg<-lm(Lpres~Temp)
betahat<-coef(m1)  # Gives the coeff of the model
betahat
vcov(m1) # Gives the var cov of the coeff.
predict(m1)
predict(m1, newdata=data.frame(Temp=c(227, 228)))
predict(m1, newdata=data.frame(Temp=c(227, 228)), interval="prediction", level=0.95)
predict(m1, newdata=data.frame(Temp=c(227, 228)), interval="confidence", level=0.95)
predict(m1, newdata=data.frame(Temp=c(227)), se.fit=T, ci.fit=T, pi.fit=T, level=0.95)  # to get predicted, confidence and prediction intervals

# Sources
   
# Read Fox (2002) Chapter 4
# Venables and Ripley (1999)  Chapter 6