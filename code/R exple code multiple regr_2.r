#Data were obtained from 21 days of the operation of a plant for the oxidation of ammonia to nitric acid.
#There are three independent variables.
#x1 is air flow. This represents the rate of operation of the plant - the nitric oxides produced are absorbed in a counter current absorption tower.
#x2 is the cooling water inlet temperature. This is the temperature of the cooling water that circulates through the coils in the absorption tower.
#x3 is the acid concentration. This is 10 (concentration of the acid circulating -5)
#y is 10 times the percentage of the ingoing ammonia to the plant that escapes from the absorption column unabsorbed. This is an inverse measure of the overall efficiency of the plant.
data <- read.table("F:/Stat 437-537/multreg.txt",header=TRUE)

attach(data)
data
plot(data)
exmp.lm<-lm(y~x1 + x2 + x3)
# or 
model1= lm(y~x1 + x2 + x3, data=data)
summary(exmp.lm)
predict(exmp.lm,data.frame(x1=65,x2=21,x3=87),se.fit=T)
anova(exmp.lm)
out<-lm(y~x3 + x2 + x1)
anova(out)
par(mfrow=c(1,2))
plot(exmp.lm)

model2= lm(y~x1 + x2 + x3, data=data)
model3= lm(log(y)~x1 + x2 + x3, data=data)

# Partial F test
full=lm(y~x1 + x2 + x3, data=data)
reduced=lm(y~x1 + x2, data=data)
anova(reduced, full)   # Compare the models

model=lm(y~x1 + x2 + x3, data=data)

predict(model,data.frame(x1=value, x2=value, x3=value),interval="confidence")

predict(model,data.frame(x1=value, x2=value, x3=value),interval="prediction")
