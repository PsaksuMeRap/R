setwd("/Volumes/LaCie/GitHub/R/data")
getwd()




d1<- read.table("treesamara.txt", header=TRUE)
d1

str(d1)

attach(d1)

trees<- as.factor(d1$tree);
class<- as.factor(d1$class);
velocity<- as.numeric(d1$velocity)


df<- data.frame(velocity, trees, class)

df

fitDf<- lm(velocity~ trees, data= df);
summary(fitDf)


factorMean<- tapply(velocity, trees, mean);
summary(factorMean)


anovaFitDf<- aov(fitDf)

summary(anovaFitDf)



library(lme4)

sapply(df,data.class)

ears.lmer2 = lmer(ears~1+(1|site)+(1|parcel));


randomFactorFit= lmer(velocity~1+(1|trees));

summary(randomFactorFit)

anova(randomFactorFit)

summary(anovaRandEffectModel)


means=sapply(split(velocity, trees), mean)

randomFit=sapply(split(fitted(randomFactorFit), trees), mean); 
randomFit

data.frame(mean=means, fitted=randomFit)


mixedFit<- lmer(velocity~ trees + (1|class) + (1|trees:class))

summary(mixedFit)

summary(anova(mixedFit))