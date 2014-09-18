
> attach(faithful)     # attach the data frame 
> eruption.lm = lm(eruptions ~ waiting)

> newdata = data.frame(waiting=80)

> predict(eruption.lm, newdata, interval="predict") 
fit    lwr    upr 
1 4.1762 3.1961 5.1564 
> detach(faithful)     # clean up

#?predict.lm