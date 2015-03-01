

# Sample size calculation as in text: Design and Analysis of Experiments
# by Montgomery, 7th edition on page 102, example 3.10

PowerCalcs<-function(means, variance, alpha) {
   overallmean<-sum(means)/length(means)
   taus<-c()
   for (i in 1:length(means)) {
     taus<-cbind(taus,means[i]-overallmean)
   }
   taussqaured = taus^2
   sum_tausquared = sum(taussqaured)

   thetable<-c()

   for (j in 3:20) {
      phisquared = (j*sum_tausquared)/(length(means)*variance)
      phi = sqrt(phisquared)
      a_n = length(means)*(j-1)
      critical=qf(p=1-alpha, df1=length(means)-1, df2=a_n)
      thebeta= pf(q=critical, df1=length(means)-1, df2=a_n, ncp=phisquared*length(means))
      thepower = 1-thebeta
      thetable<-rbind(thetable, c(j,phisquared,phi,a_n,thebeta,thepower))
   }
   return (thetable)
}

PowerCalcs(c(50,60,50,60),25,0.05)

