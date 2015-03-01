# type I error
alpha <- 0.05

# number of simulations
n.simulations <- 10000

# number of observations in each simulation 
n.obs <- 100

# a vector of test results
type.one.error <- replicate(n.simulations, 
                            shapiro.test(rnorm(n.obs))$p.value)<alpha

# type I error for the whole simulation
mean(type.one.error) #[1] 0.0492

# Store cumulative results in data frame for plotting 
sim <- data.frame(
  n.simulations = 1:n.simulations,
  type.one.error.rate = cumsum(type.one.error) / 
    seq_along(type.one.error))

# plot type I error as function of the number of simulations 
plot(sim, xlab="number of simulations", 
     ylab="cumulative type I error rate")

# a line for the true error rate
abline(h=alpha, col="red")

# alternative plot using ggplot2
require(ggplot2)
ggplot(sim, aes(x=n.simulations, y=type.one.error.rate)) + 
  geom_line() + 
  xlab('number of simulations') +
  ylab('cumulative type I error rate') + 
  ggtitle('Simulation of type I error in Shapiro-Wilk test') +
  geom_abline(intercept = 0.05, slope=0, col='red') +
  theme_bw() 