## @knitr intro
lambda <- 0.2       # rate parameter
numsims <- 1000     # number of simulations
n <- 40             # size of each simulation

# calculate mean, standard deviation and variance of the population using
# the known characteristics of the exponential distribution
mu <- 1 / lambda
sigma <- 1 / lambda
sigma2 <- sigma^2
sem <- sigma / sqrt(n)
