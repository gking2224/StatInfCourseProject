lambda <- 0.2
mu <- 1 / lambda
sigma <- 1 / lambda
sigma2 <- sigma^2

n <- 40             # size of each simulation
numsims <- 1000     # number of simulations
sem <- sigma / sqrt(n)

print(data.frame(mean = mu, sd = sigma, variance = sigma2, sem = sem))
