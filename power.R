mu <- 35
n <- 10
x <- rnorm(n, mean = mu, sd = 3)
xbar <- mean(x)
s <- sd(x)
sem <- s / sqrt(n)
t <- qt(0.975, df = n-1)

pred <- 30
power <- (xbar - pred) / sem

print(power)
