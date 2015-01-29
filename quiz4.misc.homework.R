round(ppois(10, lambda = 1787/100, lower.tail = TRUE), 2)
pbinom(3-1, 4, prob = 0.5, lower.tail = FALSE)



n <- 9
diff0 <- 1; s0 <- 1.8
diff1 <- -3; s1 <- 1.5
sp <- sqrt( (((n-1) * diff0^2 ) + ((n-1) * diff1^2)) / (n + n - 2 ) )





mu0 <- 10
mua <- 11
s <- 4
n <- 100
diff <- mua - mu0
sem <- s * (1/n + 1/n)^0.5
ts <- (mua-mu0) / sem

pnorm(ts)

pnorm(1/sem, mean = 10, sd = sem, lower.tail = FALSE)