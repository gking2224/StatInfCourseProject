n <- 100

delta <- 0.01
mu0 <- 0

s <- 0.04
sem <- s/sqrt(n)
alpha <- 0.05
z <- qnorm(1-alpha)
t <- qt(1-alpha, n-1)
pnorm(mu0 + t * sem, mean = delta, sd = sem, lower.tail = FALSE)

power.t.test(n = 100, delta = delta, sd = s, sig.level = 0.05, type = "one.sample", alt="one.sided")$power
