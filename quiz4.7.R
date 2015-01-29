n <- 100

detect <- 0.01
mu0 <- 0

s <- 0.04
sem <- s/sqrt(n)
alpha <- 0.05
z <- qnorm(1-alpha)

pnorm(mu0 + z * sem, mean = detect, sd = sem, lower.tail = FALSE)

power.t.test(n = 100, delta = 0.01, sd = 0.04, type = "one.sample", alt="one.sided")$power
