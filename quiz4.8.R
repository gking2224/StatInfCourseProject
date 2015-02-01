detect <- 0.01
mu0 <- 0
power <- 0.9
s <- 0.04
sem <- s/sqrt(n)
alpha <- 0.05
z <- qnorm(1-alpha)
t <- qt(1-alpha, n-1)
power.t.test(power = power, sig.level = 0.05, delta = detect, sd = s, type = "one.sample", alt="one.sided")$n
