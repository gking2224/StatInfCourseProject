#q1
n <- 9
sd <- 30
xbar <- 1100
sem <- sd / n^0.5
t <- qt(0.975, df = n - 1)
round(xbar + c(-1,1) * t * sem, 0)
rm(n, sd, t, sem, xbar)

#q2
n <- 9
diff <- -2
t <- qt(0.975, df = n - 1)
s <- (-diff / t) * sqrt(n)
round(s,2)
rm(n, diff, t, s)

#q4
m1 <- 5
s1 <- 0.68^0.5
m2 <- 3
s2 <- 0.6^0.5
diff <- m2 - m1
n1 <- 10
n2 <- 10
sp <- sqrt( ((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2) )
t <- qt(0.975, df = (n1 + n2 - 2))
semd <- sp * (1/n1 + 1/n2)^0.5
ci <- diff + c(-1,1)* t * semd
round(ci, 2)
rm(m1, m2, s1, s2, diff, n1, n2,sp, t, semd, ci)

#q6
n1 <- 100
n2 <- 100
m2 <- 4
s2 <- 0.5
m1 <- 6
s1 <- 2
diff <- m1 - m2
z <- qnorm(0.975)
sem <- ((s1^2 / n1) + (s2^2 / n2))^0.5
ci <- diff + c(-1, 1) * z * sem
round(ci, 3)
rm(n1, n2, m2, s2, m1, s1, diff, z, sem, ci)

# q7 (1 = placebo)
n1 <- 9
n2 <- 9
d1 <- 1
d2 <- -3
s1 <- 1.8
s2 <- 1.5
df <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2/n2)^2 / (n2 -1))
t <- qt(0.95, df = n1 + n2 - 2)
sp <- sqrt( ((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2) )
semd <- sp * (1/n1 + 1/n2)^0.5
diff <- d2 - d1
ci <- diff + c(-1,1) * t * semd
round(ci, 3)
rm(n1, n2, d1, d2, s1, s2, df, t, sp, semd, diff, ci)
