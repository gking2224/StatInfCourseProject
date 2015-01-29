n <- 9
xbar <- 1100
s <- 30
sem <- s / sqrt(n)
alpha <- 0.05
t <- qt(1 - alpha/2, df = n-1)
range <- xbar +c(-1,1) * t * sem

print(range, 0)