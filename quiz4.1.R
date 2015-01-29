x1 <- c(140,138,150,148,135)
x2 <- c(132, 135, 151, 146, 130)
n <- 5
diffs <- x2 - x1
xbar <- mean(diffs)
s <- sd(diffs)
sem <- s / sqrt(n)
#null hypothesis says mean difference should be zero:
mu0 <- 0
pval <- pt((xbar - mu0)/sem, df = n-1)
print(round(pval, 3))
rm(x1, x2, n, diffs, xbar, s, sem, mu0, pval)