mu0 <- 30
alpha <- 0.05
delta <- 5
mua <- mu0 + delta
n <- 6
sigma <- 4
sem <- sigma / sqrt(n)
t <- qt(1-alpha, n-1)
z <- qnorm(1-alpha)
desired.power <- 0.8

samples <- 100

x <- c(apply(matrix(rnorm(samples*n, mean = mu0, sd = sigma), samples, n), 1, mean),
       apply(matrix(rnorm(samples*n, mean = mua, sd = sigma), samples, n), 1, mean))
truth <- factor(rep(c("H0", "Ha"), each=samples) )
reject <- pt((x-mu0)/sem, lower.tail = FALSE, df = n-1) < alpha

m <- matrix(table(truth, reject), 2, 2, 
            dimnames = list(c("H0", "Ha"), c("FALSE", "TRUE")))
t1e <- m[1,2] / samples
t2e <- m[2,1] / samples
ap <- 1 - t2e
print(paste("actual power:", ap))

tp <- power.t.test(
    delta = delta, sd = sigma, n = n,
    type = "one.sample", alternative = "one.sided")$power
nn <- ceiling(power.t.test(
    sig.level = alpha,
    delta = delta, sd = sigma, power = desired.power,
    type = "one.sample", alternative = "one.sided")$n)
print(paste0("n needed for power=", desired.power, ": ", nn))
print(paste("theoretical power:", tp))

ctp <- pnorm(mu0 + t*sem,
             mean = mu0+delta, sd = sem, lower.tail = FALSE)
print(paste("calculated theoretical power:", ctp))
