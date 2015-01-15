# mu <- sample(x = 100:100000, size = 1) / 100
# sigma <- sample(x = 1:1000, size = 1) / 100
# n <- sample(x = 10:2000, size = 1)
library(ggplot2)
plot1 <- function(mu, sigma = 1, n = 100) {
    r <- rnorm(n, mean = mu, sd = sigma)
    xbar <- mean(r)
    s <- sd(r)
    sem <- s / sqrt(n)
    ci <- m + c(-1,1)*qt(0.975, df = 9)*sem
    
    g <- ggplot(data.frame(x = r), aes(x = x))
    g <- g + geom_density()
    g <- g + geom_vline(xintercept = xbar)
    g <- g + geom_vline(xintercept = xbar + c(-1,1)*qt(0.975, df = 9)*sem, lty = 2)
    g <- g + geom_vline(xintercept = mu, colour = "red")
    g <- g + geom_vline(xintercept = xbar + c(-1,1)*qnorm(0.975)*sem, lty = 3, colour = "green")
    return(g)
}
manipulate(plot1(mu, sigma, n), mu = slider(1, 100, label = "mu"),
           sigma = slider(0.1, 10, step = 0.1, label = "sigma"),
           n = slider(5, 2005, step = 50, label = "n"))