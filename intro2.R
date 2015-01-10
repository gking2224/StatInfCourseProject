dd <- data.frame(
    stat = c("lambda", "mu", "sigma", "sigma2", "n", "sem"),
    desc = c("rate parameter", "mean", "standard deviation",
             "variance", "number of samples", "standard error of the mean"),
    value = c(lambda, mu, sigma, sigma2, n, sem))
print(dd)
