s <- 10000
pop.mean <- 5
pop.sd <- 5
pop.variance <- pop.sd^2

r <- rnorm(s, mean = pop.mean, sd = pop.sd)
mean <- mean(r)

g <- ggplot(data=data.frame(x=r), aes(x=x))
g <- g + geom_density()
g <- g + geom_vline(xintercept = pop.mean, colour="red")
g <- g + geom_vline(xintercept = mean, colour="blue")
g <- g + ggtitle(paste("Density of", s, "rnorm variables (lambda=", lambda, ")"))
print(g)

print(paste("sample size:", s))
print(paste("population mean:", pop.mean))
print(paste("sample mean:", round(mean, 3)))
print(paste("difference:", round(mean - pop.mean, 3)))

variance <- sum((r - mean)^2) / (s - 1)

print(paste("population variance:", pop.variance))
print(paste("sample variance:", round(variance, 3)))
print(paste("difference:", round(variance - pop.variance, 3)))
