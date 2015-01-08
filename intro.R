lambda <- 0.2
pop.mean <- 1 / lambda
pop.sd <- 1 / lambda
pop.variance <- pop.sd^2
s <- 40
set.seed(20150107)

r <- rexp(s, lambda)
mean <- mean(r)

library(ggplot2)
g <- ggplot(data=data.frame(x=r), aes(x=x))
g <- g + geom_density()
g <- g + geom_vline(xintercept = pop.mean, colour="red")
g <- g + geom_vline(xintercept = mean, colour="blue")
g <- g + ggtitle(paste("Density of", s, "rexp variables (lambda=", lambda, ")"))
print(g)

print(paste("population mean:", pop.mean))
print(paste("sample mean:", round(mean, 3)))
print(paste("difference:", round(mean - pop.mean, 3)))

variance <- sum((r - mean)^2) / (s - 1)

print(paste("population variance:", pop.variance))
print(paste("sample variance:", round(variance, 3)))
print(paste("difference:", round(variance - pop.variance, 3)))
