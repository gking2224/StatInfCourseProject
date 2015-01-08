n <- 1000
s <- 40
pop.mn <- 5
pop.sd <- 5
pop.variance <- pop.sd^2

mns <- NULL
for (i in 1:n)
    mns <- c(mns, mean(rnorm(s, mean = pop.mn, sd = pop.sd)))

m.mns <- mean(mns)

g <- ggplot(data=data.frame(x=mns), aes(x=x))
g <- g + geom_density()
g <- g + geom_vline(xintercept = pop.mn, colour="red")
g <- g + geom_vline(xintercept = m.mns, colour="blue")
print(g)

print(paste("population mean:", round(pop.mean, 3)))
print(paste("mean of sample means:", round(m.mns, 3)))
print(paste("difference:", round(pop.mn - m.mns, 3)))

variance <- sum( (mns - m.mns)^2 ) / (n - 1)

print(paste("population variance:", pop.variance))
print(paste("sample variance:", round(variance, 3)))
print(paste("difference:", round(variance - pop.variance, 3)))

