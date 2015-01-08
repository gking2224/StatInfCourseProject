s <- 40
n <- 1000
lambda <- 0.2

pop.mean <- 1 / lambda
pop.sd <- 1 / lambda
pop.variance <- pop.sd^2

set.seed(20150105)

mns <- NULL
for (i in 1:n)
    mns <- c(mns,mean(rexp(s,lambda)))

m.mns <- mean(mns)

g <- ggplot(data=data.frame(x=mns), aes(x=x))
g <- g + geom_density()
g <- g + geom_vline(xintercept = pop.mean, colour="red")
g <- g + geom_vline(xintercept = m.mns, colour="blue")
print(g)

print(paste("population mean:", round(pop.mean, 3)))
print(paste("mean of sample means:", round(m.mns, 3)))
print(paste("difference:", round(pop.mean - m.mns, 3)))

variance <- sum( (mns - m.mns)^2 ) / (n - 1)

print(paste("population variance:", round(pop.variance, 3)))
print(paste("sample means variance:", round(variance, 3)))
print(paste("difference:", round(variance - pop.variance, 3)))
