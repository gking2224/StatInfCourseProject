
g1 <- sleep$extra[1:10]; g2 <- sleep$extra[11:20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- length(difference)
t <- qt(0.975, n - 1)
paired.conf.int <- mn + c(-1, 1) * t * s * sqrt(1/n)

m1 <- mean(g1)
m2 <- mean(g2)
n1 <- 10
n2 <- 10
sp <- sqrt( ((n1 - 1) * sd(g1)^2 + (n2 - 1) * sd(g2)^2) / (n1 + n2 - 2) )
unpaired.t <- qt(0.975, n1 + n2 - 2)
unpaired.conf.int <- (m2 - m1) + c(-1, 1) * unpaired.t * sp * sqrt(1/n1 + 1/n2)
comp <- rbind(paired.conf.int, unpaired.conf.int)
print(round(comp, 4))

data(sleep)
mean.df <- data.frame(m = matrix(c(m1, m2)), group = factor(c(1,2)), ID = "m")
g <- ggplot(data = sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(aes(colour = ID))
g <- g + geom_line(aes(x = group, y = m, group = factor(ID)), mean.df, lty = 2)
g <- g + geom_point(size =8, pch = 21, fill = "salmon", alpha = .5)
print(g)
