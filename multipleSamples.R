sims <- 1000
pop.mean <- 0
pop.sd <- 1
pop.var <- pop.sd^2

x <- NULL
sample.sizes <- c(seq(10, 10, by = 10), 100)
apply(matrix(sample.sizes), 1, function(ss) {
    x <<- c(x,
            apply(matrix(
                rnorm(sims*ss, mean = pop.mean, sd = pop.sd), sims), 1, mean))
})

data <- data.frame(x = x,
                   n = factor(
                       paste0("", rep(
                           sample.sizes,
                           rep(sims, length(sample.sizes)))),
                       levels=paste0("", sample.sizes)))
library(ggplot2)
g <- ggplot(data, aes(x = x, fill = n))
g <- g + geom_density(size = 2, alpha = 0.2)
g <- g + geom_vline(size = 2, xintercept = pop.mean)
print(g)
