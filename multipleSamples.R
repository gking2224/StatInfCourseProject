s <- 10
n <- 100
mean <- 3
sd <- 3

d <- NULL
apply(matrix(1:3), 1, function(s) {
    d <- apply(matrix(rnorm(s*10)), 1, var)
})
d <- apply(matrix(rnorm(10)), 1, var)
print(d)

library(ggplot2)
