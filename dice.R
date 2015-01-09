
s <- 40
nosims  <- 1000

r <- sample(x = 6, size = s*nosims, replace = TRUE)

g1 <- ggplot(data = data.frame(x=r), aes(x=x)) +
    geom_bar()

print(g1)