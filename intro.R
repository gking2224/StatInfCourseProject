set.seed(20150107)

plot.rexp.sample <- function(s, lambda) {
    
    pop.mean <- 1 / lambda
    pop.sd <- 1 / lambda
    pop.variance <- pop.sd^2
    
    r <- rexp(s, lambda)
    mean <- mean(r)
    variance <- var(r)
    
    library(ggplot2)
    g <- ggplot(data=data.frame(x=r), aes(x=x))
    g <- g + geom_density(size=2, alpha=0.2, fill="red")
    g <- g + geom_vline(
        size=2,
        alpha=0.5,
        xintercept = c(pop.mean, mean, pop.variance, variance),
        colour = c("red", "blue", "green4", "gold4"),
        lty = c(1,2,1,2))
    g <- g + annotate(
        "text",
        alpha = 0.8,
        label = c(
            paste0("Population Mean (", pop.mean, ")"),
            paste0("Sample Mean (", round(mean, 3), ")"),
            paste0("Population Variance (", round(pop.variance, 3), ")"),
            paste0("Sample Variance (", round(variance, 3), ")")),
        x = c(pop.mean, mean, pop.variance, variance),
        y = c(0.05, 0.06, 0.05, 0.03),
        hjust = -c(
            ifelse(pop.mean > mean, 0.02, -1.02),
            ifelse(pop.mean > mean, -1.02, 0.02),
            ifelse(pop.variance>max(r), -1.02, 0.02),
            -1.02),
        colour = c("red", "blue", "green4", "gold4"))
    g <- g + ggtitle(
        paste("Density of", s, "rexp variables (lambda=", lambda, ")"))
    g <- g + theme(plot.title = element_text(size = rel(2)))
    
    print(paste("population mean:", pop.mean))
    print(paste("sample mean:", round(mean, 3)))
    
    print(paste("population variance:", pop.variance))
    print(paste("sample variance:", round(variance, 3)))
    
    return(g)
}

print(plot.rexp.sample(s = 40, lambda = 0.2))