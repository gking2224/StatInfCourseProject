
set.seed(20150111)

plot.sample.means <- function(s, n, lambda) {
    pop.mean <- 1 / lambda
    pop.sd <- 1 / lambda
    pop.variance <- pop.sd^2
    
    mns <- NULL
    for (i in 1:n) {
        mns <- c(mns, mean(rexp(s, lambda)))
    }
    
    m.mns <- mean(mns)
    variance <- var(mns)
    
    g <- ggplot(data=data.frame(x=mns), aes(x=x))
    g <- g + geom_density(alpha=0.2, size=2, fill="red")
    g <- g + geom_vline(
        size=2, alpha=0.5,
        xintercept = c(
            pop.mean,
            m.mns,
            m.mns - (c(1,-1)*variance)),
        colour = c("red", "blue", "gold4", "gold4"),
        lty=c(1, 2, 2, 2))
    g <- g + annotate(
        "text",
        alpha = 0.8,
        x = c(pop.mean, m.mns, m.mns + variance),
        y = c(0.15, 0.1, 0.05),
        hjust=c(
            -0.02, 1.02,
            ifelse(m.mns + variance > max(mns)*.8, 1.02, -0.02)),
        colour=c("red", "blue", "gold4"),
        label = c(
            paste0("Population Mean (", pop.mean, ")"),
            paste0("Mean of Sample Means (", round(m.mns,3), ")"),
            paste0("Variance (+/-", round(variance, 3),")" )))
    g <- g + ggtitle(paste(
        "Distribution of the means of", n, "samples of", s,
        "\nrexp distribution variables (lambda=", lambda, ")"))
    g <- g + theme(plot.title = element_text(size = rel(2)))
    
    print(paste("population mean:", round(pop.mean, 3)))
    print(paste("mean of sample means:", round(m.mns, 3)))
    
    print(paste("population variance:", round(pop.variance, 3)))
    print(paste("sample means variance:", round(variance, 10)))
    
    return(g)
}

print(plot.sample.means(s = 40, n = 1000, lambda = 0.2))
