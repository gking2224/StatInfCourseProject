pvals <- seq(0.5, 0.99, by = 0.01)
myplot2 <- function(df) {
    
    d <- data.frame(n = qnorm(pvals), t = qt(pvals, df = df),
                    p = pvals)
    g <- ggplot(d, aes(x = n, y = t))
    g <- g + geom_line(col = "black")
    g <- g + geom_abline(col = "lightblue")
    g
}
manipulate(myplot2(df), df = slider(1, 20))
