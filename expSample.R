## @knitr expSample
set.seed(20150110)

# take a sample of rexp variables
sample <- rexp(n * numsims, lambda)

# calculate the sample mean and variance
xbar <- sum(sample) / length(sample)
s2 <- sum( (sample - xbar)^2) / (length(sample) - 1)

# make a plot of the sample values, with sample mean, sample variance,
# population mean and population variance overlayed
library(ggplot2)
col1 <- "red"
col2 <- "blue"
col3 <- "green4"
col4 <- "chocolate1"

g1  <- ggplot(
    data = data.frame(x = sample), aes(x = x)) +
    geom_density(size = 2, alpha = 0.2, fill = col1) +
    geom_vline(
        size = 2,
        alpha = 0.8,
        xintercept = c(mu, xbar, sigma2, s2),
        colour = c(col1, col2, col3, col4),
        lty = c(1, 2, 1, 2)) +
    annotate(
        "text",
        size = 5,
        alpha = 0.8,
        label = c(
            paste0("Population Mean (", mu, ")"),
            paste0("Sample\nMean\n(", round(xbar, 3), ")"),
            paste0("Population Variance (", round(sigma2, 3), ")"),
            paste0("Sample Variance (", round(s2, 3), ")")),
        x = c(mu, xbar, sigma2, s2),
        y = c(0.1, 0.05, 0.05, 0.03),
        hjust = c(-0.06, 1.06, -0.06, 1.06),
        colour = c(col1, col2, col3, col4)) +
    ggtitle(paste(
        "Figure 1: Density of", length(sample),
        "rexp variables (lambda=", lambda, ")")) +
    labs(y = "Density") +
    theme(plot.title = element_text(size = rel(1.5)))

print(g1)
