## @knitr expSampleCalc
set.seed(20150110)

# take a sample of rexp variables
sample <- rexp(n * numsims, rate = lambda)

# calculate the sample mean and variance
xbar <- sum(sample) / length(sample)
s2 <- sum( (sample - xbar)^2) / (length(sample) - 1)
s <- sqrt(s2)

## @knitr expSamplePlot

# population exp distribution values to plot
ed.sd <- seq(0, 55, by=55/39999)
# exp distribution density values
ed <- dexp(ed.sd, rate = lambda)

# make a plot of the sample values, with sample mean, sample variance,
# population mean and population variance overlayed
library(ggplot2)

g1  <- ggplot(
        data = data.frame(x = sample,
                          nd = ed,
                          y = ed.sd), aes(x = x)) +
    geom_density(size = lineWidth, alpha = 0.2, fill = col1) +
    geom_line(alpha = 1, size = 1, colour = col5,
              lty = 9, aes(x = ed.sd, y = ed)) +
    geom_vline(
        size = lineWidth, alpha = lineAlpha,
        xintercept = c(mu, xbar, sigma2, s2),
        colour = c(col1, col2, col3, col4), lty = c(1, 2, 1, 2)) +
    annotate(
        "text",
        size = annotationTextSize, alpha = textAlpha,
        label = c(
            paste0("Population Mean (", mu, ")"),
            paste0("Sample\nMean\n(", round(xbar, 3), ")"),
            paste0("Population Variance (", round(sigma2, 3), ")"),
            paste0("Sample Variance (", round(s2, 3), ")")),
        x = c(mu, xbar, sigma2, s2),
        y = c(0.1, 0.05, 0.05, 0.03),
        hjust = c(-0.06, 1.06, -0.06, 1.06),
        colour = c(col1, col2, col3, col4)) +
    annotate(
        "text", size = annotationTextSize,
        alpha = textAlpha, parse = TRUE, label = "exp(lambda)",
        x = 0.5, y = dexp(0.5, rate = lambda), hjust = -0.06, colour = col5) +
    ggtitle(paste(
        "Figure 1: Density of", length(sample),
        "rexp variables (lambda=", lambda, ")")) +
    labs(y = "Density") +
    theme(plot.title = element_text(size = titleSize))

print(g1)
