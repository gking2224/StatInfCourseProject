## @knitr expSampleMeans
# split the sample into a matrix of 'numsims' rows each containing 'n' samples
# and take the mean of each sample, leaving 1000 mean values
smp.means <- apply(matrix(sample, numsims), 1, mean)

# calculate the mean, variance and standard distribution of these sample means
muxbar <- sum(smp.means) / numsims
s2xbar <- sum( (smp.means - muxbar)^2 ) / (numsims - 1)
sxbar <- sqrt(s2xbar)

# make a plot of the sample means, with their mean, standard deviation overlayed
# together with the population mean and population standard error of the mean
g2 <- ggplot(data = data.frame(x = smp.means), aes(x = x)) +
    geom_density(alpha = 0.2, size = 2, fill = col1) +
    geom_vline(
        size = 2,
        alpha = 0.8,
        xintercept = c(
            mu,
            muxbar,
            mu + c(1,-1)*sem,
            muxbar + c(1,-1)*sxbar),
        colour = c(col1, col2, col3, col3, col4, col4),
        lty = c(1, 2, 1, 1, 2, 2)) +
    annotate(
        "text",
        size = 5,
        alpha = 0.8,
        x = c(
            mu, muxbar, mu - sem,
            muxbar + sxbar),
        y = c(0.2, 0.3, 0.5, 0.1),
        hjust = c(-0.06, 1.06, -0.06, 1.06),
        colour = c("red", col2, col3, col4),
        label = c(
            paste0("Population\nMean (", mu, ")"),
            paste0("Mean of Sample\nMeans (", round(muxbar,3), ")"),
            paste0("SEM\n(+/- ", round(sem,3), ")"),
            paste0("SD of\nSample Means\n(+/- ", round(sxbar,3),")")))+
    ggtitle(paste(
        "Figure 2: Distribution of the means of", numsims, "samples of", n,
        "\nrexp distribution variables (lambda=", lambda, ")")) +
    labs(y = "Density") +
    theme(plot.title = element_text(size = rel(1.5)))

print(g2)
