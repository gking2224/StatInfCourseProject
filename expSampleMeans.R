## @knitr expSampleMeansCalc
# split the sample into a matrix of 'numsims' rows each containing 'n' samples
# and take the mean of each sample, leaving 1000 mean values
smp.means <- apply(matrix(sample, numsims), 1, mean)

# calculate the mean, variance and standard distribution of these sample means
muxbar <- sum(smp.means) / numsims
s2xbar <- sum( (smp.means - muxbar)^2 ) / (numsims - 1)
sxbar <- sqrt(s2xbar)

## @knitr expSampleMeansPlot
# population normal distribution sd values to plot
nd.sd <- seq(mu-sem*3, mu+sem*3, by=((sem*6)/999))
# normal distribution density values
nd <- dnorm(nd.sd, mean = mu, sd = sem)

# make a plot of the sample means, with their mean and standard deviation
# overlayed together with the population normal distribution
g2 <- ggplot(
        data = data.frame(x = smp.means, 
                          nd = nd,
                          y = nd.sd)) +
    geom_density(alpha = fillAlpha, size = lineWidth, fill = col1, aes(x = x)) +
    geom_line(alpha = 1, size = lineWidth, colour = col5,
              lty = 9, aes(x = nd.sd, y = nd)) +
    scale_x_continuous(
        breaks = seq(mu-sem*3, mu+sem*3, by = sem),
        labels = mapply(
            function(n, s) {
                ifelse(s == 0,
                       paste0(round(n, 3), " (population mean)"),
                       paste0(round(n, 3), "\n(", ifelse(s>0,"+",""), s, "sem)"))
            },
            seq(mu-sem*3, mu+sem*3, by = sem),
            -3:3)) +
    geom_vline(
        size = lineWidth,
        alpha = lineAlpha,
        xintercept = c(
            muxbar,
            muxbar + c(1,-1) * sxbar),
        colour = c(col2, col3, col3),
        lty = 2) +
    annotate(
        "text",
        size = annotationTextSize,
        alpha = textAlpha,
        x = 5.9,
        y = dnorm(5.9, mean = mu, sd = sem),
        hjust = -0.06,
        colour = col5, parse = TRUE,
        label = "~N(mu,~sigma^2/n)") +
    annotate(
        "text",
        size = annotationTextSize,
        alpha = textAlpha,
        x = c(muxbar, muxbar + sxbar, muxbar - sxbar),
        y = c(0.3, 0.5, 0.5),
        hjust = c(1.06, -0.06, 1.06),
        colour = c(col2, col3, col3),
        label = c(
            paste0("Mean of\nSample Means\n(", round(muxbar, 3), ")"),
            paste0("+", round(sxbar, 3)," (1sd)"),
            paste0("-", round(sxbar, 3)," (-1sd)")))+
    ggtitle(paste(
        "Figure 2: Distribution of the means of", numsims, "samples of", n,
        "\nrexp distribution variables (lambda=", lambda, ")")) +
    labs(x = "Sample Mean", y = "Density") +
    theme(plot.title = element_text(size = titleSize))

print(g2)
