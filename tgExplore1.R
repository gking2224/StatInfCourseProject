## @knitr explore1
library(ggplot2)

means <- with(ToothGrowth, tapply(len, supp, mean))
sds <- with(ToothGrowth, tapply(len, supp, sd))
colours <- c(col1, col2)
g1 <- ggplot(
        data=ToothGrowth,
        aes(x = len, fill = supp)) +
    scale_fill_manual(values = colours) +
    geom_density(alpha = fillAlpha, size = lineWidth) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Supplement")) +
    ggtitle("Figure 1: Distribution of tooth length by Supplement") +
    theme(plot.title = element_text(size = titleSize)) +
    geom_vline(
        size = lineWidth,
        alpha = lineAlpha,
        xintercept = c(means, means + sds, means - sds),
        colour = rep(colours, times=3),
        lty = rep(c(1, 2), c(2,4))) +
    annotate("text", size = annotationTextSize,
             label = rep(c(
                            paste0("Mean\n(", round(means, 2), ")"),
                            paste0("+1 sd\n(",round(sds,2),")"),
                            "-1 sd"),
                         c(1, 1, 1, 1, 2)),
             x = c(means, means + sds, means - sds),
             y = rep(c(0.05, 0.06), times=3),
             hjust = 1.08,
             alpha = textAlpha,
             colour = rep(colours, times=3))
    
print(g1)
