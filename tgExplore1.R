## @knitr tgExplore1
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
        xintercept = c(means, means + sds),
        colour = rep(colours, times=2),
        lty = rep(c(1, 2), c(2,2))) +
    annotate("text", size = annotationTextSize,
             label = c(
                 paste0("Mean\n(", round(means, 2), ")"),
                 paste0("+1 sd\n(",round(sds,2),")")),
             x = c(means, means + sds),
             y = rep(c(0.05, 0.06), times=2),
             hjust = 1.08,
             alpha = textAlpha,
             colour = rep(colours, times=2))
    
print(g1)
