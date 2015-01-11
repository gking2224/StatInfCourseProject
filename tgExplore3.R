## @knitr tgExplore3

means <- with(ToothGrowth, tapply(len, dose, mean))
sds <- with(ToothGrowth, tapply(len, dose, sd))

colours <- c(col1, col2, col3)
g3 <- ggplot(data=ToothGrowth, aes(x = len, fill = dose)) +
    geom_density(alpha = fillAlpha, size = lineWidth) +
    scale_fill_manual(values = colours) +
    geom_vline(
        size = lineWidth,
        alpha = lineAlpha,
        xintercept = c(means, means + sds, means - sds),
        colour = rep(colours, times = 3),
        lty = rep(c(1,2), c(3, 6))) +
    annotate("text", size = annotationTextSize,
             label = rep(c(
                 paste0("Mean\n(", round(means, 2), ")"),
                 paste0("+1 sd\n(",round(sds,2),")"),
                 "-1 sd"),
                 c(rep(1, 6), 3)),
             x = c(means, means + sds, means - sds),
             y = rep(c(0.1, 0.085, 0.12), times=3),
             hjust = c(-0.08, 1.08, 1.08, 1.08, 1.08, 1.08, 1.08, -0.08, 1.08),
             alpha = textAlpha,
             colour = rep(colours, times=3)) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Dose (mg)")) +
    ggtitle("Figure 3: Distribution of tooth length by Dose") +
    theme(plot.title = element_text(size = titleSize))
print(g3)