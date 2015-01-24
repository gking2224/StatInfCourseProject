## @knitr tgExplore3
library(plyr)
means <- with(ToothGrowth, tapply(len, dose, mean))
sds <- with(ToothGrowth, tapply(len, dose, sd))
colours <- c(col1, col2, col3)
g3 <- ggplot(data=ToothGrowth, aes(x = len, fill = dose)) +
    geom_density(alpha = fillAlpha, size = lineWidth) +
    scale_fill_manual(values = colours) +
    geom_vline(
        size = lineWidth, alpha = lineAlpha, xintercept = c(means, means + sds),
        colour = rep(colours, times = 2), lty = rep(c(1,2), c(3, 3))) +
    annotate("text", size = annotationTextSize,
             label = c(paste0("Mean\n(", round(means, 2), ")"),
                 paste0("sd=",round(sds,2))),
             x = c(means, means + sds), y = rep(c(0.11, 0.105, 0.12), times=2),
             hjust = 1.08, alpha = textAlpha, colour = rep(colours, times=2)) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Dose (mg)")) +
    ggtitle("Figure 2: Distribution of tooth length by Dose") +
    theme(plot.title = element_text(size = titleSize))
print(g3)