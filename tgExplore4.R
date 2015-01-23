## @knitr tgExplore4
library(plyr)
vline.data <- ddply(
    ToothGrowth, .(supp, dose), summarize,
    mean = mean(len), sd = sd(len))
g4 <- ggplot(data=ToothGrowth, aes(x = len, fill = dose)) +
    geom_density(alpha = fillAlpha) +
    facet_grid(. ~ supp) +
    geom_vline(aes(xintercept=mean, colour = dose), vline.data) +
    geom_vline(aes(xintercept=mean + sd, colour = dose), vline.data, lty = 2) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Dose (mg)")) +
    ggtitle("Figure 4: Distribution of tooth length by Dose and Supplement") +
    theme(plot.title = element_text(size = titleSize))
print(g4)