## @knitr tgExplore4
g4 <- ggplot(data=ToothGrowth, aes(x = len, fill=dose)) +
    geom_density(alpha = fillAlpha) +
    facet_grid(. ~ supp) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Dose (mg)")) +
    ggtitle("Figure 4: Distribution of tooth length by Dose and Supplement") +
    theme(plot.title = element_text(size = titleSize))
print(g4)