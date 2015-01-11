## @knitr tgExplore2
oj <- subset(ToothGrowth, supp==levels(ToothGrowth$supp)[1])
vc <- subset(ToothGrowth, supp==levels(ToothGrowth$supp)[2])
g2 <- ggplot(
    data = data.frame(
        stats = matrix(c(mean(oj$len), var(oj$len), mean(vc$len), var(vc$len)), nrow=4),
        stat = factor(rep(c("mean", "variance"), times=2)),
        supp = factor(rep(levels(ToothGrowth$supp), c(2,2)))),
    aes(x=supp, y=stats, fill=stat)) +
    geom_bar(stat="identity", alpha=0.8, position="dodge") +
    labs(x="Supplement", y="Value") +
    guides(fill = guide_legend(title="Statistic")) +
    ggtitle("Figure 2: Mean and variance of tooth length by Supplement") +
    theme(plot.title = element_text(size = rel(1.5)))
print(g2)
print(col6)