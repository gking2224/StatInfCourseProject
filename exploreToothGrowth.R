library(datasets)
data(ToothGrowth)


ToothGrowth$dose <- factor(ToothGrowth$dose)
levels(ToothGrowth$supp) <- c("Orange Juice", "Ascorbic Acid")

library(ggplot2)


g1 <- ggplot(data=ToothGrowth, aes(x = len, fill=supp)) +
    geom_density(alpha=0.2) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Supplement")) +
    ggtitle("Figure 1: Distribution of tooth length by Supplement") +
    theme(plot.title = element_text(size = rel(1.5)))
    
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


g3 <- ggplot(data=ToothGrowth, aes(x = len, fill = dose)) +
    geom_density(alpha = 0.2) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Dose (mg)")) +
    ggtitle("Figure 3: Distribution of tooth length by Dose") +
    theme(plot.title = element_text(size = rel(1.5)))

g4 <- ggplot(data=ToothGrowth, aes(x = len, fill=dose)) +
    geom_density(alpha = 0.2) +
    facet_grid(. ~ supp) +
    labs(x="Tooth length", y="Density") +
    guides(fill = guide_legend(title="Dose (mg)")) +
    ggtitle("Figure 4: Distribution of tooth length by Dose and Supplement") +
    theme(plot.title = element_text(size = rel(1.5)))

g5 <- ggplot(data = ToothGrowth, aes(x = len, fill=supp, shape = dose))+ geom_density()

print(g5)