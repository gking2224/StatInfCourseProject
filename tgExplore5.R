## @knitr tgExplore5

g5 <- ggplot(data = ToothGrowth, aes(x = len, fill=supp, shape = dose))+ geom_density()

print(g5)