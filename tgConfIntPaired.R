library(plyr)
head(ToothGrowth)
ddply(ToothGrowth, .(dose, supp), summarize, mean = mean(len))