## @knitr tgClean
ToothGrowth$dose <- factor(ToothGrowth$dose)
levels(ToothGrowth$supp) <- c("Orange Juice", "Ascorbic Acid")
