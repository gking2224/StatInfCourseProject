## @knitr ci1
ci1 <- round(
    sapply(c("Orange Juice", "Ascorbic Acid"),
           function(s) {
               x <- ToothGrowth$len[ToothGrowth$supp==s];
               mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)
            }
    ), 2)
rownames(ci1) <- c("Lower Limit", "Upper Limit")
print(ci1)

## @knitr ci2
ci2 <- round(
    sapply(c("0.5", "1", "2"),
           function(d) {
               x <- ToothGrowth$len[ToothGrowth$dose==d];
               mean(x) + c(-1, 1) * qnorm(0.975) * sd(x)
            }
    ), 2)

rownames(ci2) <- c("Lower Limit", "Upper Limit")
print(ci2)
