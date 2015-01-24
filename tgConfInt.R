## @knitr ci1
# split data by supplement, apply 95% T confidence interval on the mean of each subset
ci1 <- round(
    sapply(c("Orange Juice", "Ascorbic Acid"),
           function(s) {
               x <- ToothGrowth$len[ToothGrowth$supp==s]
               n <- length(x)
               mean(x) + c(-1, 1) * qt(0.975, n - 1) * sd(x) / sqrt(n)
            }), 2)
rownames(ci1) <- c("Lower Limit", "Upper Limit")
print(ci1)

## @knitr ci2
# split data by dose, apply 95% T confidence interval on the mean of each subset
ci2 <- round(
    sapply(c("0.5", "1", "2"),
           function(d) {
               x <- ToothGrowth$len[ToothGrowth$dose==d];
               n <- length(x)
               mean(x) + c(-1, 1) * qt(0.975, n - 1) * sd(x) / sqrt(n)
            }), 2)
rownames(ci2) <- c("Lower Limit", "Upper Limit")
print(ci2)

## @knitr cifunction
ci.unpaired.unequal.var <- function(x1, x2) {
    m1 <- mean(x1); s1 <- sd(x1); n1 <- length(x1)
    m2 <- mean(x2); s2 <- sd(x2); n2 <- length(x2)
    diff <- m2 - m1
    df <- ((s1^2/n1) + (s2^2/n2))^2 /
        (((s1^2/n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 -1)))
    sem <- ((s1^2 / n1) + (s2^2 / n2))^0.5
    ci <- diff + c(-1, 1) * qt(0.975, df) * sem
    return(ci)
}

## @knitr ci3
aa <- ToothGrowth[ToothGrowth$supp == "Ascorbic Acid","len"]
oj <- ToothGrowth[ToothGrowth$supp == "Orange Juice", "len"]
print(ci.unpaired.unequal.var(aa, oj))


## @knitr ci4
d0p5 <- ToothGrowth[ToothGrowth$dose == 0.5,"len"]
d2 <- ToothGrowth[ToothGrowth$dose == 2.0, "len"]
print(ci.unpaired.unequal.var(d0p5, d2))


## @knitr ci5
d1aa <- subset(ToothGrowth, dose == 0.5 & supp == "Ascorbic Acid")$len
d2oj <- subset(ToothGrowth, dose == 2.0 & supp == "Orange Juice")$len
print(ci.unpaired.unequal.var(d1aa, d2oj))