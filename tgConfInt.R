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

## @knitr ci3
aa <- ToothGrowth[ToothGrowth$supp == "Ascorbic Acid","len"]
oj <- ToothGrowth[ToothGrowth$supp == "Orange Juice", "len"]
diff <- oj - aa
mn <- mean(diff)
s <- sd(diff)
n <- 30
ci3 <- mn + c(-1,1)*qt(0.975, df = n - 1)* s / sqrt(n)
print(ci3)


## @knitr ci4
d0p5 <- ToothGrowth[ToothGrowth$dose == 0.5,"len"]
d2 <- ToothGrowth[ToothGrowth$dose == 2.0, "len"]
diff <- d2 - d0p5
mn <- mean(diff)
s <- sd(diff)
n <- length(d0p5)
ci4 <- mn + c(-1,1) * qt(0.975, df = n - 1) * s / sqrt(n)
print(ci4)


## @knitr ci5
d1aa <- ToothGrowth[ToothGrowth$dose == 0.5 &
                        ToothGrowth$supp == "Ascorbic Acid","len"]
d2oj <- ToothGrowth[ToothGrowth$dose == 2.0 &
                        ToothGrowth$supp == "Orange Juice", "len"]
diff <- d2oj - d1aa
mn <- mean(diff)
s <- sd(diff)
n <- length(d1aa)
ci5 <- mn + c(-1,1) * qt(0.975, df = n - 1) * s / sqrt(n)
print(ci5)
ps2 <- ((length(d1aa) - 1) * var(d1aa) + (length(d2oj)-1) * var(d2oj)) /
    (length(d1aa) + length(d2oj) - 2)
print(ps2)

dvals <- seq(mean(ci5) - sqrt(ps2)*3, mean(ci5) + sqrt(ps2)*3, by = 0.2)
d <- data.frame(y = dnorm(dvals, mean = mean(ci5), sd = sqrt(ps2)), x = dvals)
g <- ggplot(d, aes(x = x, y = y)) + geom_line()
g <- g + geom_vline(xintercept = ci5, lty = 2)
print(g)


## @knitr ci6
m1 <- mean(d1aa)
m2 <- mean(d2oj)
n1 <- length(d1aa)
n2 <- length(d2oj)

m2 - m1 + c(-1,1) * qt(0.975, df = (n1 + n2 - 2)) * Sp * (1/n1 + 1/n2)^(0.5)5 *