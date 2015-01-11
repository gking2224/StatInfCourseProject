## @knitr tgIntro
library(datasets)
data(ToothGrowth)
str(ToothGrowth)

## @knitr tgIntro2
apply(matrix(
        with(ToothGrowth, is.na(c(len, supp, dose))), 60,
        dimnames = list(1:60, c("len", "supp", "dose"))),
    2, sum)