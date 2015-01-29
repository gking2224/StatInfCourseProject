n <- 9
# 90% ci 1077-1123
l <- 1077
u <- 1123
mu <- u - ((u - l)/2)
sem <- (u - mu) / qt(.95, n-1)

stats <- (c(u, l) - mu) / sem
print(pt(abs(stats), n-1, lower.tail = FALSE), 5)