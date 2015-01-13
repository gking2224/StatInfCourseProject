# q1

# q2
q2 <- pnorm(mean = 80, sd = 10, q = 70, lower.tail = TRUE)

# q3
q3 <- qnorm(.95, mean = 1100, sd = 75)

# q4
q4 <- qnorm(.95, mean = 1100, sd = sqrt((75^2)/100))

# q5
q5 <- pbinom(size = 5, q = 4, prob = .5, lower.tail = FALSE)
q5 <- round(sum(dbinom(size = 5, x = 4:5, prob = 0.5)), 2)

# q6
q6 <- pnorm(mean = 15, q = 14, lower.tail = FALSE, sem) - pnorm(mean = 15, q = 16, lower.tail = FALSE, sd = sem)

# q7

# q8
q8 <- round(ppois(lambda = 5 * 3, q = 10), 2)
