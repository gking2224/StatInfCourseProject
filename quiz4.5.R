diff0 <- 1
diff1 <- -3
s0 <- 1.8
s1 <- 1.5
n0 <- 9
n1 <- 9

sp <- sqrt(( (n0-1 * s0^2) + (n1-1 * s1^2) ) / (n0 + n1 - 2))

pt((diff1 - diff0), df = 16)
