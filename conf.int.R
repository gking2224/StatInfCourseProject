conf.int <- function(
    v = NA, m = NA, s = NA, n = NA,
    v2 = NA, m2 = NA, s2 = NA, n2 = NA,
    int = 0.975, t = TRUE, z = FALSE, paired = FALSE) {
    
    na.check1 <- sum(is.na(c(m, s, n)))
    if (na.check1 != 0 & na.check1 != 3)
        stop("either all of m, s, n should be given or none")
    na.check2 <- sum(is.na(c(m2, s2, n2)))
    if (na.check2 != 0 & na.check2 != 3)
        stop("either all of m2, s2, n2 should be given or none")
    
    if ((!is.na(v) & !is.na(m)) | (is.na(v) & is.na(m)))
        stop("either v should be given or all of m, s and n")
    
    if ((t & z) | (!t & !z)) stop("one of t or z must be TRUE")
    
    if (!is.na(v)) {
        m <- mean(v)
        s <- sd(v)
        n <- length(v)
    }
    if (!is.na(v2)) {
        m2 <- mean(v2)
        s2 <- sd(v2)
        n2 <- length(v2)
    }
    if (int > 1) {
        int = 1 - (1 - (int / 100))/2
    }
    if (is.na(m2)) {
        inner.conf.int.1(m, s, n, int, t)
    }
    else {
        inner.conf.int.2(m, s, n, m2, s2, n2, int, t, paired)
    }
}

inner.conf.int.1 <- function(m, s, n, int, t) {
    sem <- s / n^0.5
    t <- ifelse(t, qt(int, df = n - 1), qnorm(int))
    m + c(-1,1) * t * sem
}

inner.conf.int.2 <- function(
    m1, s1, n1, m2, s2, n2, int, t, paired) {
    
    diff <- m2 - m1
    sp <- sqrt( ((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2) )
    t <- ifelse(t, qt(int, df = (n1 + n2 - 2)), qnorm(int))
    semd <- sp * (1/n1 + 1/n2)^0.5
    c(-1,1)* t * semd
}