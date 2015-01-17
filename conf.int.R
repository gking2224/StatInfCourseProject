conf.int <- function(
    x = NA, m = NA, s = NA, n = NA,
    y = NA, m2 = NA, s2 = NA, n2 = NA,
    conf.level = 0.975, t = TRUE, z = FALSE,
    paired = FALSE, var.equal = FALSE) {
    
    na.check1 <- sum(is.na(c(m, s, n)))
    if (na.check1 != 0 & na.check1 != 3)
        stop("either all of m, s, n should be given or none")
    na.check2 <- sum(is.na(c(m2, s2, n2)))
    if (na.check2 != 0 & na.check2 != 3)
        stop("either all of m2, s2, n2 should be given or none")
    
    if ((!is.na(x) & !is.na(m)) | (is.na(x) & is.na(m)))
        stop("either x should be given or all of m, s and n")
    
    if ((t & z) | (!t & !z)) stop("one of t or z must be TRUE")
    
    if (!is.na(x)) {
        m <- mean(x)
        s <- sd(x)
        n <- length(x)
    }
    if (!is.na(y)) {
        m2 <- mean(y)
        s2 <- sd(y)
        n2 <- length(y)
    }
    if (conf.level > 1) {
        conf.level = 1 - (1 - (conf.level / 100))/2
    }
    if (is.na(m2)) {
        inner.conf.int.1(m, s, n, conf.level, t)
    }
    else {
        inner.conf.int.2(m, s, n, m2, s2, n2, conf.level, t, paired, var.equal)
    }
}

inner.conf.int.1 <- function(m, s, n, conf.level, t) {
    sem <- s / n^0.5
    t <- ifelse(t, qt(conf.level, df = n - 1), qnorm(conf.level))
    m + c(-1,1) * t * sem
}

inner.conf.int.2 <- function(
    m1, s1, n1, m2, s2, n2, conf.level, t, paired, var.equal) {
    
    diff <- m2 - m1
    if (var.equal) {
        sp <- sqrt( ((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2) )
        semd <- sp * (1/n1 + 1/n2)^0.5
        tdf <- (n1 + n2 - 2)
    }
    else {
        # unequal variance
        semd <- ((s1^2 / n1) + (s2^2 / n2))^0.5
        tdf <- (s1^2 / n1 + s2^2 / n2)^2 / ((s1^2/n1)^2 / (n1 - 1)) + ((s2^2/n2)^2 / (n2 - 1))
    }
    t <- ifelse(t, qt(conf.level, df = tdf), qnorm(conf.level))
    diff + c(-1,1)* t * semd
}