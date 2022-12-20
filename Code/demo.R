## This note checks the empirical coverage rate of CIs constructed from
## block bootstrap with series generated from an MA process

mystat <- function(x) {
    c(mean(x),
      sd(x),
      cor(x[-1], x[-length(x)]))
      # acf(x, lag.max = 1, plot = FALSE)$acf[2])
}

do1rep <- function(n, theta, statistic, blksize, R = 1000) {
    x <- arima.sim(list(ma = theta), n = n) / sqrt(1 + theta^2)
    bts <- boot::tsboot(x, statistic, l = blksize, sim = "fixed", R = R)
    ## returns a vector with length twice of that of mystat's returned values
    ## each pair forms an interval for one target
    c(apply(bts$t, 2, quantile, prob = c(.025, .975)))
}
    
mychk <- function(sim, target) {
    ret <- rep(NA, length(target))
    for (i in seq_along(target)) {
        ii <- (i - 1) * 2
        ret[i] <- mean(sim[ii + 1,] < target[i] & target[i] < sim[ii + 2,])
    }
    ret
}

#### simulation
nrep <- 1000

## n = 1000
n <- 1000
blksize <- 10
theta <- .3
sim_.3_1k <- replicate(nrep, do1rep(n, theta, mystat, blksize))
mychk(sim_.3_1k, target = c(0, 1, theta / (1 + theta^2)))

n = 8000
n <- 8000
blksize <- 20
theta <- .3
sim_.3_8k <- replicate(nrep, do1rep(n, theta, mystat, blksize))
mychk(sim_.3_8k, target = c(0, 1, theta / (1 + theta^2)))
