## This note checks the empirical coverage rate of CIs constructed from
## block bootstrap with series generated from an MA process

mystat <- function(x) {
  c(mean(x),
    sd(x),
    cor(x[-1], x[-length(x)]))
  # acf(x, lag.max = 1, plot = FALSE)$acf[2])
}

do1rep <- function(n, theta, statistic, blksize, R = 1000, level = .95) {
  x <- arima.sim(list(ma = theta), n = n) / sqrt(1 + theta^2)
  bts <- boot::tsboot(x, statistic, l = blksize, sim = "fixed", R = R)
  alpha <- 1 - level
  ## returns a vector with length twice of that of mystat's returned values
  ## each pair forms an interval for one target
  ## pctCI <- apply(bts$t, 2, quantile, prob = c(alpha/2, 1 - alpha/2))
  mbts <- apply(bts$t, 2, mean)
  crit <- apply(abs(t(t(bts$t) - mbts)), 2, quantile, prob = level)
  pctCI <- rbind(bts$t0 - crit, bts$t0 + crit)  
  empCI <- matrix(2 * bts$t0, 2, length(bts$t0), byrow = TRUE) - pctCI[c(2, 1),]
  ## BCa interval
  z0 <- qnorm(colMeans(sweep(bts$t, 2, bts$t0) < 0))
  ## thetajack <- sapply(1:n, function(i) statistic(x[-i]))
  thetajack <- sapply(1: (n / blksize),
                      function(i) statistic(x[ - ((i - 1) * blksize + 1:blksize)]))
  a <- apply(thetajack, 1, e1071::skewness) / 6
  alpha1 <- pnorm(z0 + (z0 + qnorm(alpha/2)) / (1 - a * (z0 + qnorm(alpha/2))))
  alpha2 <- pnorm(z0 + (z0 + qnorm(1 - alpha/2)) / (1 - a * (z0 + qnorm(1 - alpha/2))))
  bcaCI <- sapply(1:length(bts$t0),
                  function(i) quantile(bts$t[,i], prob = c(alpha1[i], alpha2[i])))
  ## return all intervals
  c(pctCI, empCI, bcaCI)
}

mychk <- function(sim, target) {
  p <- nrow(sim) / length(target) / 2
  target <- rep(target, p)
  ret <- rep(NA, length(target))
  for (i in seq_along(ret)) {
    ii <- (i - 1) * 2
    ret[i] <- mean(sim[ii + 1,] < target[i] & target[i] < sim[ii + 2,])
  }
  ret
}

#### simulation
nrep <- 100

## n = 1000
n <- 1000
blksize <- 10
theta <- .3
target <- c(0, 1, theta / (1 + theta^2))
sim_.3_1k <- replicate(nrep, do1rep(n, theta, mystat, blksize))
mychk(sim_.3_1k, target)

## n = 8000
n <- 8000
blksize <- 20
theta <- .3
target <- c(0, 1, theta / (1 + theta^2))
sim_.3_8k <- replicate(nrep, do1rep(n, theta, mystat, blksize))
mychk(sim_.3_8k, target)