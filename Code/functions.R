## This note checks the empirical coverage rate of CIs constructed from
## block bootstrap with series generated from an AR process

mystat <- function(x) {
  c(mean(x),
    sd(x),
    cor(x[-1], x[-length(x)]))
  # acf(x, lag.max = 1, plot = FALSE)$acf[2])
}

do1rep <- function(n, phi, statistic, blksize, R = 1000, level = .95) {
  if (phi == 0) {
    x <- rnorm(n)
  }
  else {
    x <- arima.sim(list(ar = phi), n = n) * sqrt(1 - phi^2)
  }
  bts <- boot::tsboot(x, statistic, l = blksize, sim = "fixed", R = R)
  alpha <- 1 - level
  
  
  ## Percentile interval
  pctCI <- apply(bts$t, 2, quantile, prob = c(alpha/2, 1 - alpha/2))
  
  ## Centered interval
  mbts <- apply(bts$t, 2, mean)
  crit <- 
    apply(t(t(bts$t) - mbts), 2, quantile, prob = c(alpha/2, 1 - alpha/2)) 
  ## alpha/2 and 1 - alpha/2 critical values 
  ## of pseudo-estimate - mean of pseudo-estimates
  # crit2 <- 
  #  apply(t(t(bts$t) - bts$t0), 2, quantile, prob = c(alpha/2, 1 - alpha/2)) 
  ## alpha/2 and 1 - alpha/2 critical values 
  ## of pseudo-estimate - original estimate
  
  ## interval centered around...
  estCI <- sweep(crit, 2, bts$t0, FUN = '+') # original estimate
  ## LB: bts$t0 + alpha/2 crit, UB: bts$t0 + (1 - alpha/2) crit
  # mbtCI <- sweep(crit2, 2, mbts, FUN = '+') # mean of pseudo-estimates
  ## LB: mbts + alpha/2 crit2, UB: mbts + (1 - alpha/2) crit2
  # altCI <- sweep(-crit[order(2:1),], 2, bts$t0, FUN = '+') # both
  ## LB: bts$t0 - (1 - alpha/2) crit, UB: bts$t0 - alpha/2 crit and
  ## LB: mbts - (1 - alpha/2) crit2, UB: mbts - alpha/2 crit2
  
  
  ## BCa interval
  z0 <- qnorm(colMeans(sweep(bts$t, 2, bts$t0) < 0))
  thetajack <- sapply(1: (n / blksize), 
                      function(i) 
                        statistic(x[ - ((i - 1) * blksize + 1:blksize)]))
  a <- apply(thetajack, 1, e1071::skewness) / 6
  alpha1 <- pnorm(
    z0 + (z0 + qnorm(alpha/2)) / (1 - a * (z0 + qnorm(alpha/2))))
  alpha2 <- pnorm(
    z0 + (z0 + qnorm(1 - alpha/2)) / (1 - a * (z0 + qnorm(1 - alpha/2))))
  crit3 <- sapply(1:length(bts$t0), function(i) 
    quantile(t(t(bts$t[,i]) - mbts[i]), prob = c(alpha1[i], alpha2[i]))) 
  ## alpha1 and alpha2 critical values 
  ## of pseudo-estimate - mean of pseudo-estimates
  # crit4 <- sapply(1:length(bts$t0), function(i) 
  #  quantile(t(t(bts$t[,i]) - bts$t0[i]), prob = c(alpha1[i], alpha2[i])))
  ## alpha1 and alpha2 critical values 
  ## of pseudo-estimate - original estimate
  
  ## interval centered around...
  bcaCIest <- sweep(crit3, 2, bts$t0, FUN = '+') # original estimate
  ## LB: bts$t0 + alpha1 crit3, UB: bts$t0 + alpha2 crit3
  # bcaCImbt <- sweep(crit4, 2, mbts, FUN = '+') # mean of pseudo-estimates
  ## LB: mbts + alpha1 crit4, UB: mbts + alpha2 crit4
  # bcaCIalt <- sweep(-crit3[order(2:1),], 2, bts$t0, FUN = '+') # both
  ## LB: bts$t0 - alpha2 crit3, UB: bts$t0 - alpha1 crit3 and
  ## LB: mbts - alpha2 crit4, UB: mbts - alpha1 crit4
  
  
  ## return all intervals
  c(pctCI, estCI, bcaCIest)
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

df_bts <- function(cov, nrep = 1000, phis = c(-0.4, -0.2, 0, 0.2, 0.4), 
                   sizes = c(100, 200, 300, 400, 500, 600, 700), 
                   types = c('pctCI', 'estCI', 'bcaCIest'), 
                   parameters = c('mu', 'sigma', 'phi')) {
  phi <- 
    rep(phis, each = length(sizes) * length(types) * length(parameters))
  n <- 
    rep(sizes, times = length(phis), each = length(types) * length(parameters))
  CI <- 
    rep(types, times = length(phis) * length(sizes), each = length(parameters))
  t <- rep(parameters, times = length(phis) * length(types) * length(sizes))
  LB <- sapply(cov, function(i) prop.test(i*nrep, nrep)$conf.int[1])
  UB <- sapply(cov, function(i) prop.test(i*nrep, nrep)$conf.int[2])
  
  c(data.frame(phi, n, CI, t, cov, LB, UB))
}

graph_bts <- function(t, width, data, trans = 'identity', level = .95)
{
  library(latex2exp)
  library(ggplot2)
  
  data$CI <- factor(data$CI, levels = c("pctCI", "estCI", "bcaCIest"), 
                    labels = c("Percentile", "Centered", "BCA"))
  
  ggplot(data, aes(x = n, y = cov)) +
    geom_hline(yintercept = level, linetype = 'dashed', color = 'orange') + 
    geom_line() +
    geom_errorbar(aes(ymin=LB, ymax = UB), colour="black", width = width) +
    facet_grid(factor(phi) ~ CI) +
    scale_y_continuous(trans=trans)
  ggsave(paste('../Manuscript/figures/plot_', t, '.pdf', sep = ''))
}
