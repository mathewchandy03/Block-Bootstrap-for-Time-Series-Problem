.libPaths("~/rlibs")
library(ggplot2) 
library(showtext)

mystat <- function(x) {
  c(mean(x),
    sd(x),
    cor(x[-1], x[-length(x)]))
    # acf(x, lag.max = 1, plot = FALSE)$acf[2])
}

expstat <- function(x) {
  c(1 / mean(x),
    cor(x[-1], x[-length(x)]))
    # acf(x, lag.max = 1, plot = FALSE)$acf[2])
}

calculate_a_hat <- function(row) {
  tj_mean <- mean(row)
  numerator <- sum((tj_mean - row)^3)
  denominator <- 6 * sum((tj_mean - row)^2)^(3/2)
  c(numerator / denominator)
}

do1rep <- function(n, phi, statistic, blksize, dstr = qnorm, R = 1000, level = .95) {
  if (phi == 0) {
    x <- rnorm(n)
  }
  else {
    x <- arima.sim(list(ar = phi), n = n) * sqrt(1 - phi^2)
  }
  x <- dstr(pnorm(x))
  bts <- boot::tsboot(x, statistic, l = blksize, sim = "fixed", R = R)
  alpha <- 1 - level
  
  ## Standard interval
  se <- apply(bts$t, 2, sd)
  stdCI <- sapply(1:length(bts$t0),
                  function(i) c(bts$t0[i] - qnorm(1-alpha/2)*se[i], bts$t0[i] - 
                                  qnorm(alpha/2)*se[i]))
  
  ## Student's t interval
  stuCI <- sapply(1:length(bts$t0),
                  function(i) c(bts$t0[i] - qt(1-alpha/2, n/blksize - 1)*se[i], 
                                bts$t0[i] - qt(alpha/2, n/blksize - 1)*se[i]))
  
  ## Percentile interval
  pctCI <- apply(bts$t, 2, quantile, prob = c(alpha/2, 1 - alpha/2))
  
  ## Centered Bootstrap Percentile interval (from singh2008bootstrap)
  crit <- 
    apply(t(t(bts$t) - bts$t0), 2, quantile, prob = c(1 - alpha/2, alpha/2)) 
  mbts <- apply(bts$t, 2, mean) # mean of bootstrap point estimates
  ctrCI <- -sweep(crit, 2, bts$t0, FUN = '-')
  
  ## BC interval (Carpenter et al 2000, p. 1153)
  b <- qnorm(colMeans(sweep(bts$t, 2, bts$t0) < 0))
  qup <- pnorm(2 * b - qnorm(alpha / 2))
  qlo <- pnorm(2 * b - qnorm(1 - alpha / 2))
  bcCI <- sapply(1:length(bts$t0),
                 function(i) quantile(bts$t[,i], prob=c(qlo[i], qup[i])))
  
  ## BCA interval (Diciccio and Efron (1996): p.195)
  z0 <- qnorm(colMeans(sweep(bts$t, 2, bts$t0) < 0)) # same as b in Carpenter 2000
  thetajack <- sapply(1: (n / blksize),
                      function(i)
                        statistic(x[ - ((i - 1) * blksize + 1:blksize)]))
  a_hat <- apply(thetajack, 1, calculate_a_hat)
  alpha1 <- pnorm(
    z0 + (z0 + qnorm(alpha/2)) / (1 - a_hat * (z0 + qnorm(alpha/2))))
  alpha2 <- pnorm(
    z0 + (z0 + qnorm(1 - alpha/2)) / (1 - a_hat * (z0 + qnorm(1 - alpha/2))))
  bcaCI <- sapply(1:length(bts$t0),
                  function(i) quantile(bts$t[,i], prob = c(alpha1[i], alpha2[i])))
  
  ## Proposed interval (Recentered Percentile)
  crit2 <- 
    apply(t(t(bts$t) - mbts), 2, quantile, prob = c(alpha/2, 1 - alpha/2)) 
  propCI <- sweep(crit2, 2, bts$t0, FUN = '+') # original point estimate
  
  ## return all intervals
  c(bts$t0, mbts, stdCI, stuCI, pctCI, ctrCI, bcCI, bcaCI, propCI)
}

mychk <- function(sim, target) {
  start <- length(target) * 2 + 1
  new_sim <- sim[start:nrow(sim), ]
  p <- nrow(new_sim) / length(target) / 2
  target <- rep(target, p)
  ret <- rep(NA, length(target))
  for (i in seq_along(ret)) {
    ii <- (i - 1) * 2
    ret[i] <- mean(new_sim[ii + 1,] < target[i] & target[i] < new_sim[ii + 2,])
  }
  ret
}

graph_bts <- function(t, width, data, trans = 'identity', level = .95)
{
  font_add_google("EB Garamond")
#  windows()
  
  showtext_auto()
  
  data$CI <- factor(data$CI, levels = c('stdCI', 'stuCI', 'pctCI', 'bcCI', 'bcaCI', 'propCI'), 
                    labels = c('Standard', 'Student\'s t', 'Percentile', 'BC', 'BCA', 'Recentered'))
  
  ggplot(data, aes(x = n, y = cov)) +
    geom_hline(yintercept = level, linetype = 'dashed', color = 'orange') + 
    geom_line(size = .2) +
    geom_errorbar(aes(ymin=LB, ymax = UB), colour="black", width = width, size = .2) +
    facet_grid(factor(CI) ~ factor(phi), scales = 'free') +
    labs(x = 'Sample Size', y = 'Coverage Rate') +
    scale_x_continuous(breaks = c(100, 200, 400, 800, 1600, 3200), trans='log2') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), text =
            element_text(family = "EB Garamond", size = 10),
          strip.text.y = element_text(angle = 270, hjust = 1)) +
    scale_y_continuous(trans=trans)
  ggsave(paste('../Manuscript/figures/plot_', t, '.pdf', sep = ''), width = 10,
         height = 5)
}
