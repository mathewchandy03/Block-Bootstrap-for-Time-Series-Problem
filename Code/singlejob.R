source("functions.R")
norm_results <- list()
names(norm_results) <- c()
exp_results <- list()
names(exp_results) <- c()
phi <- -.4
n = 3200
blksize <- ceiling(n^(1/3))
set.seed(13)
sim <- replicate(100, do1rep(n, phi, mystat, blksize))
norm_results[[paste(phi, n, sep = '_')]] <- sim
set.seed(13)
rho <- 0
if (phi >= 0) {
  rho <- uniroot(function(x) rho2phi(x) - phi, interval = c(0, 0.9))$root
} else {
  rho <- uniroot(function(x) rho2phi(x) - phi, interval = c(-.56, 0))$root
}
sim <- replicate(100, do1rep(n, rho, mystat, blksize, qexp))
exp_results[[paste(phi, n, sep = '_')]] <- sim

saveRDS(norm_results, '../Data/norm_result.rds')
saveRDS(exp_results, '../Data/exp_result.rds')