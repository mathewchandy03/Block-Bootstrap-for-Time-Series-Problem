source("functions.R")
norm_results <- list()
names(norm_results) <- c()
exp_results <- list()
names(exp_results) <- c()
phi <- -.4
n = 100
blksize <- ceiling(n^(1/3))
set.seed(as.integer(1))
sim <- replicate(100, do1rep(n, phi, mystat, blksize))
norm_results[[paste(phi, n, sep = '_')]] <- sim
set.seed(as.integer(1))
sim <- replicate(100, do1rep(n, phi, mystat, blksize, qexp))
exp_results[[paste(phi, n, sep = '_')]] <- sim

saveRDS(norm_results, '../Data/norm_result.rds')
saveRDS(exp_results, '../Data/exp_result.rds')