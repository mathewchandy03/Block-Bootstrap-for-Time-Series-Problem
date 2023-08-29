source("functions.R")
rho <- readRDS('../Data/rho')
i <- Sys.getenv("SLURM_ARRAY_TASK_ID")
norm_results <- list()
names(norm_results) <- c()
exp_results <- list()
names(exp_results) <- c()
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    blksize <- ceiling(n^(1/3))
    set.seed(as.integer(i))
    sim <- replicate(100, do1rep(n, phi, mystat, blksize))
    norm_results[[paste(phi, n, sep = '_')]] <- sim
    set.seed(as.integer(i))
    sim <- replicate(100, do1rep(n, rho[paste(phi)], mystat, blksize, qexp))
    exp_results[[paste(phi, n, sep = '_')]] <- sim
  }
}

saveRDS(norm_results, paste('../Data/norm_results', '_', i, '.rds', sep = ''))
saveRDS(exp_results, paste('../Data/exp_results', '_', i, '.rds', sep = ''))
