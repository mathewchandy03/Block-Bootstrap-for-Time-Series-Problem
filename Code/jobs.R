source("functions.R")
i <- Sys.getenv("SLURM_ARRAY_TASK_ID")
results <- list()
names(results) <- c()
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600, 3200)) {
    blksize <- ceiling(n^(1/3))
    target <- c(0, 1, phi)
    set.seed(as.integer(i))
    sim <- replicate(100, do1rep(n, phi, mystat, blksize))
    results[[paste(phi, n, sep = '_')]] <- sim
  }
}
saveRDS(results, paste('../Data/results', '_', i, '.rds', sep = ''))

