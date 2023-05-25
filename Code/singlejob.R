source("functions.R")
result <- list()
names(result) <- c()
phi <- -.4
n = 100
blksize <- ceiling(n^(1/3))
target <- c(0, 1, phi)
sim <- replicate(100, do1rep(n, phi, mystat, blksize))
result[[paste(phi, n, sep = '_')]] <- sim
saveRDS(result, paste('/Data/result', '_', i, '.rds', sep = ''))

