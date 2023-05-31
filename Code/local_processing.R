source("functions.R")
setwd("../Data")
local_results <- readRDS('local_results.rds')
df <- data.frame(phi = numeric(), n = numeric(), CI = character(), 
                 target = character(), cov = numeric(),
                 LB = numeric(), UB = numeric())
nrep <- 10000
for(i in names(local_results)) {
  phi <- as.numeric(strsplit(i, '_')[[1]][1])
  n <- as.integer(strsplit(i, '_')[[1]][2])
  cov <- mychk(local_results[[i]], c(0, 1, phi))
  types <- c('stdCI', 'stuCI', 'pctCI', 'ctrCI', 'bcCI', 'bcaCI', 'propCI')
  CI <- rep(types, each = length(cov) / length(types))
  parameters <- c('mu', 'sigma', 'phi')
  target <- rep(parameters, length(cov) / length(parameters))
  LB <- sapply(cov, function(i) prop.test(i*nrep, nrep)$conf.int[1])
  UB <- sapply(cov, function(i) prop.test(i*nrep, nrep)$conf.int[2])
  new_rows <- data.frame(phi = rep(phi, length(cov)), n = rep(n, length(cov)), CI, target, cov, LB, UB)
  df <- rbind(df, new_rows)
}

t <- 'mu'
df_mu <- df[(df$target == t),]
write.csv(df_mu,"df_mu.csv", row.names = FALSE)

t <- 'sigma'
df_sigma <- df[(df$target == t),]
write.csv(df_sigma,"df_sigma.csv", row.names = FALSE)

t <- 'phi'
df_phi <- df[(df$target == t),]
write.csv(df_phi,"df_phi.csv", row.names = FALSE)