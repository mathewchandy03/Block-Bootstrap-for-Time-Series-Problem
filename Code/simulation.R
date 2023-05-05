source("functions.R")
nrep <- 10000
cov <- c()
for (phi in c(-.4, -.2, 0, .2, .4)) {
  for (n in c(100, 200, 400, 800, 1600)) {
    blksize <- ceiling(n^(1/3))
    target <- c(0, 1, phi)
    set.seed(10*(phi+n))
    sim <- replicate(nrep, do1rep(n, phi, mystat, blksize))
    write.csv(sim, paste('../Data/Raw/sim', '_', as.character(phi), '_', as.character(n), '.csv', sep = ''))
    cov <- c(cov, mychk(sim, target))
  }
}

df <- data.frame(df_bts(cov))

t <- 'mu'
df_mu <- df[(df$t == t),]
write.csv(df_mu,"../Data/df_mu.csv", row.names = FALSE)

t <- 'sigma'
df_sigma <- df[(df$t == t),]
write.csv(df_sigma,"../Data/df_sigma.csv", row.names = FALSE)

t <- 'phi'
df_phi <- df[(df$t == t),]
write.csv(df_phi,"../Data/df_phi.csv", row.names = FALSE)