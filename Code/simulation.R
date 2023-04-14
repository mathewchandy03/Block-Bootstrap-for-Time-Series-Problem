nrep <- 10000

n <- 100
blksize <- 5
phi <- -.4
target <- c(0, 1, phi)
set.seed(1234)
sim_n.4_1h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
cov <- c(mychk(sim_n.4_1h, target))

n <- 200
blksize <- 6
phi <- -.4
target <- c(0, 1, phi)
set.seed(2345)
sim_n.4_2h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.4_2h,"../Data/Raw/sim_n.4_2h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.4_2h, target))

n <- 300
blksize <- 7
phi <- -.4
target <- c(0, 1, phi)
set.seed(3456)
sim_n.4_3h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.4_3h,"../Data/Raw/sim_n.4_3h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.4_3h, target))

n <- 400
blksize <- 7
phi <- -.4
target <- c(0, 1, phi)
set.seed(4567)
sim_n.4_4h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.4_4h,"../Data/Raw/sim_n.4_4h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.4_4h, target))

n <- 500
blksize <- 8
phi <- -.4
target <- c(0, 1, phi)
set.seed(5678)
sim_n.4_5h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.4_5h,"../Data/Raw/sim_n.4_5h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.4_5h, target))

n <- 600
blksize <- 9
phi <- -.4
target <- c(0, 1, phi)
set.seed(6789)
sim_n.4_6h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.4_6h,"../Data/Raw/sim_n.4_6h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.4_6h, target))

n <- 700
blksize <- 9
phi <- -.4
target <- c(0, 1, phi)
set.seed(6789)
sim_n.4_7h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.4_7h,"../Data/Raw/sim_n.4_7h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.4_7h, target))

n <- 100
blksize <- 5
phi <- -.2
target <- c(0, 1, phi)
set.seed(1234)
sim_n.2_1h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.2_1h,"../Data/Raw/sim_n.2_1h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.2_1h, target))

n <- 200
blksize <- 6
phi <- -.2
target <- c(0, 1, phi)
set.seed(2345)
sim_n.2_2h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.2_2h,"../Data/Raw/sim_n.2_2h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.2_2h, target))

n <- 300
blksize <- 7
phi <- -.2
target <- c(0, 1, phi)
set.seed(3456)
sim_n.2_3h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.2_3h,"../Data/Raw/sim_n.2_3h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.2_3h, target))

n <- 400
blksize <- 7
phi <- -.2
target <- c(0, 1, phi)
set.seed(4567)
sim_n.2_4h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.2_4h,"../Data/Raw/sim_n.2_4h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.2_4h, target))

n <- 500
blksize <- 8
phi <- -.2
target <- c(0, 1, phi)
set.seed(5678)
sim_n.2_5h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.2_5h,"../Data/Raw/sim_n.2_5h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.2_5h, target))

n <- 600
blksize <- 9
phi <- -.2
target <- c(0, 1, phi)
set.seed(6789)
sim_n.2_6h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.2_6h,"../Data/Raw/sim_n.2_6h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.2_6h, target))

n <- 700
blksize <- 9
phi <- -.2
target <- c(0, 1, phi)
set.seed(6789)
sim_n.2_7h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_n.2_7h,"../Data/Raw/sim_n.2_7h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_n.2_7h, target))

n <- 100
blksize <- 5
phi <- 0
target <- c(0, 1, phi)
set.seed(1234)
sim_0_1h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_0_1h,"../Data/Raw/sim_0_1h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_0_1h, target))

n <- 200
blksize <- 6
phi <- 0
target <- c(0, 1, phi)
set.seed(2345)
sim_0_2h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_0_2h,"../Data/Raw/sim_0_2h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_0_2h, target))

n <- 300
blksize <- 7
phi <- 0
target <- c(0, 1, phi)
set.seed(3456)
sim_0_3h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_0_3h,"../Data/Raw/sim_0_3h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_0_3h, target))

n <- 400
blksize <- 7
phi <- 0
target <- c(0, 1, phi)
set.seed(4567)
sim_0_4h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_0_4h,"../Data/Raw/sim_0_4h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_0_4h, target))

n <- 500
blksize <- 8
phi <- 0
target <- c(0, 1, phi)
set.seed(5678)
sim_0_5h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_0_5h,"../Data/Raw/sim_0_5h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_0_5h, target))

n <- 600
blksize <- 9
phi <- 0
target <- c(0, 1, phi)
set.seed(6789)
sim_0_6h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_0_6h,"../Data/Raw/sim_0_6h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_0_6h, target))

n <- 700
blksize <- 9
phi <- 0
target <- c(0, 1, phi)
set.seed(6789)
sim_0_7h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_0_7h,"../Data/Raw/sim_0_7h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_0_7h, target))

n <- 100
blksize <- 5
phi <- .2
target <- c(0, 1, phi)
set.seed(1234)
sim_.2_1h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.2_1h,"../Data/Raw/sim_.2_1h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.2_1h, target))

n <- 200
blksize <- 6
phi <- .2
target <- c(0, 1, phi)
set.seed(2345)
sim_.2_2h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.2_2h,"../Data/Raw/sim_.2_2h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.2_2h, target))

n <- 300
blksize <- 7
phi <- .2
target <- c(0, 1, phi)
set.seed(3456)
sim_.2_3h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.2_3h,"../Data/Raw/sim_.2_3h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.2_3h, target))

n <- 400
blksize <- 7
phi <- .2
target <- c(0, 1, phi)
set.seed(4567)
sim_.2_4h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.2_4h,"../Data/Raw/sim_.2_4h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.2_4h, target))

n <- 500
blksize <- 8
phi <- .2
target <- c(0, 1, phi)
set.seed(5678)
sim_.2_5h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.2_5h,"../Data/Raw/sim_.2_5h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.2_5h, target))

n <- 600
blksize <- 9
phi <- .2
target <- c(0, 1, phi)
set.seed(6789)
sim_.2_6h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.2_6h,"../Data/Raw/sim_.2_6h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.2_6h, target))

n <- 700
blksize <- 9
phi <- .2
target <- c(0, 1, phi)
set.seed(6789)
sim_.2_7h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.2_7h,"../Data/Raw/sim_.2_7h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.2_7h, target))

n <- 100
blksize <- 5
phi <- .4
target <- c(0, 1, phi)
set.seed(1234)
sim_.4_1h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.4_1h,"../Data/Raw/sim_.4_1h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.4_1h, target))

n <- 200
blksize <- 6
phi <- .4
target <- c(0, 1, phi)
set.seed(2345)
sim_.4_2h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.4_2h,"../Data/Raw/sim_.4_2h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.4_2h, target))

n <- 300
blksize <- 7
phi <- .4
target <- c(0, 1, phi)
set.seed(3456)
sim_.4_3h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.4_3h,"../Data/Raw/sim_.4_3h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.4_3h, target))

n <- 400
blksize <- 7
phi <- .4
target <- c(0, 1, phi)
set.seed(4567)
sim_.4_4h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.4_4h,"../Data/Raw/sim_.4_4h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.4_4h, target))

n <- 500
blksize <- 8
phi <- .4
target <- c(0, 1, phi)
set.seed(5678)
sim_.4_5h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.4_5h,"../Data/Raw/sim_.4_5h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.4_5h, target))

n <- 600
blksize <- 9
phi <- .4
target <- c(0, 1, phi)
set.seed(6789)
sim_.4_6h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.4_6h,"../Data/Raw/sim_.4_6h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.4_6h, target))

n <- 700
blksize <- 9
phi <- .4
target <- c(0, 1, phi)
set.seed(6789)
sim_.4_7h <- replicate(nrep, do1rep(n, phi, mystat, blksize))
write.csv(sim_.4_7h,"../Data/Raw/sim_.4_7h.csv", row.names = FALSE)
cov <- c(cov, mychk(sim_.4_7h, target))

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