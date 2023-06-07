source("functions.R")
norm_mu <- read.csv("../Data/norm_mu.csv")
norm_sigma <- read.csv("../Data/norm_sigma.csv")
norm_phi <- read.csv("../Data/norm_phi.csv")

t <- 'norm_mu'
width <- .2
graph_bts(t, width, norm_mu)

t <- 'norm_sigma'
width <- .2
graph_bts(t, width, norm_sigma)

t <- 'norm_phi'
width <- .2
graph_bts(t, width, norm_phi)

source("functions.R")
exp_mu <- read.csv("../Data/exp_mu.csv")
exp_sigma <- read.csv("../Data/exp_sigma.csv")
exp_rho <- read.csv("../Data/exp_rho.csv")

t <- 'exp_mu'
width <- .2
graph_bts(t, width, exp_mu)

t <- 'exp_sigma'
width <- .2
graph_bts(t, width, exp_sigma)

t <- 'exp_rho'
width <- .2
graph_bts(t, width, exp_rho)
