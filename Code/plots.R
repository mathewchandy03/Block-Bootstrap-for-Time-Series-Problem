df_mu <- read.csv("../Data/df_mu.csv")
df_sigma <- read.csv("../Data/df_sigma.csv")
df_phi <- read.csv("../Data/df_phi.csv")

t <- 'mu'
width <- .1
graph_bts(t, width, df_mu)

t <- 'sigma'
width <- .1
graph_bts(t, width, df_sigma)

t <- 'phi'
width <- .1
graph_bts(t, width, df_phi)
