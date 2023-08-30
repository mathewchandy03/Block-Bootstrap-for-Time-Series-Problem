library(tidyverse)
source("functions.R")
norm_mu <- read.csv("../Data/norm_mu.csv") %>% 
  filter(CI != 'ctrCI')
norm_sigma <- read.csv("../Data/norm_sigma.csv") %>% 
  filter(CI != 'ctrCI')
norm_phi <- read.csv("../Data/norm_phi.csv") %>% 
  filter(CI != 'ctrCI')

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
exp_mu <- read.csv("../Data/exp_mu.csv") %>% 
  filter(CI != 'ctrCI')
exp_sigma <- read.csv("../Data/exp_sigma.csv") %>% 
  filter(CI != 'ctrCI')
exp_phi <- read.csv("../Data/exp_phi.csv") %>% 
  filter(CI != 'ctrCI')

t <- 'exp_mu'
width <- .2
graph_bts(t, width, exp_mu)

t <- 'exp_sigma'
width <- .2
graph_bts(t, width, exp_sigma)

t <- 'exp_phi'
width <- .2
graph_bts(t, width, exp_phi)
