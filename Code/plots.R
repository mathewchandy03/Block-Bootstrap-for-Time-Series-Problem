library(tidyverse)
source("functions.R")
norm_mu <- read.csv("../Data/norm_mu.csv") %>% 
  filter(CI != 'ctrCI')
norm_sigma <- read.csv("../Data/norm_sigma.csv") %>% 
  filter(CI != 'ctrCI')
norm_phi <- read.csv("../Data/norm_phi.csv") %>% 
  filter(CI != 'ctrCI')

t <- 'norm_mu_1'
width <- .2
graph_bts(t, width, norm_mu %>% filter(blksize == ceiling(n^(1/3))))
alt_graph_bts(t, width, norm_mu %>% filter(blksize == ceiling(n^(1/3))))

t <- 'norm_sigma_1'
width <- .2
graph_bts(t, width, norm_sigma %>% filter(blksize == ceiling(n^(1/3))))

t <- 'norm_phi_1'
width <- .2
graph_bts(t, width, norm_phi %>% filter(blksize == ceiling(n^(1/3))))
alt_graph_bts2(t, width, norm_phi %>% filter(blksize == ceiling(n^(1/3))))

t <- 'norm_mu_2'
width <- .2
graph_bts(t, width, norm_mu %>% filter(blksize == ceiling(2*n^(1/3))))

t <- 'norm_sigma_2'
width <- .2
graph_bts(t, width, norm_sigma %>% filter(blksize == ceiling(2*n^(1/3))))

t <- 'norm_phi_2'
width <- .2
graph_bts(t, width, norm_phi %>% filter(blksize == ceiling(2*n^(1/3))))

exp_mu <- read.csv("../Data/exp_mu.csv") %>% 
  filter(CI != 'ctrCI')
exp_sigma <- read.csv("../Data/exp_sigma.csv") %>% 
  filter(CI != 'ctrCI')
exp_phi <- read.csv("../Data/exp_phi.csv") %>% 
  filter(CI != 'ctrCI')

t <- 'exp_mu_1'
width <- .2
graph_bts(t, width, exp_mu %>% filter(blksize == ceiling(n^(1/3))))

t <- 'exp_sigma_1'
width <- .2
graph_bts(t, width, exp_sigma %>% filter(blksize == ceiling(n^(1/3))))

t <- 'exp_phi_1'
width <- .2
graph_bts(t, width, exp_phi %>% filter(blksize == ceiling(n^(1/3))))

t <- 'exp_mu_2'
width <- .2
graph_bts(t, width, exp_mu %>% filter(blksize == ceiling(2*n^(1/3))))

t <- 'exp_sigma_2'
width <- .2
graph_bts(t, width, exp_sigma %>% filter(blksize == ceiling(2*n^(1/3))))

t <- 'exp_phi_2'
width <- .2
graph_bts(t, width, exp_phi %>% filter(blksize == ceiling(2*n^(1/3))))
