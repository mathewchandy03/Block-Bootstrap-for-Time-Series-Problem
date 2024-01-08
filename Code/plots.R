library(tidyverse)
source("functions.R")
norm_mu <- read.csv("../Data/norm_mu.csv") %>% 
  filter(CI != 'ctrCI')
norm_sigma <- read.csv("../Data/norm_sigma.csv") %>% 
  filter(CI != 'ctrCI')
norm_phi <- read.csv("../Data/norm_phi.csv") %>% 
  filter(CI != 'ctrCI')

t <- 'norm1'
width <- .2
alt_graph_bts(t, 
              width, 
              rbind(
              norm_mu %>% filter(blksize == ceiling(n^(1/3))),
              norm_sigma %>% filter(blksize == ceiling(n^(1/3))),
              norm_phi %>% filter(blksize == ceiling(n^(1/3)))))

t <- 'norm1'
width <- .2
alt_graph_bts2(t, 
              width, 
              rbind(
                norm_mu %>% filter(blksize == ceiling(n^(1/3))),
                norm_sigma %>% filter(blksize == ceiling(n^(1/3))),
                norm_phi %>% filter(blksize == ceiling(n^(1/3)))))

t <- 'norm2'
width <- .2
alt_graph_bts(t, 
              width, 
              rbind(
                norm_mu %>% filter(blksize == ceiling(2*n^(1/3))),
                norm_sigma %>% filter(blksize == ceiling(2*n^(1/3))),
                norm_phi %>% filter(blksize == ceiling(2*n^(1/3)))))

exp_mu <- read.csv("../Data/exp_mu.csv") %>% 
  filter(CI != 'ctrCI')
exp_sigma <- read.csv("../Data/exp_sigma.csv") %>% 
  filter(CI != 'ctrCI')
exp_phi <- read.csv("../Data/exp_phi.csv") %>% 
  filter(CI != 'ctrCI')

t <- 'exp1'
width <- .2
alt_graph_bts(t, 
              width, 
              rbind(
                exp_mu %>% filter(blksize == ceiling(n^(1/3))),
                exp_sigma %>% filter(blksize == ceiling(n^(1/3))),
                exp_phi %>% filter(blksize == ceiling(n^(1/3)))))

t <- 'exp2'
width <- .2
alt_graph_bts(t, 
              width, 
              rbind(
                exp_mu %>% filter(blksize == ceiling(2*n^(1/3))),
                exp_sigma %>% filter(blksize == ceiling(2*n^(1/3))),
                exp_phi %>% filter(blksize == ceiling(2*n^(1/3)))))

# Exact Clopper-Pearson CI

t <- 'norm_mu_1_cp'
width <- .2
data <- norm_mu %>% filter(blksize == ceiling(n^(1/3)))
trans = 'identity'
level = .95

font_add_google("EB Garamond")
#  windows()

showtext_auto()

data$CI <- factor(data$CI, levels = c('stdCI', 'stuCI', 'pctCI', 'bcCI', 'bcaCI', 'propCI'), 
                  labels = c('Standard', 'Student\'s t', 'Percentile', 'BC', 'BCA', 'Recentered'))

ggplot(data, aes(x = n, y = cov)) +
  geom_hline(yintercept = level, linetype = 'dashed', color = 'orange') + 
  geom_line(size = .2) +
  geom_errorbar(aes(ymin=CP_LB, ymax = CP_UB), colour="black", width = width, size = .2) +
  facet_grid(factor(CI) ~ factor(phi), scales = 'free') +
  labs(x = 'Sample Size', y = 'Coverage Rate') +
  scale_x_continuous(breaks = c(100, 200, 400, 800, 1600, 3200), trans='log2') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text =
          element_text(family = "EB Garamond", size = 10),
        strip.text.y = element_text(angle = 270, hjust = 1)) +
  scale_y_continuous(trans=trans)
ggsave(paste('../Manuscript/figures/plot_', t, '.pdf', sep = ''), width = 10,
       height = 5.8)


t <- 'exp_phi_1_cp'
width <- .2
data <- exp_phi %>% filter(blksize == ceiling(n^(1/3)))
trans = 'identity'
level = .95

font_add_google("EB Garamond")
#  windows()

showtext_auto()

data$CI <- factor(data$CI, levels = c('stdCI', 'stuCI', 'pctCI', 'bcCI', 'bcaCI', 'propCI'), 
                  labels = c('Standard', 'Student\'s t', 'Percentile', 'BC', 'BCA', 'Recentered'))

ggplot(data, aes(x = n, y = cov)) +
  geom_hline(yintercept = level, linetype = 'dashed', color = 'orange') + 
  geom_line(size = .2) +
  geom_errorbar(aes(ymin=CP_LB, ymax = CP_UB), colour="black", width = width, size = .2) +
  facet_grid(factor(CI) ~ factor(phi), scales = 'free') +
  labs(x = 'Sample Size', y = 'Coverage Rate') +
  scale_x_continuous(breaks = c(100, 200, 400, 800, 1600, 3200), trans='log2') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text =
          element_text(family = "EB Garamond", size = 10),
        strip.text.y = element_text(angle = 270, hjust = 1)) +
  scale_y_continuous(trans=trans)
ggsave(paste('../Manuscript/figures/plot_', t, '.pdf', sep = ''), width = 10,
       height = 5.8)