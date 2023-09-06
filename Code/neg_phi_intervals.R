source("functions.R")
library(ggplot2)
library(showtext)

set.seed(1234)
sim <- replicate(50, do1rep(800, -0.4, mystat, 10))
new_sim <- sim[c(3, 6, 23, 24, 35, 36, 47, 48), ]

file_titles <- c("pct", "bc", "prop")
custom_titles <- c("Percentile", "BC", "Recentered")

plot_data_800 <- data.frame(replication = 1:50,
                            point_estimate = new_sim[1, ],
                            bts_mean = new_sim[2, ],
                            lower_bound = c(new_sim[3, ], new_sim[5, ], new_sim[7, ]),
                            upper_bound = c(new_sim[4, ], new_sim[6, ], new_sim[8, ]),
                            file_title = rep(file_titles, each = 50),
                            custom_title = rep(custom_titles, each = 50),
                            sample.size = rep("n = 800", 150))

set.seed(1234)
sim <- replicate(50, do1rep(1600, -0.4, mystat, 10))
new_sim <- sim[c(3, 6, 23, 24, 35, 36, 47, 48), ]

plot_data_1600 <- data.frame(replication = 1:50,
                             point_estimate = new_sim[1, ],
                             bts_mean = new_sim[2, ],
                             lower_bound = c(new_sim[3, ], new_sim[5, ], new_sim[7, ]),
                             upper_bound = c(new_sim[4, ], new_sim[6, ], new_sim[8, ]),
                             file_title = rep(file_titles, each = 50),
                             custom_title = rep(custom_titles, each = 50),
                             sample.size = rep("n = 1600", 150))

plot_data <- rbind(plot_data_800, plot_data_1600)

plot_data$custom_title <- factor(plot_data$custom_title, 
                                 levels = c("Percentile", "BC", "Recentered"))

plot_data$sample.size <- factor(plot_data$sample.size,
                                levels = c("n = 800", "n = 1600"))

font_add_google("EB Garamond")
#  windows()

showtext_auto()

ggplot(plot_data, aes(x = replication, y = point_estimate)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.5, color = 'gray') +
  geom_point(color = 'blue') +
  geom_point(aes(y = bts_mean), color = 'red', shape = 'cross') +
  geom_hline(yintercept = -0.4, linetype = "dotted") +
  facet_grid(custom_title ~ sample.size) +
  labs(x = 'Replication', y = 'Value') +
  scale_color_manual(values = c('blue', 'red', 'gray')) +
  scale_linetype_manual(values = c(1, 1, 1)) +
  theme_minimal() + 
  theme(strip.text=element_text(size=10), text =
          element_text(family = "EB Garamond"))

ggsave('../Manuscript/figures/neg_phi_intervals.pdf', width = 10,
       height = 5.8)
