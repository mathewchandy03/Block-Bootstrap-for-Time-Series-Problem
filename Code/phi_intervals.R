source("functions.R")
library(ggplot2)

set.seed(1234)
sim <- replicate(50, do1rep(800, 0.4, mystat, 10))
new_sim <- sim[c(3, 6, 23, 24, 35, 36, 47, 48), ]

file_titles <- c("pct", "bc", "prop")
custom_titles <- c("Percentile", "Bias-Corrected", "Proposed")

# Create a data frame for plotting
plot_data <- data.frame(replication = 1:50,
                        point_estimate = new_sim[1, ],
                        bts_mean = new_sim[2, ],
                        lower_bound = c(new_sim[3, ], new_sim[5, ], new_sim[7, ]),
                        upper_bound = c(new_sim[4, ], new_sim[6, ], new_sim[8, ]),
                        file_title = rep(file_titles, each = 50),
                        custom_title = rep(custom_titles, each = 50))

# Create facet plot using ggplot2
ggplot(plot_data, aes(x = replication, y = point_estimate)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.5, color = 'gray') +
  geom_line(color = 'blue') +
  geom_line(aes(y = bts_mean), color = 'red') +
  geom_hline(yintercept = 0.4, linetype = "dotted") +
  facet_wrap(~ custom_title, ncol = 1) +
  labs(x = 'Replication', y = 'Value') +
  scale_color_manual(values = c('blue', 'red', 'gray')) +
  scale_linetype_manual(values = c(1, 1, 1)) +
  theme_minimal()

ggsave('../Manuscript/figures/norm_phi_intervals.pdf')