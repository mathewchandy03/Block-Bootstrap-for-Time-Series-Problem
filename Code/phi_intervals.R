source("functions.R")
sim <- replicate(100, do1rep(800, 0.4, mystat, 10))
new_sim <- sim[c(3, 6, 23, 24, 35, 36, 41, 42), ]

file_titles <- c("pct", "prop", "bc")

for (i in 1:3) {
  pdf(paste('../Manuscript/figures/', file_titles[i], '.pdf', sep = ''))
  lower_bound <- new_sim[(2 * i + 1), ]
  upper_bound <- new_sim[(2 * i + 2), ]
  point_estimate <- new_sim[1, ]
  bts_mean <- new_sim[2, ]
  
  x_values <- 1:100
  
  plot(x_values, point_estimate, type = 'l', ylim = range(lower_bound, upper_bound),
       xlab = 'Replication', ylab = 'Value', main = '', col = 'blue')
  
  lines(x_values, bts_mean, col = 'red')
  # Add confidence intervals
  for (j in 1:100) {
    segments(x_values[j], lower_bound[j], x_values[j], upper_bound[j])
  }
  dev.off()
}