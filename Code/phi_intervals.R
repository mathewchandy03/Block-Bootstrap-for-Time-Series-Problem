source("functions.R")
set.seed(1234)
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
  lines(x_values, lower_bound, col = "gray")
  lines(x_values, upper_bound, col = "gray")
  abline(h = 0.4, lty = "dotted")
  legend("topright", legend = c("Point Estimate", "Bootstrap Mean", "Interval Bounds"),
         col = c('blue', 'red', 'gray'), lty = c(1, 1, 1), bty = "n")
  dev.off()
}