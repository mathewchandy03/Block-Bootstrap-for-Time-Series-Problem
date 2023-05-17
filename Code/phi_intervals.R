results_1 <- readRDS('../Data/results_1.rds')
mtrx <- c()
for(i in names(results_1)) {
  toadd <- results_1[[i]][c(5,6,17,18),]
  toadd <- cbind(rep(i, nrow(toadd)), rep(c('PCT', 'BCA'), each = 2, length.out = nrow(toadd)), toadd)
  mtrx <- rbind(mtrx, toadd)
}
# Assuming your matrix is named 'data_matrix'
# 'data_matrix' is a 120x100 matrix with title in the first column

# Set the number of plots and intervals
num_plots <- nrow(mtrx) / 2
num_intervals <- ncol(mtrx) - 2

# Iterate over each plot
for (i in 1:num_plots) {
  # Calculate the row indices for the current plot
  pdf(paste('../Manuscript/figures/', plot_title, '.pdf'))
  start_row <- (i - 1) * 2 + 1
  end_row <- start_row + 1
  
  # Extract the data for the current plot
  interval_data <- mtrx[start_row:end_row, 3:ncol(mtrx)]
  interval_data <- apply(interval_data, c(1, 2), as.numeric)
  plot_title <- paste(mtrx[start_row, 1], mtrx[start_row, 2])
  
  # Create a new plot
  plot(x = 1:num_intervals,
       y = NULL,
       xlim = c(1, num_intervals),
       ylim = range(-.75, .75),
       type = 'n',
       xlab = 'Interval',
       ylab = 'Confidence Interval',
       main = plot_title,
       yaxt = 'n')
  
  # Add lower and upper bounds as line segments
  for (j in 1:num_intervals) {
    lower_bound <- interval_data[1, j]
    upper_bound <- interval_data[2, j]
    
    lines(c(j, j), c(lower_bound, upper_bound), type = 'l', col = 'blue')
  }
  
  axis(2, at = c(-0.4, -0.2, 0, 0.2, 0.4))
  dev.off()
}
