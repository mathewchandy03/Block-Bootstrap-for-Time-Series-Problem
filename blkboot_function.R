n <- 200
x <- stats::arima.sim(list(ar = 0.5), n) + 2

nmblk <- function(n, l) {
  b <- n / l # assuming that is an integer
  idx <- sample(seq(1, n - l + 1, by = l), b, replace = TRUE) # Sample between 1 and n - l + 1, 
  # separating by l, b times
  c(outer(0:(l - 1), idx, "+")) # Creates a vector listing random blocks of size l with starting points 
  # decided by idx
}

nmblk(n, 10)

#idc <- sample(seq(1, 191, by = 10), 20, replace = TRUE)
#print(idc)
#outer(0:9, idc)
#c(outer(0:9, idc, '+'))

mvblk <- function(n, l) {
  b <- n / l
  idx <- sample(1:(n - l + 1), b, replace = TRUE) # Sample any starting point between 1 and n - 1 + 1
  # they can overlap
  c(outer(0:(l - 1), idx, "+")) # Creates a vector listing random blocks of size l with starting points
  # decided by idx
}

mvblk(n, 10)

#idk <- sample(1:191, 20, replace = TRUE)
#print(idk)
#outer(0:9, idk)
#c(outer(0:9, idk, '+'))

blkboot <- function(tseries, statistic, R, l) {
  n <- length(tseries) 
  ## observed statistic
  stat <- statistic(tseries) #stat stores a certain statistic of tseries using a function
  ## nonmoving block bootstrap
  nm.stat <- replicate(R, statistic(tseries[nmblk(n, l)])) # finds the statistic of nm bootstrap and repeats R times
  ## moving block bootstrap
  mv.stat <- replicate(R, statistic(tseries[mvblk(n, l)])) # finds the statistic of mv bootstrap and repeats R times
  list(stat = stat, nm.stat = nm.stat, mv.stat = mv.stat) # returns list of three values
}

## an experiment
ar1 <- function(x) cor(x[-1], x[-length(x)]) # correlation of series without the first entry vs series without the last entry
m1 <- function(x) mean(x)
m2 <- function(x) median(x)

y <- c(0, 1, 3, 5)
y[-1]
y[-length(y)]
cor(y[-1], y[-length(y)])

blkboot(x, ar1, 1000, 10)

do1rep <- function(n, theta, l, statistic) {
  x <- arima.sim(list(ar = theta), n)
  bt <- blkboot(x, statistic, 1000, l) 
  c(bt$stat, sd(bt$nm.stat), sd(bt$mv.stat)) # returns statistics and sd's for nm and mv respectively
}

do1rep(n, 0.5, 10, ar1)

sim <- replicate(200, do1rep(100, 0.5, 10, ar1)) # replicates simulation 200 times
print(sim)