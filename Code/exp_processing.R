source("functions.R")
setwd("../Data")
file_names <- list.files(pattern = "^exp_results")
all_lists <- lapply(file_names, function(x) {
  loaded_list <- readRDS(x)
  return(loaded_list)
})
list_of_matrices <- do.call(c, all_lists)
grouped_matrices <- split(list_of_matrices, names(list_of_matrices))
combined_matrices <- lapply(grouped_matrices, function(x) {
  Reduce(cbind, x)
})
df <- data.frame(phi = numeric(), n = numeric(), CI = character(), 
                 target = character(), cov = numeric(),
                 LB = numeric(), UB = numeric())
nrep = 10000
for(i in names(combined_matrices)) {
  phi <- as.numeric(strsplit(i, '_')[[1]][1])
  n <- as.integer(strsplit(i, '_')[[1]][2])
  cov <- mychk(combined_matrices[[i]], c(1, 1, phi))
  types <- c('stdCI', 'stuCI', 'pctCI', 'ctrCI', 'bcCI', 'bcaCI', 'propCI')
  CI <- rep(types, each = length(cov) / length(types))
  parameters <- c('mu', 'sigma', 'phi')
  target <- rep(parameters, length(cov) / length(parameters))
  LB <- sapply(cov, function(j) prop.test(j*nrep, nrep)$conf.int[1])
  UB <- sapply(cov, function(j) prop.test(j*nrep, nrep)$conf.int[2])
  new_rows <- data.frame(phi = rep(phi, length(cov)), n = rep(n, length(cov)), CI, target, cov, LB, UB)
  df <- rbind(df, new_rows)
}

t <- 'mu'
exp_mu <- df[(df$target == t),]
write.csv(exp_mu,"exp_mu.csv", row.names = FALSE)

t <- 'sigma'
exp_sigma <- df[(df$target == t),]
write.csv(exp_sigma,"exp_sigma.csv", row.names = FALSE)

t <- 'phi'
exp_phi <- df[(df$target == t),]
write.csv(exp_phi,"exp_phi.csv", row.names = FALSE)

setwd("../Code")
