source("functions.R")
setwd("../Data")
file_names <- list.files(pattern = "\\.rds$")
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
for(i in names(combined_matrices)) {
  phi <- as.numeric(strsplit(i, '_')[[1]][1])
  n <- as.integer(strsplit(i, '_')[[1]][2])
  cov <- mychk(combined_matrices[[i]], c(0, 1, phi))
  types <- c('stdCI', 'stuCI', 'pctCI', 'ctrCI', 'ourctrCI', 'bcaCI')
  CI <- rep(types, each = length(cov) / length(types))
  parameters <- c('mu', 'sigma', 'phi')
  target <- rep(parameters, length(cov) / length(parameters))
  LB <- sapply(cov, function(i) prop.test(i*nrep, nrep)$conf.int[1])
  UB <- sapply(cov, function(i) prop.test(i*nrep, nrep)$conf.int[2])
  new_rows <- data.frame(phi = rep(phi, length(cov)), n = rep(n, length(cov)), CI, target, cov, LB, UB)
  df <- rbind(df, new_rows)
}

t <- 'mu'
df_mu <- df[(df$target == t),]
write.csv(df_mu,"df_mu.csv", row.names = FALSE)

t <- 'sigma'
df_sigma <- df[(df$target == t),]
write.csv(df_sigma,"df_sigma.csv", row.names = FALSE)

t <- 'phi'
df_phi <- df[(df$target == t),]
write.csv(df_phi,"df_phi.csv", row.names = FALSE)