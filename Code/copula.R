## Need packages copula and pracma

myd <- dMv
mymvd <- copula::mvdc(normalCopula(0.4), margins = "exp", paramMargins = list(rate=1), marginsIdentical = TRUE)

## numerical integration to get 
EXY <- pracma::dblquad(function(x, y) x * y * copula::dMvdc(cbind(x, y), mymvd),
                       0, 20, 0, 20)
## approximate true rho
rho <- (EXY - 1) / 1

## empirical check
z <- rMvdc(100000, mymvd)
cor(z[,1], z[,2])
