## Solve for theta
library('MASS')
dat = as.matrix(read.csv('ex1data2.txt', header = FALSE))
X = as.matrix(dat[,1:2], ncol = 1)
y = as.matrix(dat[,3], ncol = 1)
m = length(y)

source('normalisation_fx.R')
source('costFunctionJ.r')

# Normalise the data X:
X = normaliser1000(X)

# Add intercept to X:
X = cbind(rep(1, m), X)

thetaV = ginv(t(X) %*% X) %*% t(X) %*% y
thetaV