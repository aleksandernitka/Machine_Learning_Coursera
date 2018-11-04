dat = as.matrix(read.csv('ex1data1.txt', header = FALSE))
X = as.matrix(dat[,1], ncol = 1)
y = as.matrix(dat[,2], ncol = 1)

plot(X,y)

# Add column of ones to X
X = cbind(1,X)

# init theta parameters for x0, x1
theta = matrix(c(1,1), ncol = 1)

# iterations
iter = 1500

# learning rate
alpha = 0.01

# size
m = length(y)

# how J is progressing through the interations
trackJ = as.matrix(rep(0, iter), ncol = 1)
trackTheta1 = as.matrix(rep(0, iter), ncol = 1)
trackTheta2 = as.matrix(rep(0, iter), ncol = 1)

source('costFunctionJ.r')

for (i in 1 : iter){
    
    # Calculate and save J
    # costFunctionJ returns a list of J and SqError
    costFunOut = costFunctionJ(X, y, theta)
    
    # Save J
    trackJ[i] = costFunOut$J
    
    # Theta Change using vectorised method
    # thetaChange = alpha * (1/m) * (t(X) %*% error)
    
    trackTheta1[i] = theta[1,1]
    trackTheta2[i] = theta[2,1]
    
    thetaChange = alpha * (1/m) * (t(X) %*% costFunOut$Error)
    
    # Update thera
    theta = theta - thetaChange
    
}

par(mfrow=c(3,1))
plot(trackJ, type = 'l', main = 'J')
plot(trackTheta1, type = 'l', main = 'theta1')
plot(trackTheta2, type = 'l', main = 'theta2')
