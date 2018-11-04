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

# GRADIENT DESCENT

# set parameters
alpha = 0.01
iterations = 400
theta = matrix(rep(0,3), ncol = 1)

# Track J and thetas through the iterations
trackJ = as.matrix(rep(0, iterations), ncol = 1)
trackTheta1 = as.matrix(rep(0, iterations), ncol = 1)
trackTheta2 = as.matrix(rep(0, iterations), ncol = 1)
trackTheta3 = as.matrix(rep(0, iterations), ncol = 1)

for (i in 1:iterations){
    
    costFunOut = costFunctionJ(X,y,theta)
    
    trackJ[i] = costFunOut$J
    trackTheta1[i] = theta[1,1]
    trackTheta2[i] = theta[2,1]
    trackTheta3[i] = theta[3,1]
    
    thetaChange = alpha * (1/m) * (t(X) %*% costFunOut$Error)
    
    # Update thera
    theta = theta - thetaChange
    
}


par(mfrow=c(4,1))
plot(trackJ, type = 'l', main = 'J')
plot(trackTheta1, type = 'l', main = 'theta1')
plot(trackTheta2, type = 'l', main = 'theta2')
plot(trackTheta3, type = 'l', main = 'theta3')

# Select first house
data = X[1,]

# Predictor function:
predictPrice = function(data, theta){
    if (length(data) != length(theta)){
        message('ERROR: Parameters of theta and data are of different lengths')
    }
    
    else{
        price = 0
        for (i in 1:length(theta)){
            price = price + (data[i]*theta[i])
        }
        
        return(price)    
    }
}

# Predict price for a given house
predictPrice(data, theta)
