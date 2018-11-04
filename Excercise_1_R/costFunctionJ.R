costFunctionJ = function(X, y, theta){
    # calcualte cost of a given theta
    J = 0
    
    m = length(y)
    
    # First calculate preditions column vector X * theta
    prediction = X %*% theta
    
    # Now calculate the error
    error = (prediction - y)
    
    J = (1/(2*m)) * sum(error^2)
    
    # Cost funciton returns J as well as squared error
    return(list('J' = J, 'Error' = error))
    
    }
