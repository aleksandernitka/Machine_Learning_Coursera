normaliser1000 = function(X){
    
    # Setup space holder for mean of each column of X
    means = rep(0, ncol(X))
    
    # Setup Space holder for sd of each column of X
    sds = rep(0, ncol(X))
    
    # Calculate mean/sd for each column of X
    for (m in 1:length(means)){
        means[m] = mean(X[,m])
        sds[m] = sd(X[,m])
    }
    
    # Normalise as x = (x-m)/sd:
    for (r in 1:ncol(X)){
        X[,r] = (X[,r] - means[r])/sds[r]
    }
    
    return(X)
}