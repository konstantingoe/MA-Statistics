##### Functions #####

MCAR <- function(data = data, missvar = "missvar", instrument = "instrument") {
  
  if (sum(is.na(data[,instrument])) > 0) {
    print("Stop, instrument should not have missing values")
  } else {
    s=-1.5     ####-1, -1.5, -2
    n <- length(data[,instrument])
    W <- data[,instrument]
    Y <- data[,missvar]
    m <- round(sqrt(n))
    c.eig <-m
    
    delta <- ifelse(is.na(data[,missvar]),0,1)
    Weights= vector()
    for(j in 1:m) Weights[j] = j^s
    
    
    h.hat <- sum(delta/n) 
    b.fct <- function(i){hermite(W, i)/sqrt(factorial(i))}
    BasWf.mat<-matrix(unlist(mclapply(1:m, b.fct, mc.cores = detectCores() -1)), n, m)
    
    coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
    coef2.vec <- sort(coef0.vec^2, decreasing = TRUE, index.return=TRUE)
    
    BasWf.mat <- BasWf.mat[,coef2.vec$ix]
    coef2.vec <-coef2.vec$x
    
    Test0 <- n*sum(Weights^2*coef2.vec)
    
    
    C.mat<-matrix(rnorm(c.eig*1000000),c.eig,1000000)^2 
    Wf.mat <- BasWf.mat%*%diag(Weights)
    
    eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
    eps.h.mat <- as.vector(delta-h.hat)%*%(delta%*%Wf.mat/n)
    eps.mat   <- eps.1.mat - eps.h.mat
    Sigma.mat<-t(eps.mat)%*%eps.mat/n
    eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)[["values"]]))
    CC<-sort(eig.vec[1:c.eig]%*%C.mat)
    
    print(paste("The test value is",Test0))
    print(paste("While the critical value is",CC[950000]))
    print(paste("Their ratio is", Test0/CC[950000]))
    
    if (Test0 > CC[950000]){
      print("Consequently, we reject the Null of MCAR")
    } else {
      print("Consequently, we retain the Null of MCAR")
      
    }
    
  }
}