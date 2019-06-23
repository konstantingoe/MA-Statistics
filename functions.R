##### Functions #####

cos.F<-function(x,j){sqrt(2)*cos((j)*pi*x)}

#### MCAR Function ####

MCAR <- function(data = data, missvar = "missvar", instrument = "instrument", orthonormal.basis  = "hermite", m = round(sqrt(n))
) {
  
  if (sum(is.na(data[,instrument])) > 0) {
    stop("Stop, instrument should not have missing values")
  } else {
    s=-1.5     ####-1, -1.5, -2
    n <- length(data[,instrument])
    W <- data[,instrument]
    Y <- data[,missvar]
    m <- m
    c.eig <-m
    
    delta <- ifelse(is.na(data[,missvar]),0,1)
    Weights= vector()
    for(j in 1:m) Weights[j] = j^s
    
    
    h.hat <- sum(delta/n) 
    if (orthonormal.basis == "cosine"){
      if (max(W) > 1 | min(W) < 0) {
        stop("Stop, cosine basis function only takes values in [0,1]")
      } else {
      b.fct <- function(i){cos.F(W, i)/sqrt(factorial(i))}
      }
    } else if (orthonormal.basis == "hermite"){
      b.fct <- function(i){hermite(W, i)/sqrt(factorial(i))}
    }
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
    
    test <- list("n*Sn"= Test0, "Critical Value"= CC[950000], "Ratio"=Test0/CC[950000],
                 "Test decision"= if (Test0 > CC[950000]){
                   print("We reject the Null of MCAR")
                 } else {
                   print("We retain the Null of MCAR")})
    return(test)
    cat(paste("The test value is",Test0))
    cat(paste("While the critical value is",CC[950000]))
    cat(paste("Their ratio is", Test0/CC[950000]))
    
    if (Test0 > CC[950000]){
      cat("Consequently, we reject the Null of MCAR")
    } else {
      cat("Consequently, we retain the Null of MCAR")
      
    }
    
  }
}


##### MAR Function ####



#########################################################################################
#### TEST MAR: P(D=1|X,Y^*)=P(D=1|X) Response depends on hours worked   ###############
#########################################################################################


MAR <- function(data = data, missvar = "missvar", instrument = "instrument", controls = "controls", orthonormal.basis  = "hermite", m = round(sqrt(n))
) {
  
  if (sum(is.na(data[,instrument])) > 0) {
    print("Stop, instrument should not have missing values")
  } else if (sum(is.na(data[,controls])) > 0) {
    print("Stop, controls should also be non-missing")
  } else {  

        s=-1.5     ####-1, -1.5, -2
        n <- length(data[,instrument])
        W <- data[,instrument]
        Y <- data[,missvar]
        X <- data[,controls]
        m <- m
        c.eig <-m^2
        delta <- ifelse(is.na(data[,missvar]),0,1)
        
        
        Weights= vector()
        for(j in 1:(m^2)) Weights[j] = j^s
        
        # LM<- lm(delta~X+X6+X7) $ coefficients
        cv.h <- crs(delta~X, cv="exhaustive", segments.max = 10,degree.max=10, knots="uniform", kernel=FALSE)
        #cv.h$K# LM<- lm(delta~X+X6+X7)
        # summary(LM)
        h.hat <- cv.h$fitted.values
        
        
        
        knots<- expand.knots(seq(min(X),max(X), length=(cv.h$K[1,2])), order =max(cv.h$K[1,1],2))
        #BasX.mat = cbind(1,gsl.bs(X,degree=max(cv.h$K[1,1],2), nbreak=max(2,cv.h$K[1,2])))
        
        BasX.mat = cbind(1,gsl.bs(X,degree=max(4,2), nbreak=max(2,cv.h$K[1,2])))
        
        h.hat <- BasX.mat%*%ginv(t(BasX.mat)%*%BasX.mat)%*%t(BasX.mat)%*%delta
        
        if (orthonormal.basis == "cosine"){
          if (max(W) > 1 | min(W) < 0 | max(X) > 1 | min(X) < 0){
            stop("Stop, cosine basis function only takes values in [0,1]")
          } else {

        BasWh.mat=mat.or.vec(n,m)
        for(i in 1:m){BasWh.mat[,i] <- cos.F(W, i)/sqrt(factorial(i))}
          
        BasXh.mat=mat.or.vec(n,m)
        for(i in 1:m){BasXh.mat[,i] <- cos.F(X, i)/sqrt(factorial(i))}
          }  
          
        } else if (orthonormal.basis == "hermite"){
        BasWh.mat=mat.or.vec(n,m)
        for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}
        
        BasXh.mat=mat.or.vec(n,m)
        for(i in 1:m){BasXh.mat[,i] <- hermite(X, i)/sqrt(factorial(i))}
        }
        BasWf.mat=mat.or.vec(n,m^2)
        BasWf.mat <- mat.or.vec(n,(length(BasWh.mat[1,])*length(BasXh.mat[1,])))
        for( i in 1:n){ BasWf.mat[i,] <- kronecker(BasWh.mat[i,],BasXh.mat[i,])}
        
        
        coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
        coef2.vec <- sort(coef0.vec^2, decreasing = TRUE, index.return=TRUE)
        
        BasWf.mat <- BasWf.mat[,coef2.vec$ix]
        coef2.vec <-coef2.vec$x
        
        Test0 <- n*sum(Weights^2*coef2.vec)
        
        C.mat<-matrix(rnorm(c.eig*1000000),c.eig,1000000)^2 
        Wf.mat <- BasWf.mat%*%diag(Weights)
        
        eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
        eps.h.mat <- diag(as.vector(delta-h.hat))%*%BasX.mat%*%t(BasX.mat)%*%Wf.mat/n
        eps.mat  <- eps.1.mat - eps.h.mat
        Sigma.mat<-t(eps.mat)%*%eps.mat/n
        eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
        CC<-sort(eig.vec[1:c.eig]%*%C.mat)
        
        test <- list("n*Sn"= Test0, "Critical Value"= CC[950000], "Ratio"=Test0/CC[950000],
                     "Test decision"= if (Test0 > CC[950000]){
                       print("We reject the Null of MAR given X")
                     } else {
                       print("We retain the Null of MAR given X")})
        return(test)
        cat(paste("The test value is",Test0))
        cat(paste("While the critical value is",CC[950000]))
        cat(paste("Their ratio is", Test0/CC[950000]))
        
        if (Test0 > CC[950000]){
          cat("Consequently, we reject the Null of MAR given X")
        } else {
          cat("Consequently, we retain the Null of MAR given X")
  
    }
  }
}


