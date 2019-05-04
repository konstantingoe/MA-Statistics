library(Rlab)
library(fda)
library(stats)
library(MASS)
library(splines)
library(psych)
library(tensor)
library(crs)
library(orthogonalsplinebasis)
library(EQL)


m=10
n <- 500
N <- 1000
c.eig <-m

s.vec = c(-1, -1.5, -2)
w.vec = c(.2, .3, .4)


delta <- c()
delta[1:n]=1
ones<-rep(1,1,n)

#cos.F<-function(x,j){sqrt(2)^(as.numeric(1<j))*cos((j-1)*pi*x)}#
cos.F<-function(x,j){sqrt(2)*cos((j)*pi*x)}

#############################################
####### MONTE CARLO SIMULATION####################
##################################################
test.mat =matrix(nrow=12,ncol=5)
  for (s.const in 1:3){
  for (w.int in 1:3){
    
    set.seed(123456)
    
    
    s =s.vec[s.const]
    w.const =w.vec[w.int] 
    
    Weights= vector()
    for(j in 1:m) Weights[j] = j^s
    
  
    test=matrix(nrow=N,ncol=5)
    
    for (MC in 1:N)
    {
    
    unobs <- rnorm(3*n)
    W <- unobs[1:n]
    Y <- w.const*unobs[1:n] +sqrt(1-w.const^2)*unobs[(n+1):(2*n)]
    U <- unobs[(2*n+1):(3*n)]
    
    #BasWf.mat=mat.or.vec(n,m)
    #for(i in 1:m){BasWf.mat[,i] <- cos.F((W-min(W))/(max(W)-min(W)),i)}
    #BasWf.mat = gsl.bs(W,degree=17, nbreak=6)
    #knots<-expand.knots(c(0,as.vector(attr(bs(W, df=17, degree=4), "knots")),1))
    #BasWf.mat<-evaluate(OBasis(knots), W)
    
    BasWf.mat=mat.or.vec(n,m)
    for(i in 1:m){BasWf.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}
    #for(i in 1:m){BasWf.mat[,i] <- sqrt(pnorm(W))*hermite(W, i)/sqrt(factorial(i))}
   
    
    ############################
    ### Null Hypothesis  #######
    ############################
    
    q.const <- quantile(U,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    
    
    my <-f.fct(U)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
    Y.mis <- delta*Y
    
    
     
    h.hat <- sum(delta)/n 
    
    coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
    Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))
    
    #coef0.vec <-ginv(t(BasWf.mat)%*%BasWf.mat)%*%t(BasWf.mat)%*%(delta-h.hat)
    #Test0 <- t(coef0.vec)%*%t(BasWf.mat%*%diag(Weights))%*%BasWf.mat%*%diag(Weights)%*%coef0.vec
    
    
    C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
    #BasWf.mat <-t(ginv(t(BasWf.mat)%*%BasWf.mat)%*%t(BasWf.mat))
    Wf.mat <- BasWf.mat%*%diag(Weights)
    
    eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
    eps.h.mat <- as.vector(delta-h.hat)%*%(delta%*%Wf.mat/n)
    eps.mat   <- eps.1.mat - eps.h.mat
    Sigma.mat<-t(eps.mat)%*%eps.mat/n
    eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
    CC<-sort(eig.vec[1:c.eig]%*%C.mat)
    test[MC,1]<-as.numeric(Test0 >CC[9500])

    ##################
    #1st Alternative
    ##################
    YX.vec <- .3*Y+sqrt(1-.3^2)*U
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
    Y.mis <- delta*Y
    
  
    h.hat <- sum(delta)/n 
       
    coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
    Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))
    
    C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
    Wf.mat <- BasWf.mat%*%diag(Weights)
    eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
    #eps.g.mat <- diag(as.vector(g.hat-delta))%*%BasY.mat%*%t(BasY.mat)%*%Wf.mat*delta/n
    eps.h.mat <- as.vector(delta-h.hat)%*%(delta%*%Wf.mat/n)
    eps.mat  <- eps.1.mat - eps.h.mat
    Sigma.mat<-t(eps.mat)%*%eps.mat/n
    eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
    CC<-sort(eig.vec[1:c.eig]%*%C.mat)
    test[MC,2]<-as.numeric(Test0 >CC[9500])
   
    ##################
    #2nd Alternative
    ##################
    YX.vec <- 0.5*Y+sqrt(1-.5^2)*U
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
      
    Y.mis <- delta*Y
    
    
   h.hat <- sum(delta)/n 
   
   coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
   Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))
    
    C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
    Wf.mat <- BasWf.mat%*%diag(Weights)
    eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
    #eps.g.mat <- diag(as.vector(g.hat-delta))%*%BasY.mat%*%t(BasY.mat)%*%Wf.mat*delta/n
    eps.h.mat <- as.vector(delta-h.hat)%*%(delta%*%Wf.mat/n)
    eps.mat  <- eps.1.mat - eps.h.mat
    Sigma.mat<-t(eps.mat)%*%eps.mat/n
    eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
    CC<-sort(eig.vec[1:c.eig]%*%C.mat)
   test[MC,3]<-as.numeric(Test0 >CC[9500])
   
    ##################
    #3rd Alternative
    ##################
    YX.vec <- .7*Y+sqrt(1-.7^2)*U
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
    #proba <- pnorm(YX.vec)
    #delta <- ifelse(C.vec < proba,1,0)  
    Y.mis <- delta*Y
    
      
   h.hat <- sum(delta)/n 
     
    coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
    Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))
    
    
    C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
    Wf.mat <- BasWf.mat%*%diag(Weights)
    eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
    #eps.g.mat <- diag(as.vector(g.hat-delta))%*%BasY.mat%*%t(BasY.mat)%*%Wf.mat*delta/n
    eps.h.mat <- as.vector(delta-h.hat)%*%(delta%*%Wf.mat/n)
    eps.mat  <- eps.1.mat - eps.h.mat
    Sigma.mat<-t(eps.mat)%*%eps.mat/n
    eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
    CC<-sort(eig.vec[1:c.eig]%*%C.mat)
    test[MC,4]<-as.numeric(Test0 >CC[9500])
    
  
}

test.mat[((s.const^2-1)+w.int),] = colSums(test)/N
}

}
