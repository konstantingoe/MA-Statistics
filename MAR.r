library(Rlab)
library(stats)
library(MASS)
library(splines)
 library(crs)
library(orthogonalsplinebasis)
library(EQL)

s.vec = c(-1, -1.5, -2)
w.vec = c(.3, .5, .7)

n <- 500
N <- 1000
m <- 10
c.eig <-m^2

delta <- c()
delta[1:n]=1
ones<-rep(1,1,n)

cos.F<-function(x,j){sqrt(2)*cos((j)*pi*x)}

#############################################
####### MONTE CARLO SIMULATION####################
##################################################
test.mat =matrix(nrow=12,ncol=5)
for (s.const in 1:3){
  for (w.int in 1:3){
    
    set.seed(73457)
    
    s =s.vec[s.const]
    Weights= vector()
    for(j in 1:(m^2)) {Weights[j] = j^s}

    w.const =w.vec[w.int] 
    test=matrix(nrow=N,ncol=5)
    
    for (MC in 1:N)
    {
      
      W <- rnorm(n)
      Xh <- rnorm(n,0,1)
      X <- (.2*W + sqrt(1-.2^2)*Xh)
      Y <- (w.const*W + sqrt(1-w.const^2)*Xh)+rnorm(n,0,.5)
      U <- 0#rnorm(n)
      
      ############################
      ### Null Hypothesis  #######
      ############################
      YX.vec <- X
      q.const <- quantile(YX.vec,probs= 0.2)
      f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
      my <-f.fct(YX.vec)
      for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
      
      
      cv.h <- crs(delta~X, cv="exhaustive",degree.max=3, segments.max = 5, knots="uniform")
      h.hat <- cv.h$fitted.values
      sum((delta-h.hat)^2)
      knots<- expand.knots(seq(min(X),max(X), length=(cv.h$K[2])), order =cv.h$K[1])
      BasX.mat = gsl.bs(X,degree=cv.h$K[1], nbreak=(cv.h$K[2]))
      BasX.mat =cbind(ones,BasX.mat)
     
      BasWh.mat=mat.or.vec(n,m)
      for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}
      BasXh.mat=mat.or.vec(n,m)
      for(i in 1:m){BasXh.mat[,i] <- hermite(X, i)/sqrt(factorial(i))}
      
      
      BasWf.mat=mat.or.vec(n,m^2)
      BasWf.mat <- mat.or.vec(n,(length(BasWh.mat[1,])*length(BasXh.mat[1,])))
      for( i in 1:n){ BasWf.mat[i,] <- kronecker(BasWh.mat[i,],BasXh.mat[i,])}
      
      
      coef0.vec <- t(BasWf.mat)%*%(delta-h.hat)/n
       Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))
      
      C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
      Wf.mat <- BasWf.mat%*%diag(Weights)# 
      
      eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
      eps.h.mat <- diag(as.vector(delta-h.hat))%*%BasX.mat%*%t(BasX.mat)%*%Wf.mat/n
      eps.mat  <- eps.1.mat - eps.h.mat
      Sigma.mat<-t(eps.mat)%*%eps.mat/n
      eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
      CC<-sort(eig.vec[1:c.eig]%*%C.mat)
      test[MC,1]<-as.numeric(Test0 >CC[9500])
      
      ##################
      #1st Alternative
      ##################
      YX.vec <- .3*Y+sqrt(1-.3^2)*X
      q.const <- quantile(YX.vec,probs= 0.2)
      f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
      my <-f.fct(YX.vec)
      for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
      Y.mis <- delta*Y
      
      cv.h <- crs(delta~X, cv="exhaustive",degree.max=3, segments.max = 5, knots="uniform")
      h.hat <- cv.h$fitted.values
      
      coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
      Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))
      
      C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
      Wf.mat <- BasWf.mat%*%diag(Weights)# 
      eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
      eps.h.mat <- diag(as.vector(delta-h.hat))%*%BasX.mat%*%t(BasX.mat)%*%Wf.mat/n
      eps.mat  <- eps.1.mat - eps.h.mat
      Sigma.mat<-t(eps.mat)%*%eps.mat/n
      eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
      CC<-sort(eig.vec[1:c.eig]%*%C.mat)
      test[MC,2]<-as.numeric(Test0 >CC[9500])
      
      
      
      ##################
      #2nd Alternative
      ##################
      YX.vec <- .5*Y+sqrt(1-.5^2)*X
      q.const <- quantile(YX.vec,probs= 0.2)
      f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
      my <-f.fct(YX.vec)
      for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
      Y.mis <- delta*Y
      
      cv.h <- crs(delta~X, cv="exhaustive",degree.max=3, segments.max = 5, knots="uniform")
      h.hat <- cv.h$fitted.values
      
      coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
      Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))
      
      C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
      Wf.mat <- BasWf.mat%*%diag(Weights)
      eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
      eps.h.mat <- diag(as.vector(delta-h.hat))%*%BasX.mat%*%t(BasX.mat)%*%Wf.mat/n
      eps.mat  <- eps.1.mat - eps.h.mat
      Sigma.mat<-t(eps.mat)%*%eps.mat/n
      eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
      CC<-sort(eig.vec[1:c.eig]%*%C.mat)
      test[MC,3]<-as.numeric(Test0 >CC[9500])
      
      
      ##################
      #3rd Alternative
      ##################
      YX.vec <- .7*Y+sqrt(1-.7^2)*X#.7*Y+sqrt(1-.7^2)*X+sqrt(1-.7^2)*U
      q.const <- quantile(YX.vec,probs= 0.2)
      f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
      my <-f.fct(YX.vec)
      for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
      Y.mis <- delta*Y
      
      cv.h <- crs(delta~X, cv="exhaustive",degree.max=3, segments.max = 5, knots="uniform")
      h.hat <- cv.h$fitted.values
      
      coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
      Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))

      C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
      Wf.mat <- BasWf.mat%*%diag(Weights)
      eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
      eps.h.mat <- diag(as.vector(delta-h.hat))%*%BasX.mat%*%t(BasX.mat)%*%Wf.mat/n
      eps.mat  <- eps.1.mat - eps.h.mat
      Sigma.mat<-t(eps.mat)%*%eps.mat/n
      eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
      CC<-sort(eig.vec[1:c.eig]%*%C.mat)
      test[MC,4]<-as.numeric(Test0 >CC[9500])
      
    }
    
    test.mat[((s.const^2-1)+w.int),] = colSums(test)/N
    
    print(test.mat[((s.const^2-1)+w.int),])
  }
}
test.mat



# [,1]  [,2]  [,3]  [,4] [,5]
# [1,] 0.045 0.091 0.105 0.147   NA
# [2,] 0.045 0.354 0.595 0.759   NA
# [3,] 0.045 0.733 0.943 0.986   NA
# [4,] 0.049 0.090 0.109 0.151   NA
# [5,] 0.049 0.359 0.584 0.750   NA
# [6,] 0.049 0.717 0.938 0.980   NA
# [7,]    NA    NA    NA    NA   NA
# [8,]    NA    NA    NA    NA   NA
# [9,] 0.050 0.093 0.113 0.153   NA
# [10,] 0.050 0.358 0.581 0.747   NA
# [11,] 0.050 0.709 0.936 0.979   NA

