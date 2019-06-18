library(Rlab)
library(np)
library(parallel)
  

w.vec = c(.3, .5, .7)

#s=-1
n <- 500
N <- 1000


delta <- c()
delta[1:n]=1
ones<-rep(1,1,n)

B.test <- vector()
# Epanechnikov Kernel
k.fct <- function(u){as.numeric(abs(u)<=1)*(1-u^2)*3/4}
k.fct <- function(u){dnorm(u)}
  
# Bandwith as in DELGADO & MANTEIGA (2001), see page 1482
h.c <- .005*n^{-1/3}


  #############################################
  ####### MONTE CARLO SIMULATION###############
  #############################################
  test.mat =matrix(nrow=12,ncol=5)
   for (w.int in 1:3){
    
  set.seed(123456)
    
  w.const =w.vec[w.int] 
  test=matrix(nrow=N,ncol=5)
  
  for (MC in 1:N)
  {
    W <- rnorm(n)
    Xh <- rnorm(n,0,1)
    X <- (.2*W + sqrt(1-.2^2)*Xh)
    Y <- (w.const*W + sqrt(1-w.const^2)*Xh)+rnorm(n,0,.5)
    
#     f.w <- function(w){as.numeric(W<=w)}
#     f.x <- function(x){as.numeric(X<=x)}
#     
    f.xw <- function(x,w){as.numeric(X<=x)*as.numeric(W<=w)}
    
    X.mat <- mat.or.vec(n,n)+X    
    K.mat <- (1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)
   
    ############################
    ### Null Hypothesis  #######
    ############################ 
    YX.vec <- X
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
    
#     delta.mat <- mat.or.vec(n,n)+delta
#     test.fct<- 
#       function(w,x){
#     sum((1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)*(delta.mat-t(delta.mat))*as.numeric(W<=w)*as.numeric(X<=x))}
#     
#     #This is the C_n statistic of DELGADO & MANTEIGA (2001), see page 1472
#     test[MC,1] <- sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)

    reg.object <- npreg(delta ~ X, bws=h.c, ckertype='gaussian', residuals=TRUE)
    epsilon_hat <- residuals(reg.object)
    

    f.B<- function(BI){       # hier beginnt der Simulationsloop
      V <- rbinom(n, 1,(1+5^.5)/sqrt(20))
      V <-replace(V, V==1, (1-5^.5)/2)
      V <-replace(V, V==0, (1+5^.5)/2)
      
      boot.eps <- epsilon_hat*V
      delta_star <- fitted(reg.object)+ boot.eps
      delta_star.mat <- mat.or.vec(n,n)+ delta_star
      
#       test.fct<-  #selbe Funktion wie du unten nur mit delta_star anstatt delta (die funktion kann nat. aus dem B-Loop raus)
#         function(w,x){
#           sum((K.mat*(delta_star.mat-t(delta_star.mat)))*as.numeric(W<=w)*as.numeric(X<=x))}
# sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)
      
      # This is the C_n^** statistic see page 1480
      sum((rowSums(crossprod(K.mat*(delta_star.mat-t(delta_star.mat)), mapply(f.xw,X,W))))^2)#sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)#sum(mapply(test_star.fct,W,X)^2)#
      }
    
    test[MC,1] <- mean(mcmapply(f.B,1:N, mc.cores = 12))


    ##################
    #1st Alternative
    ##################
    YX.vec <- .3*Y+sqrt(1-.3^2)*X
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
   
    reg.object <- npreg(delta ~ X, bws=h.c, ckertype='gaussian', residuals=TRUE)
    epsilon_hat <- residuals(reg.object)
    
    
f.B<- function(BI){       # hier beginnt der Simulationsloop
  V <- rbinom(n, 1,(1+5^.5)/sqrt(20))
  V <-replace(V, V==1, (1-5^.5)/2)
  V <-replace(V, V==0, (1+5^.5)/2)
  
  boot.eps <- epsilon_hat*V
  delta_star <- fitted(reg.object)+ boot.eps
  delta_star.mat <- mat.or.vec(n,n)+ delta_star
  
#   K.mat <- (1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)*(delta_star.mat-t(delta_star.mat))
  #       test.fct<-  #selbe Funktion wie du unten nur mit delta_star anstatt delta (die funktion kann nat. aus dem B-Loop raus)
  #         function(w,x){
  #           sum(K.mat*as.numeric(W<=w)*as.numeric(X<=x))}
  
  # This is the C_n^** statistic see page 1480
  sum((rowSums(crossprod(K.mat*(delta_star.mat-t(delta_star.mat)), mapply(f.xw,X,W))))^2)#sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)#sum(mapply(test_star.fct,W,X)^2)#
}

test[MC,2] <- mean(mcmapply(f.B,1:N, mc.cores = 12))
        
    
    
    ##################
    #2nd Alternative
    ##################
    YX.vec <- .5*Y+sqrt(1-.5^2)*X
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
        
    reg.object <- npreg(delta ~ X, bws=h.c, ckertype='gaussian', residuals=TRUE)
    epsilon_hat <- residuals(reg.object)
    
    
f.B<- function(BI){       # hier beginnt der Simulationsloop
  V <- rbinom(n, 1,(1+5^.5)/sqrt(20))
  V <-replace(V, V==1, (1-5^.5)/2)
  V <-replace(V, V==0, (1+5^.5)/2)
  
  boot.eps <- epsilon_hat*V
  delta_star <- fitted(reg.object)+ boot.eps
  delta_star.mat <- mat.or.vec(n,n)+ delta_star
  
#   K.mat <- (1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)*(delta_star.mat-t(delta_star.mat))
  #       test.fct<-  #selbe Funktion wie du unten nur mit delta_star anstatt delta (die funktion kann nat. aus dem B-Loop raus)
  #         function(w,x){
  #           sum(K.mat*as.numeric(W<=w)*as.numeric(X<=x))}
  
  # This is the C_n^** statistic see page 1480
  sum((rowSums(crossprod(K.mat*(delta_star.mat-t(delta_star.mat)), mapply(f.xw,X,W))))^2)#sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)#sum(mapply(test_star.fct,W,X)^2)#
}

test[MC,3] <- mean(mcmapply(f.B,1:N, mc.cores = 12))
    
    
    ##################
    #3rd Alternative
    ##################
    YX.vec <- .7*Y+sqrt(1-.7^2)*X
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
   
    reg.object <- npreg(delta ~ X, bws=h.c, ckertype='gaussian', residuals=TRUE)
    epsilon_hat <- residuals(reg.object)
    
    
f.B<- function(BI){       # hier beginnt der Simulationsloop
  V <- rbinom(n, 1,(1+5^.5)/sqrt(20))
  V <-replace(V, V==1, (1-5^.5)/2)
  V <-replace(V, V==0, (1+5^.5)/2)
  
  boot.eps <- epsilon_hat*V
  delta_star <- fitted(reg.object)+ boot.eps
  delta_star.mat <- mat.or.vec(n,n)+ delta_star
  
#   K.mat <- (1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)*(delta_star.mat-t(delta_star.mat))
  #       test.fct<-  #selbe Funktion wie du unten nur mit delta_star anstatt delta (die funktion kann nat. aus dem B-Loop raus)
  #         function(w,x){
  #           sum(K.mat*as.numeric(W<=w)*as.numeric(X<=x))}
  
  # This is the C_n^** statistic see page 1480
  sum((rowSums(crossprod(K.mat*(delta_star.mat-t(delta_star.mat)), mapply(f.xw,X,W))))^2)#sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)#sum(mapply(test_star.fct,W,X)^2)#
}

test[MC,4] <- mean(mcmapply(f.B,1:N, mc.cores = 12))
   
 print(test[MC,])
  }
  
  
  print(w.const)


Test.Matrix=matrix(ncol=5,nrow=N)
for (l in 1:5){  for (MC in 1:N){ Test.Matrix[MC,l]=as.integer(test[MC,l]>=qnorm(0.975))}} #Since it is a two sided test
Sign.Level=matrix(ncol=5)
for(l in 1:5) Sign.Level[l] =mean(Test.Matrix[,l])

 print(Sign.Level)
}



#h.c <- .005*n^{-1/3}
# .3: 0.028 0.138 0.212 0.288   NA
# .5: 0.027 0.163 0.263 0.377   NA
# .7: 0.027 0.232 0.37 0.526   NA


#h.c <- .004*n^{-1/3}
# 0: 0.05 0.221 0.301 0.379
# .3: 0.049 0.201 0.301 0.399   NA
# .5: 0.051 0.23 0.386 0.492   NA
# .7: 0.05 0.323 0.508 0.626   NA

