library(Rlab)
library(fda)
library(stats)
library(MASS)
library(splines)
library(foreign)
library(orthogonalsplinebasis)
library(MASS)
library(gdata)
library(readstata13)
library(EQL)
library(xlsx)
library(crs)
library(parallel)

#####################
###Set Parameters  ##
#####################

  
  s=-1.5     ####-1, -1.5, -2
  
  ############################
  ### Read Data  #############
  ############################
  setwd("~/Dropbox/R/MAR/2nd JBES Revision/Data")
  DATA <- read.xlsx("Dataset_13_09.xlsx",1)
  Y <- DATA$labgro13
  X1<-as.numeric(as.character(DATA$bderwzeit)) # Length Of Time With Firm
  #Other possible variables:
    X2<- as.numeric(as.character(DATA$bdtatzeit)) # Actual Work Time Per Week has almost no effect on missingness
    # Replace missing values of DATA$bdtatzeit by ISCO DATA$bdtatzeit
    X2[which(X2==-1)]<- c(48.225, 43.45, 48.225, 42.925,40.525 , 43.45, 48.225,43.45)
    X2[which(X2==-3)]<- 39.6




    W <- DATA$W   #Past values of labor income
  # Replace missing values for instruments by ISCO
     W[which(W==-1)]<- c(89253, 33555, 58410, 74328, 51890, 33539, 35250, 57402, 45616, 37273, 74328)/12
     W[which(W==0)]<- 51890/12  #civil engineer

   W<-log(W)-mean(log(W))
   X1<-(log(X1)-mean(log(X1)))
   X2<-(log(X2)-mean(log(X2)))
#   
  n <- length(W)
  ones<-rep(1,1,n)
  
  delta <- vector()
  for(i in 1:n)if(Y[i]== -1){delta[i]<-0}  else {delta[i]<-1}
  
  # LM<- lm(delta~X)
  # summary(LM)
  
  1-sum(delta)/n
  
  Y.mis=delta*Y
  
  ########################
  #### TEST MCAR      #### 
  ########################
  m=20
  c.eig <-m
  
  Weights= vector()
  for(j in 1:m) Weights[j] = j^s
  
  
  h.hat <- sum(delta/n) 
  b.fct <- function(i){hermite(W, i)/sqrt(factorial(i))}
  BasWf.mat<-matrix(unlist(mclapply(1:m, b.fct, mc.cores = 7)), n, m)
   
  
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
  eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
  CC<-sort(eig.vec[1:c.eig]%*%C.mat)
  
  Test0
  CC[950000]
  
  Test0/CC[950000]



  ############################################
  #### TEST MCAR:P(D=1|X,Y^*)=P(D=1)   #######
  ############################################
  m=5
  c.eig <-m
  Weights= vector()
  for(j in 1:(m^2)) Weights[j] = j^s
  h.hat <- sum(delta/n)
  
  
  BasWh.mat=mat.or.vec(n,m)
  for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}
  
  BasXh.mat=mat.or.vec(n,m)
  for(i in 1:m){BasXh.mat[,i] <- hermite(X1, i)/sqrt(factorial(i))}

  
  
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
  eps.h.mat <- as.vector(delta-h.hat)%*%(ones%*%Wf.mat/n)
  eps.mat  <- eps.1.mat - eps.h.mat
  Sigma.mat<-t(eps.mat)%*%eps.mat/n
  eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
  CC<-sort(eig.vec[1:c.eig]%*%C.mat)
  
  
  Test0
  CC[950000]
  
  


  
  
  ############################################
  #### TEST MAR: P(D=1|X1,Y^*)=P(D=1|X1)  ###### 
  ############################################
  m=5
  c.eig <-m^2
  
  Weights= vector()
  for(j in 1:(m^2)) Weights[j] = j^s
  
  cv.h <- crs(delta~X1, cv="exhaustive", segments.max = 10,degree.max=10, knots="uniform", kernel=FALSE)
  cv.h$K# LM<- lm(delta~X+X6+X7)
  # summary(LM)
        h.hat <- cv.h$fitted.values
  
  
  
  knots<- expand.knots(seq(min(X1),max(X1), length=(cv.h$K[1,2])), order =max(cv.h$K[1,1],2))
  BasX.mat = cbind(1,gsl.bs(X1,degree=max(cv.h$K[1,1],2), nbreak=max(2,cv.h$K[1,2])))
  
  BasX.mat = cbind(1,gsl.bs(X1,degree=max(4,2), nbreak=max(2,cv.h$K[1,2])))
  
  h.hat <- BasX.mat%*%ginv(t(BasX.mat)%*%BasX.mat)%*%t(BasX.mat)%*%delta
  BasWh.mat=mat.or.vec(n,m)
  for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}

  BasXh.mat=mat.or.vec(n,m)
  for(i in 1:m){BasXh.mat[,i] <- hermite(X1, i)/sqrt(factorial(i))}

  
  
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
  
  
  Test0
  CC[950000]
  Test0/CC[950000]
  
  
  
  #########################################################################################
  #### TEST MAR: P(D=1|X2,Y^*)=P(D=1|X2) Response depends on hours worked   ###############
  #########################################################################################
  m=5
  c.eig <-m^2
  
  Weights= vector()
  for(j in 1:(m^2)) Weights[j] = j^s
  
  # LM<- lm(delta~X+X6+X7) $ coefficients
  cv.h <- crs(delta~X2, cv="exhaustive", segments.max = 10,degree.max=10, knots="uniform", kernel=FALSE)
  cv.h$K# LM<- lm(delta~X+X6+X7)
  # summary(LM)
  h.hat <- cv.h$fitted.values
  
  
  
  knots<- expand.knots(seq(min(X2),max(X2), length=(cv.h$K[1,2])), order =max(cv.h$K[1,1],2))
  BasX.mat = cbind(1,gsl.bs(X2,degree=max(cv.h$K[1,1],2), nbreak=max(2,cv.h$K[1,2])))
  
  BasX.mat = cbind(1,gsl.bs(X2,degree=max(4,2), nbreak=max(2,cv.h$K[1,2])))
  
  h.hat <- BasX.mat%*%ginv(t(BasX.mat)%*%BasX.mat)%*%t(BasX.mat)%*%delta
  BasWh.mat=mat.or.vec(n,m)
  for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}

  BasXh.mat=mat.or.vec(n,m)
  for(i in 1:m){BasXh.mat[,i] <- hermite(X2, i)/sqrt(factorial(i))}
  
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
  
  
  Test0
  CC[950000]
  Test0/CC[950000]
  
  




########################
#### Little's Test  #### 
########################
Y <- (Y_up+Y_low)/2


n <- length(X1)
ones<-rep(1,1,n)

delta <- vector()
Y_help <- Y_up-Y_low
#for(i in 1:n)if(Y_help[i]>0){delta[i]<-0}else {delta[i]<-1}
for(i in 1:n)if(Y_help[i]>10){delta[i]<-NA}else {delta[i]<-1}


1-sum(delta)/n

Y.mis=delta*Y


data <- mat.or.vec(n,2)v
data[,1] <- W
data[,2] <- Y.mis
data.fr <-data.frame(Y.mis,W)
LittleMCAR(data.fr)
LittleMCAR(data.fr)$chi.square
LittleMCAR(data.fr)$p.value

