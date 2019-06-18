############################################################################
###Programm for estimating conditional expectation with missing outcomes
#############################################################################

library(gdata)
library(fda)
library(foreign)
library(sem)
library(zoo)
library(orthogonalsplinebasis)
library(quantreg)
library(MASS)

#####################
###Set Parameters  ##
#####################
k=6
kk=14  ##k=5 then max kk is 11
dkk=5

b.const = .9

############################
### Read Data  #############
############################
setwd("~/Dropbox/R/MAR/JBES Revision/Data")
DAT <- read.dta13("data-13.dta")
#   Wt <- DAT$Wpastinc
DATA <- read.xlsx("Dataset_13_09.xlsx",1)
Y <- DATA$labgro13
W <- DATA$W
X<-as.numeric(as.character(DATA$bderwzeit)) # Length Of Time With Firm
X2<- as.numeric(DAT$is8813) #as.numeric(DAT$stib13)

# Replace missing values for instruments by ISCO
W[which(W==-1)]<- c(89253, 33555, 58410, 74328, 51890, 33539, 35250, 57402, 45616, 37273, 74328)/12
W[which(W==0)]<- 51890/12  #civil engineer



n <- length(W)

ones<-rep(1,1,n)

delta <- vector()
for(i in 1:n)if(Y[i]== -1){delta[i]<-0}  else {delta[i]<-1}



1-sum(delta)/n

Y=delta*Y



n <- length(W)
ones<-rep(1,1,n)



###############################
######### Constraint Estimator
###############################
knots.y <- attr(bs(subset(Y, Y>0), df=(k-1), degree=dkk), "knots")
#b.knots.y <- attr(bs(Y, df=k, degree=dkk),"Boundary.knots")
knots<- expand.knots(c(-1,as.vector(knots.y),Y.max), order = dkk)
ybase<-SplineBasis(knots, order=dkk)
BasY0.mat =evaluate(ybase, Y)
BasY.mat =cbind(ones,BasY0.mat)
BasY.mat = BasY.mat*delta

knots.w <- attr(bs(W, df=(kk-1), degree=dkk), "knots")
#b.knots.w <- attr(bs(X, df=kk, degree=dkk),"Boundary.knots")
#knots<- c(0,0,0,c(0,as.vector(knots.w),6000), 6000, 6000, 6000)
knots<- expand.knots(c(0,as.vector(knots.w),max(W)), order = dkk)
wbase<-SplineBasis(knots, order=dkk)
BasW.mat<-evaluate(wbase, W)
BasW.mat <- cbind(ones,BasW.mat)



ff.help<-function(x){
  return(
    sum((BasW.mat%*%ginv(t(BasW.mat)%*%BasW.mat)%*%(rowSums(t(BasW.mat))
                                                    -t(BasW.mat)%*%BasY.mat%*%x[1:k]))^2))}    #+ .1*sum((x[1:k])^2))}

pen.mat <- OuterProdSecondDerivative(ybase) + t(BasY0.mat*delta)%*%(BasY0.mat*delta)

ff0<-function(x){ff.help(x[1:k])}#+0.00001*x[2:k]%*%pen.mat%*%x[2:k]}
c.vec<-rep(1,1,k)
y.grid <- seq(min(Y),Y.max,length=20)
y.grid <- subset(Y, Y>0)
knots.y <- attr(bs(Y, df=(k-1), degree=dkk), "knots")
b.knots.y <- attr(bs(Y, df=(k-1), degree=dkk),"Boundary.knots")

A <- matrix( nrow = length(y.grid), ncol = (k-1))
for (i in 1:length(y.grid)){for (ki in 1:(k-1)){ A[i,ki] <- 
                                                   bs(y.grid[i], df=(k-1), knots=knots.y, degree = dkk, Boundary.knots =b.knots.y)[ki]}}
A <- cbind(1,A)
b <- rep(b.const,1,length(y.grid))
tsls1.est <-constrOptim(c.vec, ff0, NULL, ui = A, ci = b)$par
#g.hat1 <- cbind(ones,bs(Y, df=(k-1), knots=knots.y, degree = 3, Boundary.knots =b.knots.y))
g.hat1 <- cbind(1,evaluate(ybase, Y))%*%tsls1.est

g.constr.hat<-vector()
for (i in 1:n) if (Y[i]>0){g.constr.hat[i]<-g.hat1[i]}else {g.constr.hat[i]<-0}
phi.vec <- Y*g.constr.hat
rcoef.constr.vec<-ginv(t(BasW.mat)%*%BasW.mat)%*%t(BasW.mat)%*%phi.vec

##################################################################################
###Plot est of inverse conditional prob that Delta=1 given potential outcomes
##################################################################################
#BasW.mat = bs(X, df=(kk-1), degree=3)
#BasW.mat =cbind(ones,BasW.mat)#

y<-seq(min(Y),15000,length=1000)
knots.y <- attr(bs(Y, df=(k-1), degree=dkk), "knots")
b.knots.y <- attr(bs(Y, df=(k-1), degree=dkk),"Boundary.knots")
#g.est <- cbind(1,bs(y, df=(k-1), knots=knots.y, degree = 3, Boundary.knots =b.knots.y))%*%ginv(A)%*%t(BasY.mat)%*%BasW.mat%*% ginv(t(BasW.mat)%*%BasW.mat)%*%rowSums(t(BasW.mat))
g.constr.est<- cbind(1,evaluate(ybase, y))%*%tsls1.est
prop <- (1/g.constr.est)
plot(y,prop, type = "n", ylim=c(0.87, .94), xlim=c(min(Y),15000), ylab="P(D=1|Y*=y)", xlab="y")
#lines(y,g.est, col = "blue")
lines(y,prop)


##############################################
##### Estimation of averages #################
##############################################


mean(Y)
mean(Y/cbind(1,evaluate(ybase, Y))%*%tsls1.est)

##############################################
##### TEST  Instrument Validity ##############
##############################################
m=15
k=3
dkk=3



BasY.mat = gsl.bs(Y,degree=dkk, nbreak=k) 
BasY.mat =cbind(ones,BasY.mat)
BasY.mat = BasY.mat*delta
k=length(BasY.mat[1,])

BasW.mat = gsl.bs(W,degree=4, nbreak=8) 
BasW.mat =cbind(ones,BasW.mat)
# BasW.mat = BasW.mat*delta

ff.help<-function(x){
  return(
    sum((BasW.mat%*%ginv(t(BasW.mat)%*%BasW.mat)%*%(rowSums(t(BasW.mat))
                                                    -t(BasW.mat)%*%BasY.mat%*%x[1:k]))^2))}    #+ .1*sum((x[1:k])^2))}


ff0<-function(x){ff.help(x[1:k])}
c.vec<-rep(2,1,k)
# y.grid <- seq(min(Y),(max(Y)),length=20)
# y.grid <- subset(Y, Y>0)
# knots.y <- attr(bs(Y, df=(k-1), degree=dkk), "knots")
# b.knots.y <- attr(bs(Y, df=(k-1), degree=dkk),"Boundary.knots")
# 
# A <- matrix( nrow = length(y.grid), ncol = (k-1))
# for (i in 1:length(y.grid)){for (ki in 1:(k-1)){ A[i,ki] <- 
#                                                    bs(y.grid[i], df=(k-1), knots=knots.y, degree = dkk, Boundary.knots =b.knots.y)[ki]}}
A <- BasY.mat[which(Y>0),]#cbind(1,A)
b <- rep(1,1,length(BasY.mat[which(Y>0),1]))#rep(1,1,length(y.grid))
tsls1.est <-constrOptim(c.vec, ff0, NULL, ui = A, ci = b)$par
#g.hat1 <- cbind(ones,bs(Y, df=(k-1), knots=knots.y, degree = 3, Boundary.knots =b.knots.y))

g.constr.hat<-vector()
for (i in 1:n) if (Y[i]>0){g.constr.hat[i]<-BasY.mat[i,]%*%tsls1.est}else {g.constr.hat[i]<-0}


# n=length(Y)
# test<-vector()
# M.max=50
# for (m in 15:M.max){
  BasWf.mat=mat.or.vec(n,m)
  for(i in 1:m){BasWf.mat[,i]=hermite(log(W)-mean(log(W)), i)/sqrt(factorial(i))}
#   BasWf.mat<- BasW.mat
  coef0.vec= (1/n)*t((delta*g.constr.hat-ones))%*%(BasWf.mat)
#   coef0.vec= ginv(t(BasW.mat)%*%BasW.mat)%*%(rowSums(t(BasW.mat))-t(BasW.mat)%*%BasY.mat%*%tsls1.est[1:k])


  S0.n =sum(coef0.vec*coef0.vec)

#  Baso.mat<- t(ginv(t(BasW.mat)%*%BasW.mat)%*%t(BasW.mat))
  
  B.mat=mat.or.vec(n,m)
  for(i in 1:length(BasWf.mat[i,])){B.mat[,i] <-(delta*g.constr.hat-ones)*BasWf.mat[,i] }
#  B.mat=mat.or.vec(n,length(BasWf.mat[i,]))
#  for(i in 1:length(BasWf.mat[i,])){B.mat[,i] <-(delta*g.constr.hat-ones)*Baso.mat[,i] }
  Sigma0 <- t(B.mat)%*%(B.mat)/n
  Test0 <- (n*S0.n - tr(Sigma0))/(sqrt(2)*norm(Sigma0, type = "F"))
  
   print(Test0)
  #print(qnorm(0.995))
1-pnorm(Test0)  # one sided p-value





############################################################
##### TEST  Instrument Validity with Covariates X ##########
############################################################
k=5
dkk=2
m=5

BasYh.mat = gsl.bs(Y,degree=dkk, nbreak=k) 
# BasY.mat =cbind(ones,BasY.mat)
BasYh.mat = BasYh.mat*delta
BasX.mat = gsl.bs(X,degree=dkk, nbreak=k)
BasY.mat<- mat.or.vec(n,(1+length(BasYh.mat[1,])+length(BasX.mat[1,])))
for( i in 1:n){ BasY.mat[i,] <- c(1,BasYh.mat[i,],BasX.mat[i,])}


BasWh.mat = gsl.bs(W,degree=4, nbreak=3) 
BasXh.mat = gsl.bs(X,degree=4, nbreak=3)
BasXh.mat =cbind(ones,BasXh.mat)
 BasWh.mat =cbind(ones,BasWh.mat)
# BasW.mat<- mat.or.vec(n,(1+length(BasWh.mat[1,])+length(BasX.mat[1,])))
# for( i in 1:n){ BasW.mat[i,] <- c(1,BasWh.mat[i,],BasX.mat[i,])}

BasW.mat<- mat.or.vec(n,(length(BasWh.mat[1,])*length(BasXh.mat[1,])))
for( i in 1:n){ BasW.mat[i,] <- kronecker(BasWh.mat[i,],BasXh.mat[i,])}



# BasW.mat = BasW.mat*delta

ff.help<-function(x){
  return(
    sum((BasW.mat%*%ginv(t(BasW.mat)%*%BasW.mat)%*%(rowSums(t(BasW.mat))
                                                    -t(BasW.mat)%*%BasY.mat%*%x[1:length(BasY.mat[1,])]))^2))}    #+ .1*sum((x[1:k])^2))}


ff0<-function(x){ff.help(x[1:length(BasY.mat[1,])])}#+0.2*x[2:k]%*%pen.mat%*%x[2:k]}
c.vec<-rep(2,1,length(BasY.mat[1,]))

tsls1.est <-constrOptim(c.vec, ff0, NULL, ui = BasY.mat, ci = rep(1,1,n))$par

g.constr.hat<-vector()
for (i in 1:n) if (Y[i]>0){g.constr.hat[i]<-BasY.mat[i,]%*%tsls1.est}else {g.constr.hat[i]<-0}


# n=length(Y)
# test<-vector()
# M.max=50
# for (m in 15:M.max){
BasWh.mat=mat.or.vec(n,m)
for(i in 1:m){BasWh.mat[,i]=hermite(log(W)-mean(log(W)), i)/sqrt(factorial(i))}

BasXh.mat=mat.or.vec(n,m)
for(i in 1:m){BasXh.mat[,i] <- hermite(log(X)-mean(log(X)), i)/sqrt(factorial(i))}


BasWf.mat=mat.or.vec(n,m^2)
BasWf.mat <- mat.or.vec(n,(length(BasWh.mat[1,])*(length(BasXh.mat[1,]))))
for( i in 1:n){ BasWf.mat[i,] <- kronecker(BasWh.mat[i,],BasXh.mat[i,])}


#   BasWf.mat<- BasW.mat
coef0.vec= (1/n)*t((delta*g.constr.hat-ones))%*%(BasWf.mat)
#   coef0.vec= ginv(t(BasW.mat)%*%BasW.mat)%*%(rowSums(t(BasW.mat))-t(BasW.mat)%*%BasY.mat%*%tsls1.est[1:k])


S0.n =sum(coef0.vec*coef0.vec)

# Baso.mat<- t(ginv(t(BasW.mat)%*%BasW.mat)%*%t(BasW.mat))

B.mat=mat.or.vec(n,m^2)
for(i in 1:length(BasWf.mat[i,])){B.mat[,i] <-(delta*g.constr.hat-ones)*BasWf.mat[,i] }
#  B.mat=mat.or.vec(n,length(BasWf.mat[i,]))
#  for(i in 1:length(BasWf.mat[i,])){B.mat[,i] <-(delta*g.constr.hat-ones)*Baso.mat[,i] }
Sigma0 <- t(B.mat)%*%(B.mat)/n
Test0 <- (n*S0.n - tr(Sigma0))/(sqrt(2)*norm(Sigma0, type = "F"))

print(Test0)
#print(qnorm(0.995))
1-pnorm(Test0)  # one sided p-value







##################################################################################
####################                 Bootstrap  for constrained estimator
##################################################################################

set.seed(123456)
grid <-seq(min(Y),max(Y),length=50)
 Conf.int1=matrix(nrow=2, ncol=length(grid))
for (x in 1:length(grid))
{
  Bootstrap.sample=vector()		# saves for each subsample the calculated statistics
  for (j in  1:500){
    sample.index=vector()
    sample.index=sample(1:length(Y), replace=TRUE)
    Yb=vector()#matrix(nrow=length(sample.index),ncol=length(Y))
    Xb=vector()#matrix(nrow=length(sample.index),ncol=length(Y))
    deltab=vector()#matrix(nrow=length(sample.index),ncol=length(Y))
  
    for (i in 1:length(Y)) Yb[i]=Y[sample.index[i]]
    for (i in 1:length(Y)) Xb[i]=X[sample.index[i]]
    for (i in 1:length(Y)) deltab[i]=delta[sample.index[i]]
    
    
    knots.y <- attr(bs(subset(Yb, Yb>0), df=(k-1), degree=dkk), "knots")
    knots<- expand.knots(c(0,as.vector(knots.y),60), order = 4)
    ybase<-SplineBasis(knots)
    BasY0.mat =evaluate(ybase, Yb)
    BasY.mat =cbind(ones,BasY0.mat)
    BasY.mat = BasY.mat*deltab
    
    knots.w <- attr(bs(Xb, df=(kk-1), degree=dkk), "knots")
    knots<- expand.knots(c(0,as.vector(knots.w),6000), order = 4)
    wbase<-SplineBasis(knots)
    BasW.mat<-evaluate(wbase, Xb)
    BasW.mat <- cbind(ones,BasW.mat)
    
    #BasY.mat = bs(Yb, df=(k-1), degree=3)
    #BasY.mat =cbind(ones,BasY.mat)
    #BasY.mat = BasY.mat*deltab
    #BasW.mat = evaluate(OBasis(knots.w), X) #eval.basis (X,Basis)
    #BasW.mat = bs(Xb, df=(kk-1), degree=dkk)
    #BasW.mat =cbind(ones,BasW.mat)
       
    ff.help<-function(x){
      return(
        sum((BasW.mat%*%ginv(t(BasW.mat)%*%BasW.mat)%*%(rowSums(t(BasW.mat))-t(BasW.mat)%*%BasY.mat%*%x[1:k]))^2))}

    pen.mat <- OuterProdSecondDerivative(ybase) + t(BasY0.mat*deltab)%*%(BasY0.mat*deltab)
    
    ff0<-function(x){ff.help(x[1:k])+0.002*x[2:k]%*%pen.mat%*%x[2:k]}
    
    
    #c.vec<-c(4.322694, 1.939315, -5.908744, -2.149053, -3.224265)#rep(2,1,k)
    c.vec<-rep(2,1,k)
    y.grid <- seq(min(Yb),(max(Yb)),length=20)
    #y.grid <- subset(Yb, Yb>0)
    knots.y <- attr(bs(Yb, df=(k-1), degree=3), "knots")
    b.knots.y <- attr(bs(Yb, df=(k-1), degree=3),"Boundary.knots")
    
    A <- matrix( nrow = length(y.grid), ncol = (k-1))
    for (i in 1:length(y.grid)){for (ki in 1:(k-1)){ A[i,ki] <- 
                   bs(y.grid[i], df=(k-1), knots=knots.y, degree = 3, Boundary.knots =b.knots.y)[ki]}}
    A <- cbind(1,A)
    b <- rep(b.const,1,length(y.grid))
    tsls1.est <-constrOptim(c.vec, ff0, NULL, ui = A, ci = b)$par
    #g.hat <- cbind(ones,bs(Yb, df=(k-1), knots=knots.y, degree = 3, Boundary.knots =b.knots.y))
    #g.hat <- cbind(1,evaluate(ybase, Yb))%*%tsls1.est
     #A<-t(BasY.mat)%*%BasW.mat%*% ginv(t(BasW.mat)%*%BasW.mat)%*%t(BasW.mat)%*%BasY.mat
    #g.hat<-BasY.mat%*%ginv(A)%*%t(BasY.mat)%*%BasW.mat%*% ginv(t(BasW.mat)%*%BasW.mat)%*%rowSums(t(BasW.mat))
    
    
    y3 = grid[x]
    
    Bootstrap.sample[j] <- 1/(cbind(1,evaluate(ybase, y3))%*%tsls1.est)
  }
  Conf.int1[1,x]=quantile(Bootstrap.sample,probs= 0.05)
  Conf.int1[2,x]=quantile(Bootstrap.sample,probs= 0.95)
  #rm(Bootstrap.sample)
  print(x)
}
lines(grid,Conf.int1[1,], col = "black")
lines(grid,Conf.int1[2,], col = "black")
