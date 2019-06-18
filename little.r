library(mvnmle)
library(BaylorEdPsych)


n <- 500
N=1000


delta <- c()
delta[1:n]=1

w.vec = c(.2, .3, .4)



test.mat =matrix(nrow=3,ncol=4)
for (w.int in 1:3){
  
  set.seed(123)
  
  w.const =w.vec[w.int]
  
  test=matrix(nrow=N,ncol=12)
  
  for (MC in 1:N)
  {
    
    unobs <- rnorm(3*n)
    W <- unobs[1:n]
    Y <- w.const*unobs[1:n] +sqrt(1-w.const^2)*unobs[(n+1):(2*n)]
    U <- unobs[(2*n+1):(3*n)]

    
    ############################
    ### Null Hypothesis  #######
    ############################
    
    q.const <- quantile(U,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    
    
    my <-f.fct(U)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
    Y.mis <- delta*Y
    Y.mis <- ifelse(Y.mis==0, NA, Y.mis)
    
    
    
    data <- mat.or.vec(n,2)
    data[,1] <- W
    data[,2] <- Y.mis
    data.fr <-data.frame(Y.mis,W)
    LittleMCAR(data.fr)$chi.square
    LittleMCAR(data.fr)$df
    qchisq(.95, 1)
    test[MC,1] <- LittleMCAR(data.fr)$chi.square
    #test[MC,s.const]<-as.numeric(Test0 >CC[9500])
    
    
       
    
    ##################
    #1st Alternative
    ##################
    YX.vec <- .3*Y+sqrt(1-.3^2)*U
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
    Y.mis <- delta*Y
    Y.mis <- ifelse(Y.mis==0, NA, Y.mis)
    
    
    data <- mat.or.vec(n,2)
    data[,1] <- W
    data[,2] <- Y.mis
    data.fr <-data.frame(Y.mis,W)
    LittleMCAR(data.fr)$chi.square
    test[MC,2] <- LittleMCAR(data.fr)$chi.square
    
    ##################
    #2nd Alternative
    ##################
    YX.vec <- 0.5*Y+sqrt(1-.5^2)*U
    q.const <- quantile(YX.vec,probs= 0.2)
    f.fct<-function(x){as.numeric(x>=q.const)+.1*as.numeric(x<q.const)}
    my <-f.fct(YX.vec)
    for (i in 1:n){ delta[i] <-  rbern(1,my[i])} 
    
    Y.mis <- delta*Y
    Y.mis <- ifelse(Y.mis==0, NA, Y.mis)
    
    
    
    data <- mat.or.vec(n,2)
    data[,1] <- W
    data[,2] <- Y.mis
    data.fr <-data.frame(Y.mis,W)
    LittleMCAR(data.fr)$chi.square
    test[MC,3] <- LittleMCAR(data.fr)$chi.square
    
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
    Y.mis <- ifelse(Y.mis==0, NA, Y.mis)
    
    
    data <- mat.or.vec(n,2)
    data[,1] <- W
    data[,2] <- Y.mis
    data.fr <-data.frame(Y.mis,W)
    test[MC,4] <- LittleMCAR(data.fr)$chi.square
    
}

Test.Matrix=matrix(ncol=4,nrow=N)
for (l in 1:4){  for (MC in 1:N){ Test.Matrix[MC,l]=as.integer(test[MC,l]>= qchisq(.95, 1))}}
Sign.Level=matrix(ncol=4,nrow=1)
for(l in 1:4) Sign.Level[1,l] =mean(Test.Matrix[,l])

test.mat[w.int,] = Sign.Level
}
