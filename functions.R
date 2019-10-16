##### Functions #####

### function for Monte Carlo results:

mc.se <- function(x,k=k){
  ave_est <-  mean(x)
  emp_sd <-  sd(x)
  bias_mcse <- emp_sd / sqrt(k)
  std_bias <-  ave_est / emp_sd
  return(c(bias_mcse,std_bias))
}



normalize.log <- function(x){
  if (any(x == 0, na.rm=T) == TRUE){
    cat("Warning: Zero values are recoded to 1")
    (log(1+x) - log(mean(1+x, na.rm = T)))/log(sd(1+x, na.rm =T))
  } else {
    (log(x) - log(mean(x, na.rm = T)))/log(sd(x, na.rm =T))
  }
}

mean_or_mode <- function(y){
  if (is.numeric(y)){
    mean(y, na.rm=T)
  } else if (is.factor(y)){
    names(sort(-table(y)))[1]
  }
}

#generate MCAR data from full dataset
make.mcar <- function(data, miss.prob=miss.prob, cond = NULL){
  data1 <- select(data, one_of(cond))  
  for (i in 1: ncol(data1)){
    if (is.numeric(data1[,i])){ 
      n1 <- nrow(data1) - nrow(data1[data1[,i] != -99,])
      n2 <- nrow(data1[data1[,i] != -99,])
      p1 <- 0
      p2 <- miss.prob
      n <- ifelse(data1[,i] == -99, n1, n2)
      p <- ifelse(data1[,i] == -99, p1, p2)
      data1[rbinom(n, 1, 1-p) == 0,i] <- NA
    } else {
      n1 <- nrow(data1) - nrow(data1[data1[,i] != -2,])
      n2 <- nrow(data1[data1[,i] != -2,])
      p1 <- 0
      p2 <- miss.prob
      n <- ifelse(data1[,i] == -2, n1, n2)
      p <- ifelse(data1[,i] == -2, p1, p2)
      data1[rbinom(n, 1, 1-p) == 0,i] <- NA
    }  
  }
  data <- cbind(select(data, -cond),data1)
  return(data)
}


#generate MAR data from full dataset

make.mar <- function(data, miss.prob=miss.prob, cond = NULL, x.vars = NULL){
  data1 <- select(data, one_of(cond, x.vars))  
  f <- function(t) {            # Define a path through parameter space
    sapply(t, function(y) mean(1 / (1 + exp(-y -x %*% beta))))
  }
  for (i in 1: length(cond)){
    if (is.numeric(data1[,cond[i]])){ 
      #define formula for each of the missing dependent vars
      frmla <- as.formula(paste(cond[i], paste(x.vars[1:length(x.vars)], sep = "", 
                                               collapse = " + "), sep = " ~ "))
      x <- sapply(select(data1[data1[,cond[i]] != -99,], one_of(x.vars)), as.numeric)
      reg <- lm(frmla, data = as.data.frame(sapply(data1[data1[,cond[i]] != -99,], as.numeric)))
      beta <- reg$coefficients[-1]  # Fix the coefficients through regression
      if (sum(is.na(beta)) > 0){
        beta <- na.omit(beta)
        small.names <- names(beta)
        x <- x[,small.names] 
      } else {
        results <- sapply(miss.prob, function(miss.prob) {
          alpha <- uniroot(function(t) f(t) - miss.prob, c(-1e6, 1e6), tol = .Machine$double.eps^0.5)$root
          c(alpha, f(alpha))})
        dimnames(results) <- list(c("alpha", "f(alpha)"), p=miss.prob)
      }
      # Find parameters (alpha, beta) yielding any specified proportions `p`.
      
      beta <- c(results[1,1], beta)
      ones <- rep(1,nrow(x))
      x.c <- cbind(ones,x)
      
      mod <- as.numeric(beta %*% t(x.c)) 
      rp <- exp(mod) / (exp(mod) + 1) # Suppress values between 0 and 1 via inverse-logit
      
      n1 <- nrow(data1) - nrow(x)
      n2 <- nrow(x)
      p1 <- 0
      p2 <- rp
      n <- ifelse(data1[,cond[i]] == -99, n1, n2)
      p <- ifelse(data1[,cond[i]] == -99, p1, p2)
      data1[rbinom(n, 1, p) == 1,cond[i]] <- NA
      
    } else {
      frmla <- as.formula(paste(cond[i], paste(x.vars[1:length(x.vars)], sep = "", 
                                               collapse = " + "), sep = " ~ "))
      x <- sapply(select(data1[data1[,cond[i]] != -2,], one_of(x.vars)), as.numeric)
      reg <- lm(frmla, data = as.data.frame(sapply(data1[data1[,cond[i]] != -2,], as.numeric)))
      beta <- reg$coefficients[-1]  # Fix the coefficients through regression
      if (sum(is.na(beta)) > 0){
        beta <- na.omit(beta)
        small.names <- names(beta)
        x <- x[,small.names] 
      } else {
        results <- sapply(miss.prob, function(miss.prob) {
          alpha <- uniroot(function(t) f(t) - miss.prob, c(-1e6, 1e6), tol = .Machine$double.eps^0.5)$root
          c(alpha, f(alpha))})
        dimnames(results) <- list(c("alpha", "f(alpha)"), p=miss.prob)
      }
      # Find parameters (alpha, beta) yielding any specified proportions `p`.
      
      beta <- c(results[1,1], beta)
      ones <- rep(1,nrow(x))
      x.c <- cbind(ones,x)
      
      mod <- as.numeric(beta %*% t(x.c)) 
      rp <- exp(mod) / (exp(mod) + 1) # Suppress values between 0 and 1 via inverse-logit
      
      n1 <- nrow(data1) - nrow(x)
      n2 <- nrow(x)
      p1 <- 0
      p2 <- rp
      n <- ifelse(data1[,cond[i]] == -2, n1, n2)
      p <- ifelse(data1[,cond[i]] == -2, p1, p2)
      data1[rbinom(n, 1, p) == 1,cond[i]] <- NA
    }
  }
  data <- cbind(select(data, -cond, -x.vars),data1)
  return(data)
}




#generate MNAR data from full dataset
make.mnar <- function(data, miss.prob=miss.prob, cond = NULL){
  data1 <- select(data, one_of(cond))  
  for (i in 1: ncol(data1)){
    if (is.numeric(data1[,i])){ 
      data1[data1[,i] != -99,i][data1[data1[,i] != -99,i] > median(data1[data1[,i] != -99,i])] = ifelse(sample(c(T, F),
                                                                                                               length(data1[data1[,i] != -99,i][data1[data1[,i] != -99,i] > median(data1[data1[,i] != -99,i])]),
                                                                                                               replace=T, prob=c(miss.prob*2, 1-miss.prob*2)),
                                                                                                        NA,
                                                                                                        data1[data1[,i] != -99,i][data1[data1[,i] != -99,i] > median(data1[data1[,i] != -99,i])])
    } else if (is.ordered(data1[,i])) {
      n1 <- nrow(data1[data1[,i] == -2,])
      n2 <- nrow(data1[data1[,i] != -2,])
      n <- ifelse(data1[,i] == -2, n1, n2)
      p1 <- 0
      p2 <- miss.prob/((sum(data1[,i] != max(data1[,i]) & data1[,i] != -2)/n2) + 2*(sum(data1[,i] == max(data1[,i]) & data1[,i] != -2)/n2))
      p3 <- 2*p2 ### weight p2 and p3 by the percentage of occurance of each cell 
      p <- ifelse(data1[,i] == -2, p1, ifelse(data1[,i] == max(data1[,i]), p3, p2))
      r  <- rbinom(n, size = 1, prob=p)
      data1[r==1,i] <- NA
    } else {
      n1 <- nrow(data1[data1[,i] == -2,])
      n2 <- nrow(data1[data1[,i] != -2,])
      n <- ifelse(data1[,i] == -2, n1, n2)
      p1 <- 0
      p2 <- miss.prob/((sum(as.numeric(data1[,i]) != max(as.numeric(data1[,i])) & data1[,i] != -2)/n2) + 2*(sum(as.numeric(data1[,i]) == max(as.numeric(data1[,i])) & data1[,i] != -2)/n2))
      p3 <- 2*p2 ### weight p2 and p3 by the percentage of occurance of each cell 
      p <- ifelse(data1[,i] == -2, p1, ifelse(as.numeric(data1[,i]) == max(as.numeric(data1[,i])), p3, p2))
      r  <- rbinom(n, size = 1, prob=p)
      data1[r==1,i] <- NA    }
  }  
  data <- cbind(select(data, -cond),data1)
  return(data)
}

##### Reverse arcs #####

#reversible.arcs just changing the arcs direction 
arc.reversal <- function(object=object){
  arcs <- reversible.arcs(object)
  sum <- NULL
  for (i in 1:nrow(arcs)){
    if (reliability[arcs[i,1]] > reliability[arcs[i,2]]){
      #print("potentially reverse here")
      object <- reverse.arc(object, arcs[i,1], arcs[i,2], check.cycles = F, check.illegal = TRUE, debug = F)
      sum[i] <- ifelse(i>0,1,0)
    } 
  }
  print(paste0("Number of arcs redirected: ",sum(sum, na.rm = T)))
  #return(object)
} 

#CPDAG approach with producing bn object
arc.reversal2 <- function(object = object){
  obj <- cpdag(object)
  
  arcsdir <- directed.arcs(obj)
  arcsundir <- undirected.arcs(obj)
  arcs <- matrix(nrow = nrow(arcsundir), ncol = 2)
  
  for (i in 1:nrow(arcsundir)){
    if (reliability[arcsundir[i,1]] == reliability[arcsundir[i,2]]){ 
      arcs[i,] <- arcsundir[i,]
    }
  }  
  arcs <- na.omit(arcs)
  for (i in 1:nrow(arcs)){
    arcs <- try(arcs[!(grepl(paste0("^",arcs[i,2],"$"), arcs[,1]) & grepl(paste0("^",arcs[i,1],"$"), arcs[,2])),],silent =T)
  }
  
  arcs1 <- matrix(nrow = nrow(arcsundir), ncol = 2)
  for (i in 1:nrow(arcsundir)){
    if (reliability[arcsundir[i,1]] < reliability[arcsundir[i,2]]){ 
      arcs1[i,] <- arcsundir[i,]
    }
  }  
  arcs1 <- na.omit(arcs1)
  arcsfinal <- rbind(arcsdir,arcs,arcs1)
  object$arcs <- arcsfinal
  return(object)
  
  #logic2 <- NULL
  #for (i in 1:nrow(arcsfinal)){
  #  logic2[i] <- a[arcsfinal[i,1]] <= a[arcsfinal[i,2]]
  #}
}


############################################################################################################ 
############################################################################################################ 
############################################################################################################ 
############################################################################################################ 
############################################################################################################ 

#### BN-imputation by parents function ####

bn.parents.imp <- function(bn=bn, dag=dag, dat=dat, seed = NULL){
  set.seed(seed)
  rel_label <- miss_var_summary(dat[,names(dat) != "pid"], order = T)
  reliability <- rel_label$pct_miss
  names(reliability) <- rel_label$variable
  imp.reliability <- sort(reliability[reliability>0], decreasing = F)
  
  for (k in 1:length(imp.reliability)){
    parents <- bnlearn::parents(dag, names(imp.reliability)[k])
    if (length(parents) == 0){
      data <- dat[is.na(dat[,names(imp.reliability[k])]),]
      test <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = T, method = "lw"))
      testsample <- dplyr::sample_n(na.omit(test), nrow(data), replace = T)
    } else {
      data <- dat[is.na(dat[,names(imp.reliability[k])]),parents]
      data <- as.data.frame(data) 
      test <- NULL
      for (j in 1:nrow(data)){
        if (ncol(data)>1){
          data1 <- data[j,]
          data2 <- as.data.frame(data1[,!apply(data1,2,function(x) any(is.na(x)))])
          names(data2) <- colnames(data1)[apply(!is.na(data1), 2, any)]
          if (ncol(data2)==0){
            test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = T, method = "lw"))
          } else if (ncol(data2)>1){
            listtest <- setNames(lapply(1:ncol(data2), function(i) data2[,i]), nm=names(data2))
            test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
          } else {
            listtest <- setNames(lapply(1:ncol(data2), function(i) data2[,i]), nm=names(data2))
            test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
          }
        } else {
          names(data) <- parents[1]
          if (is.na(data[j,])){
            test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = T, method = "lw"))
          } else {
            listtest <- setNames(list(parents = as.character(data[j,])), nm = names(data))
            test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
          }
        }
      }
      testsample <- sapply(seq_along(test), function(x) sample(na.omit(test[[x]]), 1))
    }
    data[names(imp.reliability)[k]] <- testsample
    dat[,names(imp.reliability[k])][is.na(dat[,names(imp.reliability[k])])] <- data[,names(imp.reliability)[k]]
  }
  return(dat)
}

############################################################################################################ 
############################################################################################################ 
# BN Reliability Chain imputation:

bnrc.imp <- function(bn=bn, dat=dat, cnt.break = cnt.break, returnfull = TRUE, seed = NULL){
  set.seed(seed)
  # prepare
  rel_label <- miss_var_summary(dat[,names(dat) != "pid"], order = T)
  reliability <- rel_label$pct_miss
  names(reliability) <- rel_label$variable
  reliability <- sort(reliability[reliability > 0])
  
  # retrieve vector of means for reliability variables:
  imp.mean <- setNames(lapply(dat[,names(reliability)], mean_or_mode), nm=names(reliability))
  # start chain
  mb <- setNames(lapply(1:length(reliability), function(k) bnlearn::mb(bn,names(reliability[k]))), nm=names(reliability))
  
  ##begin with the first variable always cycle through here
  for (i in 1:length(reliability)){
    dat_mi <- as.data.frame(dat[is.na(dat[,names(reliability[i])]),mb[[i]]])
    colnames(dat_mi) <- mb[[i]]
    if (sum(is.na(dat_mi)) > 0){
      for (m in 1:ncol(dat_mi)){  
        dat_mi[is.na(dat_mi[,mb[[i]][m]]),mb[[i]][m]] <- imp.mean[[mb[[i]][m]]]
      }
    }
    listtest <- setNames(lapply(1:nrow(dat_mi), function(r)
      setNames(lapply(1:ncol(dat_mi), function(i) dat_mi[r,i]), nm=names(dat_mi))), nm=names(dat_mi))
    test <- mclapply(mc.cores = numCores, 1:nrow(dat_mi), function(r) 
      bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listtest[[r]], method = "lw"))
    
    testsample <- sapply(seq_along(test), function(x) sample(na.omit(test[[x]][[1]]), 1))
    dat_mi[names(reliability)[i]] <- testsample
    dat[,names(reliability[i])][is.na(dat[,names(reliability[i])])] <- dat_mi[,names(reliability)[i]]
  }
  ### begin first iteration after initiation:
  dat.store <- list()
  dat.store[[1]] <- dat
  count <- 1
  cnt.break <- cnt.break
  repeat {
    if (count>cnt.break){
      break
    }
    for (i in 1:length(reliability)){
      dat[,names(reliability)[i]] <- mi.multiple.imp$MCAR[[1]][[1]][,names(reliability)[i]] 
      dat_mi <- as.data.frame(dat[is.na(dat[,names(reliability[i])]),mb[[i]]])
      colnames(dat_mi) <- mb[[i]]
      listtest <- setNames(lapply(1:nrow(dat_mi), function(r)
        setNames(lapply(1:ncol(dat_mi), function(i) dat_mi[r,i]), nm=names(dat_mi))), nm=1:nrow(dat_mi))
      test <- mclapply(mc.cores = numCores,1:nrow(dat_mi), function(r) 
        tryCatch({
          bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listtest[[r]], method = "lw")
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
      if (sum(sapply(test, is.null))>0){
        test.null <- which(sapply(test, is.null))
        test[[test.null]] <- dat.store[[count]][is.na(dat[,names(reliability[i])]),names(reliability[i])][test.null] 
      }
      testsample <- sapply(seq_along(test), function(x) sample(na.omit(test[[x]][[1]]), 1))
      dat_mi[names(reliability)[i]] <- testsample
      dat[,names(reliability[i])][is.na(dat[,names(reliability[i])])] <- dat_mi[,names(reliability)[i]]
    }
    count <- count + 1
    dat.store[[count]] <- dat
  }
  if (returnfull == FALSE){
    return(dat)
  } else if (returnfull == T) {
    datalist <- list("finalData" = dat, "DataHistory" = dat.store)
    return(datalist)
  }
}

#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
##### Processing functions post simulation #####

ks.list <- function(data=data){
            results <- setNames(mclapply(mc.cores = numCores, 1:length(miss.mech.vec), function(m)
                          setNames(lapply(seq_along(miss.prob), function(p) 
                            sapply(1:k,
                              function(l) sapply(1:length(continuous.imp.vars),
                               function(i) 
                                ks.test(data[[m]][[p]][[l]][,continuous.imp.vars[i]],truth[,continuous.imp.vars[i]])$statistic))),
                                  nm= names(miss.prob))), nm = miss.mech.vec)
            
            results <- setNames(lapply(1:length(miss.mech.vec), function(m)
                        setNames(lapply(seq_along(miss.prob), function(p) 
                          as.data.frame(t(results[[m]][[p]]))),
                           nm= names(miss.prob))), nm = miss.mech.vec)
            for (m in 1:length(miss.mech.vec)){ 
              for (p in 1:length(miss.prob)){
                colnames(results[[m]][[p]]) <- continuous.imp.vars
              }
            }  
            return(results)
}     

misclass.error <- function(data=data){
  results <- setNames(lapply(1:length(miss.mech.vec), function(m)
               setNames(lapply(seq_along(miss.prob), function(p) 
                sapply(1:k,
                  function(l) sapply(1:length(discrete.imp.vars),
                    function(i) 
                      1-sum(diag(table(data[[m]][[p]][[l]][,discrete.imp.vars[i]],
                        truth[,discrete.imp.vars[i]])))/sum(table(data[[m]][[p]][[l]][,discrete.imp.vars[i]],
                          truth[,discrete.imp.vars[i]]))))),
                            nm= names(miss.prob))), nm = miss.mech.vec)
  
  results <- setNames(lapply(1:length(miss.mech.vec), function(m)
              setNames(lapply(seq_along(miss.prob), function(p) 
                as.data.frame(t(results[[m]][[p]]))),
                  nm= names(miss.prob))), nm = miss.mech.vec)
  for (m in 1:length(miss.mech.vec)){ 
    for (p in 1:length(miss.prob)){
      colnames(results[[m]][[p]]) <- discrete.imp.vars
    }
  }  
  return(results)
}

####2nd level for continuous variables:
bd.full <- function(data=data){
  results <- setNames(mclapply(mc.cores = numCores, 1:length(miss.mech.vec), function(m)
              setNames(mclapply(mc.cores=numCores, seq_along(miss.prob), function(p) 
                sapply(1:k, function(l) bd.test(x = select(data[[m]][[p]][[l]], 
                  one_of(continuous.imp.vars, discrete.imp.vars)), y = select(truth, 
                    one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)),
                     nm= names(miss.prob))), nm = miss.mech.vec)
  return(results)
}

summ.reps <- function(data=data, sum.func = mean){
  results <- setNames(lapply(1:length(miss.mech.vec), function(m)
    as.data.frame(sapply(seq_along(miss.prob), function(p)
      sapply(1:k, function(i)
        sum.func(data[[m]][[p]][1:i]))))),nm=miss.mech.vec)
  for (m in 1:length(miss.mech.vec)){
    colnames(results[[m]]) <- names(miss.prob)
  }
  return(results)
}


#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
                                    ##### Testing #####
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#

cos.F<-function(x,j){sqrt(2)*cos((j)*pi*x)}
k.fct <- function(u){as.numeric(abs(u)<=1)*(1-u^2)*3/4}

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
      b.fct <- function(i){cos.F(W, i)} 
      }
    } else if (orthonormal.basis == "hermite"){
      b.fct <- function(i){hermite(W, i)/sqrt(factorial(i))}
    } else if (orthonormal.basis == "bspline"){
      knots<- expand.knots(seq(min(W),max(W)))
      #b.fct <- function(i){bSpline(W, degree = i, knots = knots)} #Boundary.knots = range(W, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
      b.fct <- function(i){bSpline(W, degree = i, Boundary.knots = range(W, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
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
####                       TEST MAR: P(D=1|X,Y^*)=P(D=1|X)                ###############
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
        for(i in 1:m){BasWh.mat[,i] <- cos.F(range01(W), i)} 
          
        BasXh.mat=mat.or.vec(n,m)
        for(i in 1:m){BasXh.mat[,i] <- cos.F(range01(X), i)}
          }  
          
        } else if (orthonormal.basis == "hermite"){
        BasWh.mat=mat.or.vec(n,m)
        for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}
        
        BasXh.mat=mat.or.vec(n,m)
        for(i in 1:m){BasXh.mat[,i] <- hermite(X, i)/sqrt(factorial(i))}
        
        } else if (orthonormal.basis == "bspline"){
        
          knots<- expand.knots(seq(min(W),max(W)))
          b.fct <- function(i){bSpline(W, degree = i, knots = knots)} #Boundary.knots = range(W, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
          #b.fctW <- function(i){bSpline(W, degree = i, Boundary.knots = range(W, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
          #BasWh.mat <- mat.or.vec(n,m)
          BasWh.mat <- b.fctW(m)
          
          knots<- expand.knots(seq(min(X),max(X)))
          b.fct <- function(i){bSpline(X, degree = i, knots = knots)} #Boundary.knots = range(X, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
          #b.fctX <- function(i){bSpline(X, degree = i, Boundary.knots = range(X, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
          #BasXh.mat <- mat.or.vec(n,m)
          BasXh.mat <- b.fctX(m) 
        }
        #attributes(BasWh.mat) <- NULL
        #attributes(BasXh.mat) <- NULL
        BasWf.mat <- mat.or.vec(n,m^2)
        #BasWf.mat <- mat.or.vec(n,(length(as.matrix(BasWh.mat[1,]))*length(as.matrix(BasXh.mat[1,]))))
        
        for(i in seq_len(n)){BasWf.mat[i,] <- kronecker(as.matrix(BasWh.mat[i,]),as.matrix(BasXh.mat[i,]))}

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

###########################
####### Delgado Test ######
###########################

delgado <- function(data = data, missvar = "missvar", instrument = "instrument", controls = "controls"){
  if (sum(is.na(data[,instrument])) > 0) {
    print("Stop, instrument should not have missing values")
  } else if (sum(is.na(data[,controls])) > 0) {
    print("Stop, controls should also be non-missing")
  } else {  
    
    n <- length(data[,instrument])
    W <- data[,instrument]
    Y <- data[,missvar]
    X <- data[,controls]

    delta <- ifelse(is.na(data[,missvar]),0,1)
    
    # Bandwith as in DELGADO & MANTEIGA (2001), see page 1482
    h.c <- .005*n^{-1/3}
    
    X.mat <- mat.or.vec(n,n)+X    
    K.mat <- (1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)
    
    
        delta.mat <- mat.or.vec(n,n)+delta
         test.fct<- 
           function(w,x){
         sum((1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)*(delta.mat-t(delta.mat))*as.numeric(W<=w)*as.numeric(X<=x))}
    #     
    #     #This is the C_n statistic of DELGADO & MANTEIGA (2001), see page 1472
    #     test[MC,1] <- sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)
    
     # allocate space
   #  B <- 2000
  #   epsilon_hat <- rep(0, B)
     
     # perform B bootstrap repetitions
   #  for(b in 1:B){
  #     ind <- sample(1:n, n, replace = TRUE)
  #     epsilon_hat[b] <- residuals(npreg(delta[ind] ~ X[ind], bws=h.c, ckertype='gaussian', residuals=TRUE))
  #   }
     
         
    reg.object <- npreg(delta ~ X, bws=h.c, ckertype='gaussian', residuals=TRUE)
    epsilon_hat <- residuals(reg.object)
    
    
  #  f.B<- function(BI){       # hier beginnt der Simulationsloop
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
        f.xw <- function(x,w){as.numeric(X<=x)*as.numeric(W<=w)}
        
        fb <- sum((rowSums(crossprod(K.mat*(delta_star.mat-t(delta_star.mat)), mapply(f.xw,X,W))))^2)
        #fb <- sum(mapply(test.fct,W,X)^2)#sum(mapply(test_star.fct,W,X)^2)#
   #     return(fb)
    #  }
      
      test <- list("C_n**"= fb, "Critical Value"= qnorm(0.975), "Ratio"=fb/qnorm(0.975),
                   "Test decision"= if (fb > qnorm(0.975)){
                     print("We reject the Null of MAR given X")
                   } else {
                     print("We retain the Null of MAR given X")})
      return(test)
      cat(paste("The test value is",fb))
      cat(paste("While the critical value is",qnorm(0.975)))
      cat(paste("Their ratio is", fb/qnorm(0.975)))
      
      if (fb > qnorm(0.975)){
        cat("Consequently, we reject the Null of MAR given X")
      } else {
        cat("Consequently, we retain the Null of MAR given X")
      }  
  }
}






