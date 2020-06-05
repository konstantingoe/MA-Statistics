# Create latent model of the form: X = TB' + E
# with E ~ N(0,0.01) and B ~ U(-1,1)

simLatent <- function(n = 200, p = 50, k = 3){
  T <- matrix(rnorm(n * k, 0, 1), ncol = k)
  B <- matrix(runif(p * k, -1, 1), ncol = k)
  X <- T %*% t(B)
  E <- matrix(rnorm(n * p, 0, 0.1), ncol = p)
  XE <- X + E
  return(XE)
}

#simcategorical <- function(n = 200, p = 5){
#  O <- matrix(replicate(p,as.factor(sample(c(1,2,3),n,TRUE,prob = gtools::rdirichlet(1,c(.5,.5,.5))))), ncol=p)
#  return(O)
#}

simcategorical <- function(n = 200, p = 5){
  O <- matrix(replicate(p,as.factor(sample(c(1,2,3),n,TRUE,prob = c(1/3,1/3,1/3)))), ncol=p)
  return(O)
}

simdummy <- function(n = 200, p = 5){
  D <- matrix(replicate(p,as.factor(rbinom(n,1,runif(1, .4,.6)))), ncol = p)
  return(D)
}


#generate MCAR data from full dataset
make.mcar <- function(data, miss.prob=miss.prob){
  for (i in 1: ncol(data)){
    n <- nrow(data)
    p <- miss.prob
    data[rbinom(n, 1, 1-p) == 0,i] <- NA
  }
  return(data)
}

# Define a path through parameter space
f <- function(t) {            
  sapply(t, function(y) mean(1 / (1 + exp(-y -x %*% beta))))
}

#generate MAR data from full dataset

make.mar <- function(data, miss.prob=miss.prob){
  f <- function(t) {            
    sapply(t, function(y) mean(1 / (1 + exp(-y -x %*% beta))))
  }
  data1 <- sapply(data, as.numeric)
  data2 <- sapply(data, as.numeric)
  for(j in 1:ncol(data)){
    xs <- sample(names(data)[-j],30)
    x <- as.matrix(data2[,xs])
    frmla <- as.formula(paste(names(data)[j], paste(xs, sep = "", 
                                                    collapse = " + "), sep = " ~ "))
    reg <- lm(frmla, data = as.data.frame(sapply(data, as.numeric)))
    beta <- reg$coefficients[-1]    
    results <- sapply(miss.prob, function(miss.prob) {
      alpha <- uniroot(function(t) f(t) - miss.prob, c(-1e6, 1e6), tol = .Machine$double.eps^0.5)$root
      c(alpha, f(alpha))})
    dimnames(results) <- list(c("alpha", "f(alpha)"), p=miss.prob)
    
    beta <- c(results[1,1], beta)
    ones <- rep(1,nrow(x))
    x.c <- cbind(ones,x)
    
    mod <- as.numeric(beta %*% t(x.c)) 
    rp <- exp(mod) / (exp(mod) + 1) # Suppress values between 0 and 1 via inverse-logit
    
    n <- nrow(x)
    pr <- rp
    data1[rbinom(n, 1, pr) == 1,j] <- NA
  }
  return(as.data.frame(data1))
}

make.mnar <- function(data, miss.prob=miss.prob){
  f <- function(t) {            
    sapply(t, function(y) mean(1 / (1 + exp(-y -x %*% beta))))
  }
  data1 <- sapply(data, as.numeric)
  data2 <- sapply(data, as.numeric)
  for(j in 1:ncol(data)){
    xs <- c(names(data)[j],sample(names(data)[-j],29))
    x <- as.matrix(data2[,xs])
    frmla <- as.formula(paste(sample(setdiff(names(data),xs),1), paste(xs, sep = "", 
                                                                       collapse = " + "), sep = " ~ "))
    reg <- lm(frmla, data = as.data.frame(sapply(data, as.numeric)))
    beta <- reg$coefficients[-1]    
    results <- sapply(miss.prob, function(miss.prob) {
      alpha <- uniroot(function(t) f(t) - miss.prob, c(-1e6, 1e6), tol = .Machine$double.eps^0.5)$root
      c(alpha, f(alpha))})
    dimnames(results) <- list(c("alpha", "f(alpha)"), p=miss.prob)
    
    beta <- c(results[1,1], beta)
    ones <- rep(1,nrow(x))
    x.c <- cbind(ones,x)
    
    mod <- as.numeric(beta %*% t(x.c)) 
    rp <- exp(mod) / (exp(mod) + 1) # Suppress values between 0 and 1 via inverse-logit
    
    n <- nrow(x)
    pr <- rp
    data1[rbinom(n, 1, pr) == 1,j] <- NA
  }
  return(as.data.frame(data1))
}


#### Functions for creating missings for filter data:

#generate MCAR data from full dataset
make.mcar.filter <- function(data, miss.prob=miss.prob, cond = NULL){
  data1 <- dplyr::select(data, dplyr::one_of(cond))  
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
  data <- cbind(dplyr::select(data, -cond),data1)
  return(data)
}

#generate MAR data from full dataset

make.mar.filter <- function(data, miss.prob=miss.prob, cond = NULL, x.vars = NULL){
  data1 <- dplyr::select(data, dplyr::one_of(cond, x.vars))  
  f <- function(t) {            # Define a path through parameter space
    sapply(t, function(y) mean(1 / (1 + exp(-y -x %*% beta))))
  }
  for (i in 1: length(cond)){
    if (is.numeric(data1[,cond[i]])){ 
      #define formula for each of the missing dependent vars
      frmla <- as.formula(paste(cond[i], paste(x.vars[1:length(x.vars)], sep = "", 
                                               collapse = " + "), sep = " ~ "))
      x <- sapply(dplyr::select(data1[data1[,cond[i]] != -99,], dplyr::one_of(x.vars)), as.numeric)
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
      x <- sapply(dplyr::select(data1[data1[,cond[i]] != -2,], dplyr::one_of(x.vars)), as.numeric)
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
  data <- cbind(dplyr::select(data, -cond, -x.vars),data1)
  return(data)
}



#generate MNAR data from full dataset
make.mnar.filter <- function(data, miss.prob=miss.prob, cond = NULL){
  data1 <- dplyr::select(data, dplyr::one_of(cond))  
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
  data <- cbind(dplyr::select(data, -cond),data1)
  return(data)
}


##### Reverse arcs #####

#reversible.arcs just changing the arcs direction 
arc.reversal <- function(object=object, data=data){
  
  rel_label <- miss_var_summary(data, order = T)
  reliability <- rel_label$pct_miss
  names(reliability) <- rel_label$variable
  
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


##### BN imputation #####

#### BN-imputation by parents function ####

bn.parents.imp <- function(bn=bn, dag=dag, dat=dat){
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
            #listtest <- setNames(list(parents = as.character(data[j,])), nm = names(data))
            listtest <- setNames(list(parents = data[j,]), nm = names(data))
            test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
          }
        }
      }
      if (sum(sapply(test, is.null))>0){
        test.null <- which(sapply(test, is.null))
        for (t in 1:length(test.null)){
          test[test.null[t]] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = T, method = "lw"))
        }
      } else if (any(sum(sapply(test, is.nan)>0))){
        test.nan <- which(sapply(1:length(test), function(k) sum(is.nan(test[[k]])))>0)
        for (t in 1:length(test.nan)){
          test[test.nan[t]] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = T, method = "lw"))
        }
      }
      testsample <- tryCatch({sapply(seq_along(test), function(x) sample(na.omit(test[[x]]), 1))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    data[names(imp.reliability)[k]] <- testsample
    dat[,names(imp.reliability[k])][is.na(dat[,names(imp.reliability[k])])] <- data[,names(imp.reliability)[k]]
  }
  return(dat)
}

##### BNimp with Markov blanket set for imputation #####

bnrc.nomean <- function(bn=bn, data=data, cnt.break = cnt.break, returnfull = TRUE){
  # prepare
  dat <- data
  original <- dat
  rel_label <- miss_var_summary(dat[,names(dat) != "pid"], order = T)
  reliability <- rel_label$pct_miss
  names(reliability) <- rel_label$variable
  reliability <- sort(reliability[reliability > 0])
  # start chain
  mb <- setNames(lapply(1:length(reliability), function(k) bnlearn::mb(bn,names(reliability[k]))), nm=names(reliability))
  pa <- setNames(lapply(1:length(reliability), function(k) bnlearn::parents(bn,names(reliability[k]))), nm=names(reliability))
  
  ##begin with the first variable always cycle through here
  for (i in 1:length(reliability)){
    dat_mi <- as.data.frame(dat[is.na(dat[,names(reliability[i])]),mb[[i]]])
    colnames(dat_mi) <- mb[[i]]
    listtest <- lapply(1:nrow(dat_mi), function(r)
      setNames(lapply(1:ncol(dat_mi), function(j) dat_mi[r,j]), nm=names(dat_mi)))
    listtest <- lapply(listtest, function(x) x[!is.na(x)]) 
    test <- lapply(1:nrow(dat_mi), function(r) 
      tryCatch({
        bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listtest[[r]], method = "lw")
      }, error=function(e){cat("ERROR in node",names(reliability)[i]," :",conditionMessage(e), "\n")}))
    if (sum(sapply(test, is.null))>0){
      test.null <- which(sapply(test, is.null))
      for (t in 1:length(test.null)){
        if (is_empty(pa[[i]])){
          test[[test.null[t]]] <- tryCatch({
            bnlearn::cpdist(bn, nodes = names(reliability)[i], method = "lw", evidence = T)
          }, error=function(e){cat("ERROR in parent node",names(reliability)[i]," :",conditionMessage(e), "\n")})
        } else {
          dat_pa <- as.data.frame(dat[test.null[t],pa[[i]]])
          colnames(dat_pa) <- pa[[i]]
          listpa <- setNames(lapply(1:ncol(dat_pa), function(j) dat_pa[1,j]), nm=names(dat_pa))
          listpa <- Filter(Negate(anyNA), listpa)
          test[[test.null[t]]] <- tryCatch({
            bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listpa, method = "lw")
          }, error=function(e){cat("ERROR in parent node",names(reliability)[i]," :",conditionMessage(e), "\n")})
        }
      }
    } else if (any(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)){
      test.nan <- which(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)
      for (t in 1:length(test.nan)){
        test[[test.nan[t]]] <- bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = TRUE, method = "lw")
      }
    }
    testsample <- tryCatch({sapply(seq_along(test), function(x) sample(na.omit(test[[x]][[1]])[test[[x]][[1]] != -2],1))},
                           error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
    if (is.null(testsample)){
      print(paste("fatal error in initiation -- all is NULL in one of node:", names(reliability[i])))
    } else {
      dat_mi[names(reliability)[i]] <- testsample
      dat[,names(reliability[i])][is.na(dat[,names(reliability[i])])] <- dat_mi[,names(reliability)[i]]
    }
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
      dat[,names(reliability)[i]] <- original[,names(reliability)[i]] 
      dat_mi <- as.data.frame(dat[is.na(dat[,names(reliability[i])]),mb[[i]]])
      colnames(dat_mi) <- mb[[i]]
      listtest <- setNames(lapply(1:nrow(dat_mi), function(r)
        setNames(lapply(1:ncol(dat_mi), function(j) dat_mi[r,j]), nm=names(dat_mi))), nm=1:nrow(dat_mi))
      test <- lapply(1:nrow(dat_mi), function(r) 
        tryCatch({
          bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listtest[[r]], method = "lw")
        }, error=function(e){cat("ERROR in node",names(reliability)[i]," :",conditionMessage(e), "\n")}))
      if (sum(sapply(test, is.null))>0){
        test.null <- which(sapply(test, is.null))
        for (t in 1:length(test.null)){
          if (is_empty(pa[[i]])){
            test[[test.null[t]]] <- tryCatch({
              bnlearn::cpdist(bn, nodes = names(reliability)[i], method = "lw", evidence = T)
            }, error=function(e){cat("ERROR in parent node",names(reliability)[i]," :",conditionMessage(e), "\n")})
          } else {
            dat_pa <- as.data.frame(dat[test.null[t],pa[[i]]])
            colnames(dat_pa) <- pa[[i]]
            listpa <- setNames(lapply(1:ncol(dat_pa), function(j) dat_pa[1,j]), nm=names(dat_pa))
            listpa <- Filter(Negate(anyNA), listpa)
            test[[test.null[t]]] <- tryCatch({
              bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listpa, method = "lw")
            }, error=function(e){cat("ERROR in parent node",names(reliability)[i]," :",conditionMessage(e), "\n")})
          }
        }
      } else if (any(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)){
        test.nan <- which(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)
        for (t in 1:length(test.nan)){
          test[[test.nan[t]]] <- bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = TRUE, method = "lw")
        }
      }
      testsample <- tryCatch({sapply(seq_along(test), function(x) sample(na.omit(test[[x]][[1]])[test[[x]][[1]] != -2],1))},
                             error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
      if (is.null(testsample)){
        print(paste("error in evidence -- all is NULL in one of node:", names(reliability[i])))
        dat[,names(reliability[i])][is.na(dat[,names(reliability[i])])] <- dat.store[[1]][,names(reliability[i])][is.na(dat[,names(reliability[i])])]
      } else {
        dat_mi[names(reliability)[i]] <- testsample
        dat[,names(reliability[i])][is.na(dat[,names(reliability[i])])] <- dat_mi[,names(reliability)[i]]
      }
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



