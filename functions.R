##### Functions #####

### function for Monte Carlo results:

mc.se <- function(x,k=k){
  ave_est <-  mean(x)
  emp_sd <-  sd(x)
  bias_mcse <- emp_sd / sqrt(k)
  std_bias <-  ave_est / emp_sd
  return(c(bias_mcse,std_bias))
}

skip.streams <- function(n) {
  x <- .Random.seed
  for (i in seq_len(n))
    x <- nextRNGStream(x)
  assign('.Random.seed', x, pos=.GlobalEnv)
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

make.mar <- function(data, miss.prob=miss.prob, cond = NULL, x.vars = NULL){
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
make.mnar <- function(data, miss.prob=miss.prob, cond = NULL){
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
            listtest <- setNames(list(parents = as.character(data[j,])), nm = names(data))
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


############################################################################################################ 
############################################################################################################ 
# BN Reliability Chain imputation:

bnrc.imp <- function(bn=bn, data=data, cnt.break = cnt.break, returnfull = TRUE){
  # prepare
  dat <- data
  original <- dat
  rel_label <- miss_var_summary(dat[,names(dat) != "pid"], order = T)
  reliability <- rel_label$pct_miss
  names(reliability) <- rel_label$variable
  reliability <- sort(reliability[reliability > 0])
  
  # retrieve vector of means for reliability variables:
  imp.m <- list()
  naming <- NULL
  for (i in 1:length(reliability)){
    if (is.factor(dat[,names(reliability)[i]])){
      y <- dat[dat[,names(reliability)[i]] != "-2",names(reliability)[i]]
      imp.m[[i]] <- names(sort(-table(y)))[1]
      naming[i] <- names(reliability)[i]
    } else {
      y <- dat[dat[,names(reliability)[i]] != min(dat[,names(reliability)[i]], na.rm = T),names(reliability)[i]]
      imp.m[[i]] <- mean(y, na.rm = T)
      naming[i] <- names(reliability)[i]
    }
  }  
  names(imp.m) <- naming
  #imp.mean <- setNames(lapply(dat[,names(reliability)], mean_or_mode), nm=names(reliability))
  # start chain
  mb <- setNames(lapply(1:length(reliability), function(k) bnlearn::mb(bn,names(reliability[k]))), nm=names(reliability))
  
  ##begin with the first variable always cycle through here
  for (i in 1:length(reliability)){
    dat_mi <- as.data.frame(dat[is.na(dat[,names(reliability[i])]),mb[[i]]])
    colnames(dat_mi) <- mb[[i]]
    if (sum(is.na(dat_mi)) > 0){
      for (m in 1:ncol(dat_mi)){  
        dat_mi[is.na(dat_mi[,mb[[i]][m]]),mb[[i]][m]] <- imp.m[mb[[i]][m]]
      }
    }
    listtest <- lapply(1:nrow(dat_mi), function(r)
      setNames(lapply(1:ncol(dat_mi), function(i) dat_mi[r,i]), nm=names(dat_mi)))
    test <- lapply(1:nrow(dat_mi), function(r) 
      tryCatch({
        bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listtest[[r]], method = "lw")
      }, error=function(e){cat("ERROR in node",names(reliability)[i]," :",conditionMessage(e), "\n")}))
    if (sum(sapply(test, is.null))>0){
      test.null <- which(sapply(test, is.null))
      for (t in 1:length(test.null)){
        test[[test.null[t]]] <- bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = TRUE, method = "lw")
      }
    } else if (any(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)){
      test.nan <- which(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)
      for (t in 1:length(test.nan)){
        test[[test.nan[t]]] <- bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = TRUE, method = "lw")
      }
    }
    testsample <- tryCatch({sapply(seq_along(test), function(x) sample(na.omit(test[[x]][[1]])[test[[x]][[1]] != -2], 1))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
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
            setNames(lapply(1:ncol(dat_mi), function(i) dat_mi[r,i]), nm=names(dat_mi))), nm=1:nrow(dat_mi))
          test <- lapply(1:nrow(dat_mi), function(r) 
            tryCatch({
              bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listtest[[r]], method = "lw")
            }, error=function(e){cat("ERROR in node",names(reliability)[i]," :",conditionMessage(e), "\n")}))
          
          if (sum(sapply(test, is.null))>0){
            test.null <- which(sapply(test, is.null))
            for (t in 1:length(test.null)){
              test[[test.null[t]]] <- bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = TRUE, method = "lw")
            }
          } else if (any(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)){
            test.nan <- which(sapply(1:length(test), function(k) sum(is.nan(test[[k]][[1]])))>0)
            for (t in 1:length(test.nan)){
              test[[test.nan[t]]] <- bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = TRUE, method = "lw")
            }
          }
          testsample <- tryCatch({sapply(seq_along(test), function(x) sample(na.omit(test[[x]][[1]])[test[[x]][[1]] != -2], 1))
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
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
        dat_pa <- as.data.frame(dat[test.null[t],pa[[i]]])
        colnames(dat_pa) <- pa[[i]]
        listpa <- setNames(lapply(1:ncol(dat_pa), function(j) dat_pa[1,j]), nm=names(dat_pa))
        listpa <- lapply(listpa, function(x) x[!is.na(x)]) 
        test[[test.null[t]]] <- tryCatch({
          bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listpa, method = "lw")
        }, error=function(e){cat("ERROR in node",names(reliability)[i]," :",conditionMessage(e), "\n")})
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
          dat_pa <- as.data.frame(dat[test.null[t],pa[[i]]])
          colnames(dat_pa) <- pa[[i]]
          listpa <- setNames(lapply(1:ncol(dat_pa), function(j) dat_pa[1,j]), nm=names(dat_pa))
          listpa <- lapply(listpa, function(x) x[!is.na(x)]) 
          test[[test.null[t]]] <- tryCatch({
            bnlearn::cpdist(bn, nodes = names(reliability)[i], evidence = listpa, method = "lw")
          }, error=function(e){cat("ERROR in parent node",names(reliability)[i]," :",conditionMessage(e), "\n")})
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
  results <- setNames(lapply(c(1,3,2), function(m)
               setNames(lapply(seq_along(miss.prob), function(p) 
                sapply(1:k,
                  function(l) sapply(1:length(discrete.imp.vars),
                    function(i) 
                      1-sum(diag(table(data[[m]][[p]][[l]][,discrete.imp.vars[i]],
                        truth[,discrete.imp.vars[i]])))/sum(table(data[[m]][[p]][[l]][,discrete.imp.vars[i]],
                          truth[,discrete.imp.vars[i]]))))),
                            nm= names(miss.prob))), nm = c("MCAR", "MAR", "MNAR"))
  
  results <- setNames(lapply(c(1,3,2), function(m)
              setNames(lapply(seq_along(miss.prob), function(p) 
                as.data.frame(t(results[[m]][[p]]))),
                  nm= names(miss.prob))), nm = c("MCAR", "MAR", "MNAR"))
  for (m in c(1,3,2)){ 
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
                sapply(1:k, function(l) bd.test(x = dplyr::select(data[[m]][[p]][[l]], 
                  dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth, 
                    dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)),
                     nm= names(miss.prob))), nm = miss.mech.vec)
  return(results)
}

summ.reps <- function(data=data, sum.func = mean){
  results <- setNames(lapply(c(1,3,2), function(m)
    as.data.frame(sapply(seq_along(miss.prob), function(p)
      sapply(1:k, function(i)
        sum.func(data[[m]][[p]][1:i]))))),nm=c("MCAR", "MAR", "MNAR"))
  for (m in c(1,3,2)){
    colnames(results[[m]]) <- names(miss.prob)
  }
  return(results)
}


#### for continuous vars:
make.lvl1.table <- function(which.mc.error="MCE"){
  level1.table <-   setNames(lapply(seq_along(miss.prob), function(p)
    sapply(seq_along(table.imp), function(i)
      sapply(table.imp[[i]][[p]][[1]], mean, na.omit =T))),nm=names(miss.prob))
  for (p in 1:length(miss.prob)){
    for (m in 2:3){
      level1.table[[p]] <- cbind(level1.table[[p]], 
                                 setNames(lapply(seq_along(miss.prob), function(p)  
                                   sapply(seq_along(table.imp), function(i)
                                     sapply(table.imp[[i]][[p]][[m]], mean, na.omit =T))),nm=names(miss.prob))[[p]])
    }
  }  
  
  for (p in 1:length(miss.prob)){
    colnames(level1.table[[p]]) <- rep(names(table.imp),3)
    level1.table[[p]] <- round(level1.table[[p]], digits = 4)
  }
  
  level1.table.sd <-   setNames(lapply(seq_along(miss.prob), function(p)
    sapply(seq_along(table.imp), function(i)
      sapply(table.imp[[i]][[p]][[1]], sd, na.omit =T))),nm=names(miss.prob))
  
  if (which.mc.error=="MCE"){
    for (p in 1:length(miss.prob)){
      for (m in 2:3){
        level1.table.sd[[p]] <- cbind(level1.table.sd[[p]], 
                                      setNames(lapply(seq_along(miss.prob), function(p)  
                                        sapply(seq_along(table.imp), function(i)
                                          sapply(table.imp[[i]][[p]][[m]], sd, na.omit =T))),nm=names(miss.prob))[[p]])
      }
    }  
  } else if (which.mc.error=="MCVar"){
    for (p in 1:length(miss.prob)){
      for (m in 2:3){
        level1.table.sd[[p]] <- cbind(level1.table.sd[[p]], 
                                      setNames(lapply(seq_along(miss.prob), function(p)  
                                        sapply(seq_along(table.imp), function(i)
                                          sapply(table.imp[[i]][[p]][[m]], var, na.omit =T))),nm=names(miss.prob))[[p]])
      }
    }   
  }
  
  for (p in 1:length(miss.prob)){
    colnames(level1.table.sd[[p]]) <- rep(names(table.imp),3)
    level1.table.sd[[p]] <- apply(round(level1.table.sd[[p]], digits = 4), 2, function(i) paste("(", i, ")", sep=""))
  }
  
  output <- list("0,1"=NULL,"0.2"=NULL,"0.3" = NULL)
  for (p in 1:length(miss.prob)){
    table.names <- NULL
    table.empty <- rep("",length(continuous.imp.vars))
    for (i in 1:nrow(level1.table[[p]])){
      output[[p]] <- rbind(output[[p]], level1.table[[p]][i,])
      table.names <- c(table.names,continuous.imp.vars[i])
      output[[p]] <- rbind(output[[p]], level1.table.sd[[p]][i,])
      table.names <- c(table.names,table.empty[i])
    }
    rownames(output[[p]]) <- table.names
  }
  return(output)
}

######other table proposal:

mcarlvltbl <- function(missmech = missmech){
  if (missmech == "MCAR"){
    miss <- 1
  } else if (missmech == "MAR"){
    miss <- 3
  } else if (missmech == "MNAR"){
    miss <- 2
  }
    
    mcarlvl1 <- cbind(cbind(sapply(lvl1.bn[[miss]]$`0.1`, mean), sapply(lvl1.bnrc_nm[[miss]]$`0.1`, mean), sapply(lvl1.mice[[miss]]$`0.1`, mean)),
                      cbind(sapply(lvl1.bn[[miss]]$`0.2`, mean), sapply(lvl1.bnrc_nm[[miss]]$`0.2`, mean), sapply(lvl1.mice[[miss]]$`0.2`, mean)),
                      cbind(sapply(lvl1.bn[[miss]]$`0.3`, mean), sapply(lvl1.bnrc_nm[[miss]]$`0.3`, mean), sapply(lvl1.mice[[miss]]$`0.3`, mean)))
    mcarlvl1 <- round(mcarlvl1, digits = 4)
    
    mcarlvl1sd <- cbind(cbind(sapply(lvl1.bn[[miss]]$`0.1`, sd), sapply(lvl1.bnrc_nm[[miss]]$`0.1`, sd), sapply(lvl1.mice[[miss]]$`0.1`, sd)),
                        cbind(sapply(lvl1.bn[[miss]]$`0.2`, sd), sapply(lvl1.bnrc_nm[[miss]]$`0.2`, sd), sapply(lvl1.mice[[miss]]$`0.2`, sd)),
                        cbind(sapply(lvl1.bn[[miss]]$`0.3`, sd), sapply(lvl1.bnrc_nm[[miss]]$`0.3`, sd), sapply(lvl1.mice[[miss]]$`0.3`, sd)))
    
    mcarlvl1sd <- apply(round(mcarlvl1sd, digits = 4), 2, function(i) paste("(", i, ")", sep=""))
    
    
    powermcar <- cbind(cbind(sapply(as.data.frame(lvl1.bn[[miss]]$`0.1` < .04334), sum)/k, sapply(as.data.frame(lvl1.bnrc_nm[[miss]]$`0.1` < .04334), sum)/k, sapply(as.data.frame(lvl1.mice[[miss]]$`0.1` < .04334), sum)/k),
                       cbind(sapply(as.data.frame(lvl1.bn[[miss]]$`0.2` < .04334), sum)/k, sapply(as.data.frame(lvl1.bnrc_nm[[miss]]$`0.2` < .04334), sum)/k, sapply(as.data.frame(lvl1.mice[[miss]]$`0.2` < .04334), sum)/k),
                       cbind(sapply(as.data.frame(lvl1.bn[[miss]]$`0.3` < .04334), sum)/k, sapply(as.data.frame(lvl1.bnrc_nm[[miss]]$`0.3` < .04334), sum)/k, sapply(as.data.frame(lvl1.mice[[miss]]$`0.3` < .04334), sum)/k))
    powermcar <- as.matrix(sapply(as.data.frame(powermcar), format, digits=4, nsmall=4))
    
    output <- NULL
    v <- 1
    for (i in 1:nrow(mcarlvl1)){
      output <- rbind(output,mcarlvl1[i,])
      rownames(output)[v] <- rownames(mcarlvl1)[i]
      output <- rbind(output, mcarlvl1sd[i,])
      rownames(output)[v+1] <- rownames(mcarlvl1)[i]
      output <- rbind(output, powermcar[i,])
      rownames(output)[v+2] <- rownames(mcarlvl1)[i]
      v <- v + 3
    }
     colnames(output) <- rep(c("BN_Pi","BN_MBRC", "MICE"),3)
return(output)
}


#### for discrete vars:

make.lvl1.misclass.table <- function(){
  lvl1.table10 <- bind_cols(as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[1]][[1]] ,mean)))),
                            as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[2]][[1]] ,mean)))),
                            as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[3]][[1]] ,mean)))))
  lvl1.table20 <- bind_cols(as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[1]][[2]] ,mean)))),
                            as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[2]][[2]] ,mean)))),
                            as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[3]][[2]] ,mean)))))
  lvl1.table30 <- bind_cols(as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[1]][[3]] ,mean)))),
                            as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[2]][[3]] ,mean)))),
                            as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[3]][[3]] ,mean)))))
  
  lvl1.tablemean <- bind_rows(lvl1.table10,lvl1.table20,lvl1.table30)
  lvl1.tablemean <- as.matrix(round(lvl1.tablemean, digits = 4))
  
  lvl1.table10sd <- bind_cols(as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[1]][[1]] ,sd)))),
                              as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[2]][[1]] ,sd)))),
                              as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[3]][[1]] ,sd)))))
  lvl1.table20sd <- bind_cols(as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[1]][[2]] ,sd)))),
                              as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[2]][[2]] ,sd)))),
                              as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[3]][[2]] ,sd)))))
  lvl1.table30sd <- bind_cols(as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[1]][[3]] ,sd)))),
                              as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[2]][[3]] ,sd)))),
                              as.data.frame(sapply(1:3, function(i) cbind(sapply(table.disc.imp[[i]][[3]][[3]] ,sd)))))
  
  lvl1.tablesd <- bind_rows(lvl1.table10sd,lvl1.table20sd,lvl1.table30sd)
  lvl1.tablesd <- apply(round(lvl1.tablesd, digits = 4), 2, function(i) paste("(", i, ")", sep=""))
  
  output <- NULL
  for (i in 1:nrow(lvl1.tablemean)){
    output <- rbind(output,lvl1.tablemean[i,])
    output <- rbind(output, lvl1.tablesd[i,])
  }
  rownames(output) <- rep(c(rep("Education", 2),rep("Superior", 2),rep("Comp. size", 2)),3)
  colnames(output) <- rep(c("BN_Pi","BN_MBRC", "MICE"),3)
  return(output)
}

### extract bd statistic and corresponding p-value from output
# a_n = a_1 + d(n-1), d = 3

lvl2.extract <- function(data=data){
  bd.list <- list("MCAR" = list("0.1" = NULL, "0.2" = NULL, "0.3" = NULL), "MNAR" = list("0.1" = NULL, "0.2" = NULL, "0.3" = NULL), "MAR" = list("0.1" = NULL, "0.2" = NULL, "0.3" = NULL))
  p.list <-  list("MCAR" = list("0.1" = NULL, "0.2" = NULL, "0.3" = NULL), "MNAR" = list("0.1" = NULL, "0.2" = NULL, "0.3" = NULL), "MAR" = list("0.1" = NULL, "0.2" = NULL, "0.3" = NULL))
  for (m in 1:length(miss.mech.vec)){
    for (p in 1:length(miss.prob)){
      v <- 1
      w <- 2
      for (i in 1:k){
        bd.list[[m]][[p]][i] <- data[[m]][[p]][[v]]
        p.list[[m]][[p]][i] <- data[[m]][[p]][[w]]
        v <- 1 + 3*(i-1)
        w <- 2 + 3*(i-1)
      }
    }
  }
  return(list("bd"= bd.list, "p"=p.list))
}

make.lvl2.table <- function(){
    lvl2bn.mean <- c(sapply(seq_along(miss.prob), function(p) mean(lvl2.bn.bd$bd[[1]][[p]])),
                     sapply(seq_along(miss.prob), function(p) mean(lvl2.bn.bd$bd[[3]][[p]])),
                     sapply(seq_along(miss.prob), function(p) mean(lvl2.bn.bd$bd[[2]][[p]])))
    lvl2bn.sd <- c(sapply(seq_along(miss.prob), function(p) sd(lvl2.bn.bd$bd[[1]][[p]])),
                   sapply(seq_along(miss.prob), function(p) sd(lvl2.bn.bd$bd[[3]][[p]])),
                   sapply(seq_along(miss.prob), function(p) sd(lvl2.bn.bd$bd[[2]][[p]])))
    lvl2bn.power <- c(sapply(seq_along(miss.prob), function(p) sum(lvl2.bn.bd$p[[1]][[p]] > .05)/k),
                      sapply(seq_along(miss.prob), function(p)  sum(lvl2.bn.bd$p[[3]][[p]] > .05)/k),
                      sapply(seq_along(miss.prob), function(p)  sum(lvl2.bn.bd$p[[2]][[p]] > .05)/k))
    
    lvl2bn.table <- as.data.frame(round(rbind(lvl2bn.mean,lvl2bn.sd, lvl2bn.power), digits = 5))
    lvl2bn.table[2,] <- apply(round(lvl2bn.table[2,], digits = 5), 2, function(i) paste("(", i, ")", sep=""))
    colnames(lvl2bn.table) <- c(rep("MCAR",3), rep("MAR",3), rep("MNAR",3))
    
    lvl2bnrc.mean <- c(sapply(seq_along(miss.prob), function(p) mean(lvl2.bnrc.bd$bd[[1]][[p]])),
                       sapply(seq_along(miss.prob), function(p) mean(lvl2.bnrc.bd$bd[[3]][[p]])),
                       sapply(seq_along(miss.prob), function(p) mean(lvl2.bnrc.bd$bd[[2]][[p]])))
    lvl2bnrc.sd <- c(sapply(seq_along(miss.prob), function(p) sd(lvl2.bnrc.bd$bd[[1]][[p]])),
                     sapply(seq_along(miss.prob), function(p) sd(lvl2.bnrc.bd$bd[[3]][[p]])),
                     sapply(seq_along(miss.prob), function(p) sd(lvl2.bnrc.bd$bd[[2]][[p]])))
    lvl2bnrc.power <- c(sapply(seq_along(miss.prob), function(p) sum(lvl2.bnrc.bd$p[[1]][[p]] > .05)/k),
                        sapply(seq_along(miss.prob), function(p)  sum(lvl2.bnrc.bd$p[[3]][[p]] > .05)/k),
                        sapply(seq_along(miss.prob), function(p)  sum(lvl2.bnrc.bd$p[[2]][[p]] > .05)/k))
    
    lvl2bnrc.table <- as.data.frame(round(rbind(lvl2bnrc.mean,lvl2bnrc.sd, lvl2bnrc.power), digits = 5))
    lvl2bnrc.table[2,] <- apply(round(lvl2bnrc.table[2,], digits = 5), 2, function(i) paste("(", i, ")", sep=""))
    colnames(lvl2bnrc.table) <- c(rep("MCAR",3), rep("MAR",3), rep("MNAR",3))
    
    lvl2mice.mean <- c(sapply(seq_along(miss.prob), function(p) mean(lvl2.mice.bd$bd[[1]][[p]])),
                       sapply(seq_along(miss.prob), function(p) mean(lvl2.mice.bd$bd[[3]][[p]])),
                       sapply(seq_along(miss.prob), function(p) mean(lvl2.mice.bd$bd[[2]][[p]])))
    lvl2mice.sd <- c(sapply(seq_along(miss.prob), function(p) sd(lvl2.mice.bd$bd[[1]][[p]])),
                     sapply(seq_along(miss.prob), function(p) sd(lvl2.mice.bd$bd[[3]][[p]])),
                     sapply(seq_along(miss.prob), function(p) sd(lvl2.mice.bd$bd[[2]][[p]])))
    lvl2mice.power <- c(sapply(seq_along(miss.prob), function(p) sum(lvl2.mice.bd$p[[1]][[p]] > .05)/k),
                        sapply(seq_along(miss.prob), function(p)  sum(lvl2.mice.bd$p[[3]][[p]] > .05)/k),
                        sapply(seq_along(miss.prob), function(p)  sum(lvl2.mice.bd$p[[2]][[p]] > .05)/k))
    
    lvl2mice.table <- as.data.frame(round(rbind(lvl2mice.mean,lvl2mice.sd,lvl2mice.power), digits = 5))
    lvl2mice.table[2,] <- apply(round(lvl2mice.table[2,], digits = 5), 2, function(i) paste("(", i, ")", sep=""))
    colnames(lvl2mice.table) <- c(rep("MCAR",3), rep("MAR",3), rep("MNAR",3))
    
    level2table <- rbind(lvl2bn.table,lvl2bnrc.table,lvl2mice.table)
    
  return(level2table)
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






