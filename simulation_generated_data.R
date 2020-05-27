####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
#### Initialising Simulation Study ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

rm(list = ls())

source("packages.R")
source("functions_final.R")
# For simulation on crunch server
mypath <- "/soep/kgoebler/data"

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
### Generate simulated data: ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

set.seed(285)

# Create latent model of the form: X = TB' + E
# with E ~ N(0,0.01) and B ~ U(-1,1)
# and simulate Categorical and dummy variables

# number of continuous variables from latent model
P <- 60
N <- 50 

# Continuous part of data
data <- as.data.frame(simLatent(n = N, p = P, k = 3))
# Discrete part of data
data <- bind_cols(data, as.data.frame(simcategorical(n = N, p = 5)))
data <- bind_cols(data, as.data.frame(simdummy(n = N, p = 5)))
names(data) <- paste("X",1:70,sep = "")
for (i in 1:5){
  data[,P+i] <- factor(data[,P+i], ordered = T)  
}

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
#### Generating Missing Data ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

miss.mech <- list("MCAR" = make.mcar, "MAR" = make.mar, "MNAR" = make.mnar)
miss.prob = list("0.1"=0.1, "0.2"=0.2, "0.3"=0.3)
#Set number of simulation runs
R <- 20
numCores <- detectCores() -1
plan(multiprocess, workers = numCores)

milist <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    setNames(lapply(1:R, function(r) miss.mech[[m]](data = data, miss.prob = miss.prob[[p]])),
             nm = 1:R)),nm=names(miss.prob))), nm=names(miss.mech))


for (m in 2:length(miss.mech)){
  for (p in 1:length(miss.prob)){
    for (r in 1:R){
      for (j in (P+1):(P+5))
        milist[[m]][[p]][[r]][,j] <- factor(milist[[m]][[p]][[r]][,j], ordered = T)
    }
  }
}
for (m in 2:length(miss.mech)){
  for (p in 1:length(miss.prob)){
    for (r in 1:R){
      for (j in (P+6):(P+10))
        milist[[m]][[p]][[r]][,j] <- factor(milist[[m]][[p]][[r]][,j], ordered = F)
    }
  }
}



####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
#### Missforest imputation ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

missforestimpute <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    setNames(future_lapply(future.seed = T, 1:R, function(r) missForest(milist[[m]][[p]][[r]])$ximp),
             nm = 1:R)), nm=names(miss.prob))), nm=names(miss.mech))

save(missforestimpute, file = "missforestimpute.RDA")

#save(missforestimpute, file = paste(mypath, "missforestimpute.RDA", sep = "/"))

###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
#### BN imputation ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
mi.structure <- bnlearn::structural.em(milist$MAR$`0.1`$`1`, maximize = "hc",
                                       fit = "mle", maximize.args = list(score = "bic-cg") , impute = "bayes-lw", max.iter = 2, return.all = T, debug = F)

mi.structure.rev <- mi.structure
arc.reversal(object = mi.structure.rev$dag, data = milist$MAR$`0.1`$`1`)

(bnplot <- ggnet2(mi.structure.rev$dag$arcs,
                  arrow.size = 9, arrow.gap = 0.025, label = T))
#ggsave("truthstruct.pdf", plot = bnplot)

bn <-  bn.fit(mi.structure$dag, mi.structure$imputed, method = "mle")

bnimp <- bn.parents.imp(bn=bn,dag = mi.structure.rev$dag, dat = milist$MAR$`0.1`$`1`)

bnimp.mp <- bnrc.nomean(bn=bn, dat = milist$MAR$`0.3`$`17`, cnt.break = 3)
####
dat <- milist$MAR$`0.1`$`1`





bnimptest <- bd.test(data, bnimp.mp$finalData)
bnimptest2 <- bd.test(data, bnimp)

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
#### MICE imputation ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

dry <- milist
meth <- milist
pred <- milist
for (m in 1:length(miss.mech)){
  for (p in 1:length(miss.prob)){
    for (r in 1:R){
      #dryrun
      dry[[m]][[p]][[r]] <- mice(milist[[m]][[p]][[r]], m=1,maxit = 0, print=F)
      
      # use PLS imputation method for all continuous variables
      meth[[m]][[p]][[r]] <- dry[[m]][[p]][[r]]$method
      meth[[m]][[p]][[r]][1:P] <- "pls"
      # specify predictor matrix
      pred[[m]][[p]][[r]] <- quickpred(milist[[m]][[p]][[r]], mincor = .2, minpuc = .5)
      
      # use 9 PLS factors for all variables that are continuous
      pls.facs <- as.list(rep(9,P))
      names(pls.facs) <- names(meth[[m]][[p]][[r]][1:P])
      
      # choose Bayesian linear regression 
      pls.impMethod <- as.list(rep("norm", P))
      names(pls.impMethod) <- names(meth[[m]][[p]][[r]][1:P])
    }
  }
}

miceimpute <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    setNames(future_lapply(future.packages = "miceadds", future.seed = T, 1:R, function(r)
      mice::complete(mice(milist[[m]][[p]][[r]], m = 1, printFlag = FALSE, maxit = 15, method = meth[[m]][[p]][[r]], predictorMatrix = pred[[m]][[p]][[r]],
                          pls.facs = pls.facs, pls.impMethod = pls.impMethod, ls.meth="ridge", ridge=0.1,  pls.print.progress=F))),
      nm = 1:R)), nm=names(miss.prob))), nm=names(miss.mech))


save(miceimpute, file = paste(mypath, "miceimpute.RDA", sep = "/"))

#load("missforestimpute.RDA")
#load("miceimpute.RDA")

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
#### Performance Evaluation ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

#### BD test ####

performance.missForest <- setNames(lapply(1:length(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:R, function(r) bd.test(x = missforestimpute[[m]][[p]][[r]], y = data)[["complete.info"]])),
    nm= names(miss.prob))), nm = names(miss.mech))


performance.mice <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:R, function(r) bd.test(x = miceimpute[[m]][[p]][[r]], y = data)[["complete.info"]])),
    nm= names(miss.prob))), nm = names(miss.mech))


save(performance.missForest, file = paste(mypath, "forest_performance.RDA", sep = "/"))
save(performance.mice, file = paste(mypath, "mice_performance.RDA", sep = "/"))

#load("forest_performance.RDA")
#load("mice_performance.RDA")

#### retrieveing mean and sd over repetitions:

missforest.bd <- lvl2.extract(data = performance.missForest)
mice.bd <- lvl2.extract(data = performance.mice)

#performance.table <- make.table()
#stargazer(performance.table, title = "Ball divergence between missForest imputation and MICE",
#          out = "table.tex", colnames = T, summary = F, notes = "Source: Author's calculations. BD mean value over R Monte Carlo draws. Monte Carlo Error in brackets. Test-power underneath")

### plot boxplots
labl <- c("missForest", "MICE")

boxpd <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    data_frame(BD = c(missforest.bd[[1]][[m]][[p]],mice.bd[[1]][[m]][[p]]),
               Method = factor(c(rep(1, R), rep(2, R)), ordered = F, labels= labl))),nm = names(miss.prob))),
  nm = names(miss.mech))


scales <- c(.005, 0.014,.009)


box.mean <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    ggplot(aes(x=Method, y=BD, colour = Method), data = boxpd[[m]][[p]]) +
      stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
      xlab("") +
      scale_y_continuous(limits = c(0, scales[m])) + 
      scale_colour_manual(values = gg_color(2)) +
      theme(legend.position = "none") +
      ylab("Ball divergence") +
      ggtitle(paste("BD", 10*p,"% missings and", names(miss.mech)[m], "data")) +
      theme(plot.title = element_text(size = rel(1), vjust = 0), 
            axis.title = element_text(size = rel(0.8)),
            axis.title.y = element_text( vjust=2 ),
            axis.title.x = element_text( vjust=-0.5))),
    nm = names(miss.prob))),nm = names(miss.mech))

plot_grid(box.mean[[1]][[1]], box.mean[[1]][[2]], box.mean[[1]][[3]],
          box.mean[[2]][[1]], box.mean[[2]][[2]], box.mean[[2]][[3]],
          box.mean[[3]][[1]], box.mean[[3]][[2]], box.mean[[3]][[3]], ncol = 3, nrow = 3)

ggsave("boxplot_mean.pdf")


box <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    ggplot(boxpd[[m]][[p]], aes(x=Method, y=BD)) +
      geom_boxplot(aes(group=Method, color = Method)) +
      xlab("") +
      scale_y_continuous(limits = c(0, scales[m])) +  
      theme(legend.position = "none") +
      ylab("Ball divergence") +
      ggtitle(paste("BD", 10*p,"% missings and", names(miss.mech)[m], "data")) +
      theme(plot.title = element_text(size = rel(1), vjust = 0), 
            axis.title = element_text(size = rel(0.8)),
            axis.title.y = element_text( vjust=2 ),
            axis.title.x = element_text( vjust=-0.5))),
    nm = names(miss.prob))),nm = names(miss.mech))


plot_grid(box[[1]][[1]], box[[1]][[2]], box[[1]][[3]],
          box[[2]][[1]], box[[2]][[2]], box[[2]][[3]],
          box[[3]][[1]], box[[3]][[2]], box[[3]][[3]], ncol = 3, nrow = 3)

ggsave("boxplot.pdf")


missforest.rmse <- setNames(lapply(seq_along(miss.mech), function(m) setNames(lapply(seq_along(miss.prob), function(p) 
  sapply(1:R, function(r) nrmse(missforestimpute[[m]][[p]][[r]][,1:60], milist[[m]][[p]][[r]][,1:60], data[,1:60]))),
  nm = names(miss.prob))), nm=names(miss.mech))

mice.rmse <- setNames(lapply(seq_along(miss.mech), function(m) setNames(lapply(seq_along(miss.prob), function(p) 
  sapply(1:R, function(r) nrmse(miceimpute[[m]][[p]][[r]][,1:60], milist[[m]][[p]][[r]][,1:60], data[,1:60]))),
  nm = names(miss.prob))), nm=names(miss.mech))

boxpd.rmse <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    data_frame(RMSE = c(missforest.rmse[[m]][[p]],mice.rmse[[m]][[p]]),
               Method = factor(c(rep(1, R), rep(2, R)), ordered = F, labels= labl))),nm = names(miss.prob))),
  nm = names(miss.mech))

scales.rmse <- c(.4, .61,.5)

box.rmse <- setNames(lapply(seq_along(miss.mech), function(m)
  setNames(lapply(seq_along(miss.prob), function(p)
    ggplot(boxpd.rmse[[m]][[p]], aes(x=Method, y=RMSE)) +
      geom_boxplot(aes(group=Method, color = Method)) +
      xlab("") +
      scale_y_continuous(limits = c(0, scales.rmse[m])) +  
      theme(legend.position = "none") +
      ylab("NRMSE") +
      ggtitle(paste("NRMSE", 10*p,"% missings and", names(miss.mech)[m], "data")) +
      theme(plot.title = element_text(size = rel(1), vjust = 0), 
            axis.title = element_text(size = rel(0.8)),
            axis.title.y = element_text( vjust=2 ),
            axis.title.x = element_text( vjust=-0.5))),
    nm = names(miss.prob))),nm = names(miss.mech))


plot_grid(box.rmse[[1]][[1]], box.rmse[[1]][[2]], box.rmse[[1]][[3]],
          box.rmse[[2]][[1]], box.rmse[[2]][[2]], box.rmse[[2]][[3]],
          box.rmse[[3]][[1]], box.rmse[[3]][[2]], box.rmse[[3]][[3]], ncol = 3, nrow = 3)

ggsave("boxplot_rmse.pdf")