##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))

# first impute filter information and then based on these impute wealth components:

##### Filter Imputation


mydata <- mydata %>% 
  mutate(owner = factor(owner),
         other_estate = factor(other_estate),
         assets_filter = factor(assets_filter),
         building_contract_filter = factor(building_contract_filter),
         life_insure_filter = factor(life_insure_filter),
         business_holdings_filter = factor(business_holdings_filter),
         vehicles_filter = factor(vehicles_filter), 
         tangibles_filter = factor(tangibles_filter),
         residence_debt_filter = factor(residence_debt_filter),
         other_estate_debt_filter = factor(other_estate_debt_filter),
         consumer_debt_filter = factor(consumer_debt_filter),
         education_debt_filter = factor(education_debt_filter),
         lmstatus = factor(lmstatus),
         sex = factor(sex),
         famstd = factor(famstd),
         inherit_filter = factor(inherit_filter),
         selfempl = factor(selfempl),
         kidsu16 = factor(kidsu16),
         other_estate_debt_filter = factor(other_estate_debt_filter),
         employed = factor(employed),
         livingcond = factor(livingcond, ordered = T),
         other_estate_value_limits = factor(other_estate_value_limits, ordered = T),
         educationjob    = ifelse(educationjob <0 , NA, educationjob),
         educationjob    = factor(educationjob, ordered = T),
         housedebt       = factor(housedebt),
         lnsqrmtrs       = normalize.log(sqmtrs),
         lnsaving        = normalize.log(saving_value),
         lnhhnetto       = normalize.log(hhnetto),
         lninheritance   = normalize.log(total_inheritance),
         lnorbis         = normalize.log(orbis_wealth),
         lnwage_g        = normalize.log(wage_gross_m),
         lnjobduration   = normalize.log(jobduration),
         lnestateinc     = normalize.log(estate_income_value),
         educationjob    = ifelse(educationjob==-1,NA,educationjob))

###### Preparing Monte Carlo Study #####

filterimp <- c("pid","owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
               "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
               "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", "lmstatus",
               "sex", "famstd", "inherit_filter", "selfempl","kidsu16", "employed",
               "livingcond", "educationjob", "housedebt", "lnsqrmtrs", "lnsaving", "lnhhnetto", "lninheritance",
               "lnorbis", "lnwage_g", "lnjobduration")    
filterimp2 <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
               "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
               "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", "lmstatus",
               "sex", "famstd", "inherit_filter", "selfempl","kidsu16", "employed",
               "livingcond", "educationjob", "housedebt", "lnsqrmtrs", "lnsaving", "lnhhnetto", "lninheritance",
               "lnorbis", "lnwage_g", "lnjobduration")   
  
filter <- select(mydata, one_of(filterimp))
gg_miss_var(filter, show_pct = TRUE)

# first impute filter variables 
filter.imp <- VIM::kNN(filter, variable = filterimp2, imp_var =F)

gg_miss_var(filter.imp, show_pct = TRUE)

filters <- c("pid","owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
             "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
             "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter")
filters2 <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
             "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
             "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter")
filter.post <- select(filter.imp,one_of(filters))

#### parse filters back to a subset of mydata with all of the wealth components:

wealth.vars <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles",
                 "residence_debt", "other_estate_debt", "consumer_debt", "education_debt")
other.vars <- c("pid","lmstatus","sex", "famstd", "inherit_filter", "selfempl","kidsu16", "employed",
                "livingcond", "educationjob", "housedebt", "lnsqrmtrs", "lnsaving", "lnhhnetto", "lninheritance",
                "lnorbis", "lnwage_g", "lnjobduration")   

wealth.comp <- select(mydata, one_of(wealth.vars,other.vars))
wealth.comp <- full_join(filter.post,wealth.comp, by=names(intersect(wealth.comp,filter.post)))
gg_miss_var(wealth.comp, show_pct = TRUE)

### all 12 wealth components each filled up sequentially if filter is true
### put here all target variables and the remaining wealth components and then call from this list!
filter.imp <- setNames(lapply(seq_along(wealth.vars), function(i) 
  VIM::kNN(select(filter(wealth.comp, .data[[filters2[[i]]]] == 1), one_of(wealth.vars[i], other.vars)), imp_var =F)), names(wealth.vars))

gg_miss_var(filter.imp[[1]], show_pct = TRUE)

pid <- as.data.frame(mydata$pid)
names(pid) <- "pid"

wealth.final <- left_join(pid, select(filter.imp[[1]], one_of(c("pid", wealth.vars[1]))), by = "pid")
for(i in 2:12){
  wealth.final <- left_join(wealth.final, select(filter.imp[[i]], one_of(c("pid", wealth.vars[i]))), by = "pid")
}
gg_miss_var(wealth.final, show_pct = TRUE)

asset.vars <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles")
liabilities.vars <- c("residence_debt", "other_estate_debt", "consumer_debt", "education_debt")
                 
wealth.final <- wealth.final %>% 
  mutate(asset = rowSums(select(wealth.final, one_of(asset.vars)), na.rm = T),
         liabilities = rowSums(select(wealth.final, one_of(liabilities.vars)), na.rm = T),
         netwealth = asset -liabilities)

gg_miss_var(wealth.final, show_pct = TRUE)
hist(wealth.final$netwealth)
summary(wealth.final$netwealth)

targetvars <- c("pid","sex", "lnhhnetto", "lnwage_g", "educationjob", "lninheritance",
                "housedebt", "selfempl", "employed", "famstd", "age",
                "lnorbis", "lmstatus", "hours_actual", "inherit_filter", "kidsu16",
                "lnestateinc", "lnsaving", "superior", "stillfirstemp",
                "lnsqrmtrs", "lnjobduration")

final <- select(mydata, one_of(targetvars)) 
final <- left_join(final, select(wealth.final, one_of(c("pid", "netwealth"))), by = "pid") %>% 
  select(-pid)
gg_miss_var(final, show_pct = TRUE)
final.imp <- VIM::kNN(final, imp_var = F)
gg_miss_var(final.imp, show_pct = TRUE)

#write.dta(final.imp, "wealth_knn.dta")

multiple.imp <- left_join(full_join(wealth.final,filter.post, by="pid"), select(mydata, one_of(targetvars)), by="pid") 
gg_miss_var(multiple.imp, show_pct = TRUE)
multiple.imp<- VIM::kNN(multiple.imp, variable = targetvars, imp_var = F)
gg_miss_var(multiple.imp, show_pct = TRUE)

multiple.imp <- multiple.imp %>% 
  select(-asset, -netwealth, -liabilities) 
for (i in 1:ncol(multiple.imp)){
  if (sum(is.na(multiple.imp[,i]) > 0)){
    multiple.imp[,i] <- ifelse(is.na(multiple.imp[,i]), 0, multiple.imp[,i])
  }
}
multiple.imp <- multiple.imp %>% 
  mutate(lnresidence       = normalize.log(residence_value),
         lnestate          = normalize.log(other_estate_value),
         lnassets          = normalize.log(assets),
         lnbuilding        = normalize.log(building_contract),
         lnlife            = normalize.log(life_insure),
         lnbusiness        = normalize.log(business_holdings),
         lnvehicles        = normalize.log(vehicles),
         lntangibles       = normalize.log(tangibles),
         lnresidence_debt  = normalize.log(residence_debt),
         lnestate_debt     = normalize.log(other_estate_debt),
         lnconsumer_debt   = normalize.log(consumer_debt),
         lnstudent_debt    = normalize.log(education_debt),
         superior          = factor(superior, ordered = F),
         stillfirstemp     = factor(stillfirstemp, ordered = F),
         educ_high_job     = factor(ifelse(educationjob==4 | educationjob==3,1,0), ordered =F),
         educ_voc_job      = factor(ifelse(educationjob==2,1,0), ordered =F))

multiple.imp <- select(multiple.imp, -asset.vars, -liabilities.vars, -educationjob)


tryfull <- hc(multiple.imp[, -which(names(multiple.imp) == "pid")], whitelist = whitelist, score = "bic-cg")
ggnet2(tryfull$arcs, directed = TRUE,
       arrow.size = 9, arrow.gap = 0.025, label = T)

set.seed(1234)
trymiss <- make.mcar(multiple.imp, p=.1, cond = cond.vector)

gg_miss_var(trymiss, show_pct = TRUE)
for (i in 1:length(lnwealthvars)){
  trymiss[,lnwealthvars[i]] <- ifelse(is.na(trymiss[,lnwealthvars[i]]) & multiple.imp[,lnwealthvars[i]] == min(multiple.imp[,lnwealthvars[i]]), min(multiple.imp[,lnwealthvars[i]]), trymiss[,lnwealthvars[i]])
}
gg_miss_var(trymiss, show_pct = TRUE)

rel_label <- miss_var_summary(trymiss[,names(trymiss) != "pid"], order = T)
reliability <- rel_label$pct_miss
names(reliability) <- rel_label$variable

# initialise an empty BN 
bn = bn.fit(empty.graph(names(trymiss[,names(trymiss) != "pid"])), trymiss[,names(trymiss) != "pid"])
# three iterations of structural EM.
for (i in 1:5) {
  # expectation step.
  imputed = bnlearn::impute(bn, trymiss[,names(trymiss) != "pid"], method = "bayes-lw")
  # maximisation step (forcing LAT to be connected to the other nodes).
  dag = hc(imputed, whitelist = whitelist,score = "bic-cg")
  bn  = bn.fit(dag, imputed, method = "mle")
}

unlist(bnlearn::compare(cpdag(dag), cpdag(tryfull)))

try <- dag
arc.reversal(object = try)

ggnet2(try$arcs, directed = TRUE,
       arrow.size = 9, arrow.gap = 0.025, label = T)
bn  = bn.fit(try, imputed, method = "mle")


#write loop for first imputation by parents:
imp.reliability <- sort(reliability[reliability>0], decreasing = F)
gg_miss_var(trymiss, show_pct = TRUE)

testing <- trymiss
for (k in 1:length(imp.reliability)){
  parents <- bnlearn::parents(bn, names(imp.reliability)[k])
  if (length(parents) == 0){
  data <- trymiss[is.na(trymiss[,names(imp.reliability[k])]),]
  test <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = T, method = "lw"))
  testsample <- dplyr::sample_n(na.omit(test), nrow(data), replace = T)
  } else {
  data <- trymiss[is.na(trymiss[,names(imp.reliability[k])]),parents]
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
      listtest <- setNames(list(parents = as.character(data2[1,])), nm = names(data2))
      test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
    }
    #test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
    } else {
    names(data) <- parents[1]
    listtest <- setNames(list(parents = as.character(data[1,])), nm = names(data))
    test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
    }
   }
  testsample <- sapply(seq_along(test), function(x) sample(na.omit(test[[x]]), 1))
  }
  data[names(imp.reliability)[k]] <- testsample
  testing[,names(imp.reliability[k])][is.na(testing[,names(imp.reliability[k])])] <- data[,names(imp.reliability)[k]]
}

gg_miss_var(testing, show_pct = TRUE)




hellinger(testing$lnhhnetto,multiple.imp$lnhhnetto)
hellinger(imputed$lnhhnetto,multiple.imp$lnhhnetto)

tab <- table(testing$employed,multiple.imp$employed)
1-sum(diag(tab))/sum(tab)



# Bayesian Network for other estate:
  #make sure I have the same variables for BN as for MICE plus a couple more
# MICE imputation covariates from SOEP:
  
  #sex, west, lnwage, years of education, debt indicator, income from rent, selfemployment dummy, car dummy, 
  #public sector, sparbuch1, nopartner dummy, age, age2, private insurance, inheritance, rural area (BIK), other regional stuff (Raumordnungsregion)
  
  
### ask Markus for regional indicators!  
#estate.clean <- estate.clean %>% 
#  mutate(estate = exp(lnestate)* mean(estate$other_estate_value, na.rm = T)) %>% 
#  mutate(estate_limits = ifelse(is.na(estate_limits) & estate < 20000,1,
#                                            ifelse(is.na(estate_limits) & estate >= 20000 & estate < 150000,2,
#                                                   ifelse(is.na(estate_limits) & estate >= 150000 & estate < 500000,3,
#                                                          ifelse(is.na(estate_limits) & estate >= 500000 & estate < 1000000,4,
#                                                                 ifelse(is.na(estate_limits) & estate >= 1000000,5,estate_limits))))))


#apparently we don't necessarily need the deal package
# for later reordering of the cpdag to dag: pdag2dag(x, ordering), or maybe node.ordering(x, debug = FALSE)
# for prediction/impuatation: impute(object, data, method, ..., debug = FALSE)

# if normal structure learning can't handle constraints from the flags try:
#learn.mb(x, node, method, whitelist = NULL, blacklist = NULL, start = NULL,
#test = NULL, alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE)
#learn.nbr(x, node, method, whitelist = NULL, blacklist = NULL,
#          test = NULL, alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE)
### missing values unpractical: set to 0:


mi.list <- setNames(lapply(seq_along(c(asset.vars, liabilities.vars)), function(i) 
  filter(multiple.imp, .data[[filters2[[i]]]] == 1)),nm=c(asset.vars, liabilities.vars)) 
mi.list <- setNames(lapply(seq_along(filters2), function(k) 
  mi.list[[k]][,-which(names(mi.list[[k]]) == filters2[k])]),nm=c(asset.vars, liabilities.vars)) 
  
gg_miss_var(mi.list$residence_value, show_pct = TRUE)

# whitelist the filter vars:
#ln wealth components

lnwealthvars <- c("lnresidence", "lnestate", "lnassets", "lnbuilding", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
                  "lnresidence_debt", "lnestate_debt", "lnconsumer_debt", "lnstudent_debt")

from <- filters2
to <- lnwealthvars
whitelist <- as.data.frame(cbind(from,to)) 


structure <- setNames(lapply(seq_along(lnwealthvars), function(j) hc(mi.list[[j]][, -which(names(mi.list[[j]]) == "pid")],
                 score = "loglik-cg", whitelist = whitelist[-j,])), nm=c(asset.vars, liabilities.vars))  
#structure2 <- hc(residence.imp, score = "aic-cg")
#structure3 <- hc(residence.imp, score = "bic-cg")

#### creating reproducable missing patterns: on full dataset multiple.imp 
# goddamn patters!!! 
set.seed(12345)

# specify variable with missing patterns and different freq values
mi.multiple.imp <- ampute(multiple.imp, prop = .1, mech = "MCAR")
patterns <- mi.multiple.imp$patterns
test <- -1*(patterns-1)
patterns <- test
patterns[,1:14] <- 1 
patterns$kidsu16 <- 1
patterns$age <- 1
patterns$inherit_filter <- 1
patterns$lnorbis <- 1
patterns <- patterns[which(rowSums(patterns)!= 18),]  #ncol(patterns)),]

first <- seq(from = 0.001, to = 0.07, length.out = nrow(patterns)-1)
sum(first)
freq <- c(first,1-sum(first))

cond.vector <- c(filters, "sex", "age", "kidsu16", "inherit_filter", "lnorbis")

mi.multiple.imp1 <- ampute(multiple.imp, prop = .1, mech = "MCAR", patterns = patterns, bycases=T) #, freq = freq)
mi.multiple.imp2 <- ampute(multiple.imp, prop = .1, mech = "MAR", patterns = patterns, bycases=T) #, freq = freq)
mi.multiple.imp3 <- ampute(multiple.imp, prop = .1, mech = "MAR", patterns = patterns, bycases=T) #, freq = freq)

gg_miss_var(mi.multiple.imp1$amp, show_pct = TRUE)
gg_miss_var(mi.multiple.imp2$amp, show_pct = TRUE)
gg_miss_var(mi.multiple.imp3$amp, show_pct = TRUE)


###### Decide whether I want to learn structure given missing values as well or just imputation with a given structure...

mi.list.withmissings <- setNames(lapply(seq_along(c(asset.vars, liabilities.vars)), function(i) 
  filter(mi.multiple.imp1$amp, .data[[filters2[[i]]]] == 1)),nm=c(asset.vars, liabilities.vars)) 
mi.list.withmissings <- setNames(lapply(seq_along(filters2), function(k) 
  mi.list.withmissings[[k]][,-which(names(mi.list.withmissings[[k]]) == filters2[k])]),nm=c(asset.vars, liabilities.vars)) 

#### BN Imputation algorithm ####
# not yet possible, because the predict function is dead 
##### Redirect arcs according to reliability ####



rel_label <- miss_var_summary(mi.list.withmissings$residence_value[,names(mi.list.withmissings$residence_value) != "pid"], order = T)
reliability <- rel_label$pct_miss
names(reliability) <- rel_label$variable

# write routine, that checks each arc whether the arrow is pointing from the less reliable to the more
# reliable node. If so, reverse arc and continue!

#First Method: 
try <- structure$residence_value
arc.reversal(object = try)

### Potentially ready for imputation: 
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
############################################## BNimp #############################################
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#

### Structure learning using Structural EM algorithm:

### diy:

# initialise an empty BN 
bn = bn.fit(empty.graph(names(mi.list.withmissings$residence_value[,names(mi.list$residence_value) != "pid"])), mi.list.withmissings$residence_value[,names(mi.list$residence_value) != "pid"])
# three iterations of structural EM.
for (i in 1:5) {
  # expectation step.
  imputed = bnlearn::impute(bn, mi.list.withmissings$residence_value[,names(mi.list$residence_value) != "pid"], method = "bayes-lw")
  # maximisation step (forcing LAT to be connected to the other nodes).
  dag = hc(imputed, whitelist = whitelist[-1,],score = "loglik-cg")
  bn  = bn.fit(dag, imputed, method = "mle")
}

bnlearn::compare(cpdag(dag), cpdag(structure$residence_value))


#try on pc at DIW
#mi.structure <- structural.em(mi.list.withmissings$residence_value[,names(mi.list$residence_value) != "pid"], maximize = "hc",
#                              fit = "mle", maximize.args = list(score = "loglik-cg", whitelist = whitelist[-1,]) , impute = "bayes-lw", max.iter = 10) 


### ok engage into cpquery and cpdist in order to retrieve imputation values:

dagtest <- dag 
arc.reversal(object = dagtest)

ggnet2(dagtest$arcs, directed = TRUE,
       arrow.size = 9, arrow.gap = 0.025, label = T)
bn  = bn.fit(dagtest, imputed, method = "mle")

#write loop for first imputation by parents:
imp.reliability <- sort(reliability[reliability>0], decreasing = F)
gg_miss_var(mi.list.withmissings$residence_value, show_pct = TRUE)

#for (k in 1:length(imp.reliability)){
  for (k in 1:15){
    
  parents <- bnlearn::parents(bn, names(imp.reliability)[k])
  
  data <- mi.list.withmissings$residence_value[is.na(mi.list.withmissings$residence_value[,names(imp.reliability[k])]),parents]
  test <- NULL
  for (j in 1:nrow(data)){
    data1 <- data[j,]
    data2 <- data1[,!apply(data1,2,function(x) any(is.na(x)))]
    listtest <- setNames(lapply(1:ncol(data2), function(i) data2[,i]), nm=names(data2))
    test[j] <- try(bnlearn::cpdist(bn, nodes = names(imp.reliability)[k], evidence = listtest, method = "lw"))
    #testsample[j] <- sample(na.omit(test$stillfirstemp), 1)
  }
  testsample <- sapply(seq_along(test), function(x) sample(na.omit(test[[x]]), 1))
  data[names(imp.reliability)[k]] <- testsample
  mi.list.withmissings$residence_value[,names(imp.reliability[k])][is.na(mi.list.withmissings$residence_value[,names(imp.reliability[k])])] <- testsample
}

hellinger(mi.list.withmissings$residence_value$lnbuilding,mi.list$residence_value$lnbuilding)

tab <- table(mi.list.withmissings$residence_value$employed, mi.list$residence_value$employed)
1-sum(diag(tab))/sum(tab)
  
  
  
  
  
  
  
  



#this only allows for ML estimation of the parameters at the nodes... go to STAN for more fancy stuff!
fit <-  bnlearn::bn.fit(test2, mi.list$residence_value[,names(mi.list$residence_value) != "pid"])
fit$lnresidence
bn.fit.qqplot(fit$lnresidence)

bnlearn::mb(test2, "lnresidence")

ggnet2(test2$arcs, directed = TRUE,
       arrow.size = 9, arrow.gap = 0.025, label = T)

imputed <- bnlearn::impute(fit, mi.list.withmissings$residence_value[,names(mi.list.withmissings$residence_value) != "pid"], method = "bayes-lw")

imputed2 <- bnlearn::predict.bn.fit(fit, node = "stillfirstemp", data = mi.list.withmissings$residence_value[,names(mi.list.withmissings$residence_value) != "pid"], method = "parents")
#bnlearn::mb(test2, "stillfirstemp")

ks.test(imputed$lnresidence, mi.list$residence_value$lnresidence)
summary(imputed$lnresidence)
summary(mi.list$residence_value$lnresidence)
hell <- hellinger(imputed$lnresidence, mi.list$residence_value$lnresidence)

p1 <- ggplot(data = imputed, aes(x=lnresidence))+
  geom_density(fill = "gold", alpha = .6) +
  # Change the fill colour to differentiate it
  geom_density(data=mi.list$residence_value, fill="#52854C", alpha = .6) +
  labs(title = "Density of log of market value of primary residence")+
  labs(y="Density")+
  labs(x="log(residence value)")
p1





#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
############################################## MICE ##############################################
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#

imp <- mice(mi.list.withmissings$residence_value, maxit = 0, print=F)
meth <- imp$method
pred <- imp$predictorMatrix

indepvars.all <- c("kidsu16", "sex", "educ_high_job", "educ_voc_job", "lnwage_g",
                   "lnhhnetto", "lninheritance", "inherit_filter", "lnsaving", "superior",
                   "selfempl", "employed", "famstd", "age", "hours_actual", "lnjobduration",
                   "lnestateinc", "stillfirstemp")
pred[,] <- 0
miss <- NULL
for (i in 1:length(a)){
  if (a[i]>0){
    miss[i] <- names(a[i])
  }
}  
miss <- na.omit(miss)
for (i in 1:length(miss)){
  pred[miss[i], indepvars.all[indepvars.all != miss[i]]] <- 1
}

for (i in 1:length(lnwealthvars)){
  pred[lnwealthvars[i], indepvars.all[indepvars.all != lnwealthvars[i]]] <- 1
  pred[lnwealthvars[i], "lnorbis"] <- 1
  
}

pred["lninheritance", c("lntangibles", "housedebt", "lnresidence", "lnestate", "lnestateinc")] <- 1
pred["housedebt", c("lnresidence", "lnestate", "lnsqrmtrs", "lnbuilding", "lnestateinc")] <- 1
pred["lnresidence", c("lnsqrmtrs", "housedebt", "lnestateinc")] <- 1
pred["lnestate", c("lnsqrmtrs", "housedebt", "lnestateinc")] <- 1
pred["lnestateinc", c("lnresidence", "lnestate", "lnsqrmtrs", "lnbuilding")] <- 1
pred["lnresidence_debt", c("lnsqrmtrs", "housedebt", "lnestateinc")] <- 1
pred["lnestate_debt", c("lnsqrmtrs", "housedebt", "lnestateinc")] <- 1

mice.imp <- mice(mi.list.withmissings$residence_value, maxit = 15, predictorMatrix = pred ,print=F, seed=123)
plot(mice.imp)

stripplot(mice.imp)
hell <- sapply(1:mice.imp$m, function(i) hellinger(mice.imp$imp$lnhhnetto[[i]], mi.list$residence_value$lnhhnetto))






#multiple.imp2 <- as.data.frame(multiple.imp2)
for (i in 1:ncol(multiple.imp2)){
    if (sum(is.na(multiple.imp2[,i]) > 0)){
  multiple.imp2[,i] <- ifelse(is.na(multiple.imp2[,i]), 0, multiple.imp2[,i])
  }
}
gg_miss_var(multiple.imp2, show_pct = TRUE)


# whitelist the filter vars:
from <- c(filters2)
to <- c(asset.vars, liabilities.vars)
whitelist <- as.data.frame(cbind(from,to)) 

structurelearn1 <- hc(multiple.imp2, score = "loglik-cg", whitelist = whitelist)
structurelearn2 <- hc(multiple.imp2, score = "aic-cg", whitelist = whitelist)
structurelearn3 <- hc(multiple.imp2, score = "bic-cg", whitelist = whitelist)

score <- bnlearn::score(structurelearn1, multiple.imp2, type = "aic-cg", by.node = T)



bnlearn::parents(structurelearn4, "residence_value")


graphviz.plot(structurelearn)

ggnet2(structurelearn4$arcs, directed = TRUE,
       arrow.size = 9, arrow.gap = 0.025, label = T)




reliability <- sapply(multiple.imp2, function(x) sum(is.na(x)))/nrow(multiple.imp2)
reliability <- order(reliability, decreasing = T)





net <- deal::network(residence.imp)
print(net)
plot(net)

prior <- jointprior(net)

net <- deal::learn(net, residence.imp, prior)$nw

best <- autosearch(net, residence.imp, prior, trace = T, removecycles = T)








net <- deal::network(multiple.imp2)
print(net)
plot(net)

prior <- jointprior(net, N = 10)

net <- deal::learn(net, final.imp, prior)$nw

best <- autosearch(net, final.imp, prior, trace = T, removecycles = T)


mstring <- deal::modelstring(best$nw)
mstring
dag.deal <- model2network(mstring)

ggnet2(dag.deal$arcs, directed = TRUE,
       arrow.size = 9, arrow.gap = 0.025, label = T)
ggsave("bayesian_net.pdf")

mb(dag.deal, "lnestate")

#### for presentation ####

dag1 <- empty.graph(nodes = c("A", "B", "C", "D", "E", "F"))
arc.set <- matrix(c("A", "C",
                    "B", "C",
                    "C", "D",
                    "C", "E",
                    "D", "F",
                    "E", "F"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag1) <- arc.set

modelstring(dag1)
nodes(dag1)
arcs(dag1)

# Graphical Implementation
pdf("exampleBN.pdf") 
graphviz.plot(dag1)
dev.off()



hlight <- list(nodes = nodes(dag1), arcs = arcs(dag1),
               col = "grey", textCol = "grey")
pp <- graphviz.plot(dag1, highlight = hlight)

graph::nodeRenderInfo(pp) <- list(col = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45)),
                                  textCol = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45)))
graph::edgeRenderInfo(pp) <- list(col = c("A~C" = "black", "B~C" = "black"),
                                  lwd = c("A~C" = 3, "B~C" = 3))
pdf("parentsBN.pdf") 
Rgraphviz::renderGraph(pp)
dev.off()

mb(dag1, node = "C")
pp <- graphviz.plot(dag1, highlight = hlight)

graph::nodeRenderInfo(pp) <- list(col = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black"),
                                  textCol = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black"))
graph::edgeRenderInfo(pp) <- list(col = c("A~C" = "black", "B~C" = "black", "C~D" = "black", "C~E" = "black"),
                                  lwd = c("A~C" = 3, "B~C" = 3, "C~D" = 3, "C~E" = 3))
pdf("markovb.pdf") 
Rgraphviz::renderGraph(pp)
dev.off()


### better to see the Markov blanket property:

dag2 <- empty.graph(nodes = c("A", "B", "C", "D", "E", "F", "G"))
arc.set <- matrix(c("A", "C",
                    "B", "C",
                    "C", "D",
                    "C", "E",
                    "D", "F",
                    "E", "F",
                    "G", "D"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set

mb(dag2, node = "C")

pp <- graphviz.plot(dag2, highlight = hlight)

graph::nodeRenderInfo(pp) <- list(col = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black", "G" = "black"),
                                  textCol = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black", "G" = "black"))
graph::edgeRenderInfo(pp) <- list(col = c("A~C" = "black", "B~C" = "black", "C~D" = "black", "C~E" = "black", "G~D" = "black"),
                                  lwd = c("A~C" = 3, "B~C" = 3, "C~D" = 3, "C~E" = 3, "G~D" = 3))
pdf("markovb_2.pdf") 
Rgraphviz::renderGraph(pp)
dev.off()


