##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))

#potentially <- set_na(mydata$residence_debt_filter, na =c("Does not apply" = -2), as.tag = T)
# first impute filter information and then based on these impute wealth components:

##### Filter Imputation


mydata <- mydata %>% 
  mutate(owner                           = factor(owner),
         other_estate                    = factor(other_estate),
         assets_filter                   = factor(assets_filter),
         building_contract_filter        = factor(building_contract_filter),
         life_insure_filter              = factor(life_insure_filter),
         business_holdings_filter        = factor(business_holdings_filter),
         vehicles_filter                 = factor(vehicles_filter), 
         tangibles_filter                = factor(tangibles_filter),
         residence_debt_filter           = factor(residence_debt_filter),
         other_estate_debt_filter        = factor(other_estate_debt_filter),
         consumer_debt_filter            = factor(consumer_debt_filter),
         education_debt_filter           = factor(education_debt_filter),
         lmstatus                        = factor(lmstatus),
         sex                             = factor(sex),
         famstd                          = factor(famstd),
         inherit_filter                  = factor(inherit_filter),
         selfempl                        = factor(selfempl),
         kidsu16                         = factor(kidsu16),
         livingcond                      = factor(livingcond, ordered = T),
         education                       = factor(education, ordered = T),
         job_training                    = factor(job_training),
         compsize                        = factor(compsize, ordered = T),
         superior                        = factor(superior),
         overtime                        = factor(overtime, ordered = T),
         second_empl                     = factor(second_empl),
         citizen                         = factor(citizen),
         schoolingF                      = factor(schoolingF, ordered = T),
         schoolingM                      = factor(schoolingM, ordered = T),
         trainingF                       = factor(trainingF, ordered = T),
         trainingM                       = factor(trainingM, ordered = T),
         hhtyp                           = factor(hhtyp),
         livingcond                      = factor(livingcond, ordered = T),
         housecond                       = factor(housecond, ordered = T),
         lnsqrmtrs                       = normalize.log(sqmtrs),
         lnsaving                        = normalize.log(saving_value),
         lnhhnetto                       = normalize.log(hhnetto),
         lninheritance                   = normalize.log(total_inheritance),
         lnorbis                         = normalize.log(orbis_wealth),
         lnwage_g                        = normalize.log(wage_gross_m),
         lnwage_n                        = normalize.log(wage_net_m),
         lnjobduration                   = normalize.log(jobduration),
         lnestateinc                     = normalize.log(estate_income_value))
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

lnwealthvars <- c("lnresidence", "lnestate", "lnassets", "lnbuilding", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
                  "lnresidence_debt", "lnestate_debt", "lnconsumer_debt", "lnstudent_debt")

from <- filters2
to <- lnwealthvars
whitelist <- as.data.frame(cbind(from,to)) 

cond.vector <- c(filters, "sex", "age", "kidsu16", "inherit_filter", "lnorbis")

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
graphviz.plot(try)

bn  = bn.fit(try, imputed, method = "mle")


#write loop for first imputation by parents:
imp.reliability <- sort(reliability[reliability>0], decreasing = F)
gg_miss_var(trymiss, show_pct = TRUE)

dat <- trymiss

set.seed(1234)
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
  trymiss[,names(imp.reliability[k])][is.na(trymiss[,names(imp.reliability[k])])] <- data[,names(imp.reliability)[k]]
}

gg_miss_var(testing, show_pct = TRUE)

try2 <- bn.parents.imp(bn=bn, dat=dat, seed = 1234)




sapply(1:length(lnwealthvars), function(i) 
  hellinger(testing[,lnwealthvars[i]],multiple.imp[,lnwealthvars[i]]) <= hellinger(imputed[,lnwealthvars[i]],multiple.imp[,lnwealthvars[i]]))

hellinger(testing[,lnwealthvars[i]],multiple.imp[,lnwealthvars[i]])

tab <- table(testing$employed,multiple.imp$employed)
1-sum(diag(tab))/sum(tab)

###compare truth and imputed:
distcomp.vars <- names(imp.reliability)

emd <- emd(as.matrix(select(testing, one_of(distcomp.vars))), as.matrix(select(multiple.imp, one_of(distcomp.vars))))

### ask Markus for regional indicators!  
#estate.clean <- estate.clean %>% 
#  mutate(estate = exp(lnestate)* mean(estate$other_estate_value, na.rm = T)) %>% 
#  mutate(estate_limits = ifelse(is.na(estate_limits) & estate < 20000,1,
#                                            ifelse(is.na(estate_limits) & estate >= 20000 & estate < 150000,2,
#                                                   ifelse(is.na(estate_limits) & estate >= 150000 & estate < 500000,3,
#                                                          ifelse(is.na(estate_limits) & estate >= 500000 & estate < 1000000,4,
#                                                                 ifelse(is.na(estate_limits) & estate >= 1000000,5,estate_limits))))))

# MICE imputation covariates from SOEP:

#sex, west, lnwage, years of education, debt indicator, income from rent, selfemployment dummy, car dummy, 
#public sector, sparbuch1, nopartner dummy, age, age2, private insurance, inheritance, rural area (BIK), other regional stuff (Raumordnungsregion)


#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
############################################## MICE ##############################################
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#

imp <- mice(dat, maxit = 0, print=F)
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

miss <- names(imp.reliability)

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

mice.imp <- mice(dat, maxit = 15, predictorMatrix = pred ,print=F, seed=123)
plot(mice.imp)

stripplot(mice.imp)
hell <- sapply(1:mice.imp$m, function(i) hellinger(mice.imp$imp$lnhhnetto[[i]], multiple.imp$lnhhnetto))
hell1 <- hellinger(try2$lnhhnetto, multiple.imp$lnhhnetto)

#multiple.imp2 <- as.data.frame(multiple.imp2)
for (i in 1:ncol(multiple.imp2)){
    if (sum(is.na(multiple.imp2[,i]) > 0)){
  multiple.imp2[,i] <- ifelse(is.na(multiple.imp2[,i]), 0, multiple.imp2[,i])
  }
}
gg_miss_var(multiple.imp2, show_pct = TRUE)
