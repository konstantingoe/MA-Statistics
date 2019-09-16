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


# mydata <- mydata %>% 
#   mutate(owner                           = factor(owner),
#          other_estate                    = factor(other_estate),
#          assets_filter                   = factor(assets_filter),
#          building_contract_filter        = factor(building_contract_filter),
#          life_insure_filter              = factor(life_insure_filter),
#          business_holdings_filter        = factor(business_holdings_filter),
#          vehicles_filter                 = factor(vehicles_filter), 
#          tangibles_filter                = factor(tangibles_filter),
#          residence_debt_filter           = factor(residence_debt_filter),
#          other_estate_debt_filter        = factor(other_estate_debt_filter),
#          consumer_debt_filter            = factor(consumer_debt_filter),
#          education_debt_filter           = factor(education_debt_filter),
#          lmstatus                        = factor(lmstatus),
#          sex                             = factor(sex),
#          famstd                          = factor(famstd),
#          inherit_filter                  = factor(inherit_filter),
#          selfempl                        = factor(selfempl),
#          kidsu16                         = factor(kidsu16),
#          livingcond                      = factor(livingcond, ordered = T),
#          education                       = factor(education, ordered = T),
#          job_training                    = factor(job_training),
#          compsize                        = factor(compsize, ordered = T),
#          superior                        = factor(superior),
#          overtime                        = factor(overtime, ordered = T),
#          second_empl                     = factor(second_empl),
#          citizen                         = factor(citizen),
#          schoolingF                      = factor(schoolingF, ordered = T),
#          schoolingM                      = factor(schoolingM, ordered = T),
#          trainingF                       = factor(trainingF, ordered = T),
#          trainingM                       = factor(trainingM, ordered = T),
#          hhtyp                           = factor(hhtyp),
#          livingcond                      = factor(livingcond, ordered = T),
#          housecond                       = factor(housecond, ordered = T),
#          lnsqrmtrs                       = normalize.log(sqmtrs),
#          lnsaving                        = normalize.log(saving_value),
#          lnhhnetto                       = normalize.log(hhnetto),
#          lninheritance                   = normalize.log(total_inheritance),
#          lnorbis                         = normalize.log(orbis_wealth),
#          lnwage_g                        = normalize.log(wage_gross_m),
#          lnwage_n                        = normalize.log(wage_net_m),
#          lnjobduration                   = normalize.log(jobduration),
#          lnestateinc                     = normalize.log(estate_income_value))


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
         nkids                           = factor(nkids, ordered = T),
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
         saving                          = factor(saving),
         partner                         = factor(partner),
         gborn                           = factor(gborn),
         educationjob                    = factor(educationjob, ordered = T),
         bula                            = factor(bula_sta),
         bik                             = fct_rev(factor(bik, ordered = T)),
         ggk                             = factor(ggk, ordered = T),
         wuma7                           = factor(wuma7, ordered = F),
         wuma3                           = factor(wuma3, ordered = T),
         wuma5                           = factor(wuma5, ordered = T),
         ost                             = factor(ost),
         hhgr                            = factor(hhgr, ordered = T))


###### Preparing Monte Carlo Study #####

### subsetting useful variables:

targetvars <- c("pid", "age", "sex", "ost", "bula", "bik", "ggk", "wuma7", "wuma3", "wuma5", "hhgr", "orbis_wealth", "job_training", "lmstatus", "educationjob", "jobduration",
                "selfempl", "compsize", "superior", "workinghours", "overtime", "wage_gross_m", "wage_net_m",
                "second_empl", "inherit_filter", "total_inheritance", "citizen", "famstd",
                "owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
                "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
                "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", 
                "residence_value", "other_estate_value", "assets", "building_contract",
                "life_insure", "business_holdings", "vehicles", "tangibles",
                "residence_debt", "other_estate_debt", "consumer_debt", "education_debt",
                "gborn", "education", "schoolingF", "schoolingM", "trainingF", "trainingM", 
                "nkids", "partner", "hhtyp", "livingcond", "sqmtrs", "hhnetto", 
                "saving", "saving_value", "kidsu16")

data <- select(mydata, one_of(targetvars))
gg_miss_var(data, show_pct = TRUE)

#all except 12 wealth components plus inheritance and saving which will also be imputed when filter is imputed 

filterimp <-  c("pid", "age", "sex",  "ost", "bula", "bik", "ggk", "wuma7", "wuma3", "wuma5", "hhgr", "orbis_wealth", "job_training", "lmstatus", "educationjob", "jobduration",
                "selfempl", "compsize", "superior", "workinghours", "overtime", "wage_gross_m", "wage_net_m",
                "second_empl", "inherit_filter", "citizen", "famstd",
                "owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
                "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
                "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", 
                "gborn", "education", "schoolingF", "schoolingM", "trainingF", "trainingM", 
                "nkids", "partner", "hhtyp", "livingcond", "sqmtrs", "hhnetto", 
                "saving", "kidsu16")
 
filterimp2 <- c("age", "sex", "ost", "bula", "bik", "ggk", "wuma7", "wuma3", "wuma5", "hhgr", "orbis_wealth", "job_training", "lmstatus", "educationjob", "jobduration",
                "selfempl", "compsize", "superior", "workinghours", "overtime", "wage_gross_m", "wage_net_m",
                "second_empl", "inherit_filter", "citizen", "famstd",
                "owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
                "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
                "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", 
                "gborn", "education", "schoolingF", "schoolingM", "trainingF", "trainingM", 
                "nkids", "partner", "hhtyp", "livingcond", "sqmtrs", "hhnetto", 
                "saving", "kidsu16") 
  
filter <- select(data, one_of(filterimp))
gg_miss_var(filter, show_pct = TRUE)

# first impute filter variables 
filter.imp <- VIM::kNN(filter, variable = filterimp2, dist_var = colnames(filter[, -which(names(filter) == "pid")]),
                       donorcond =  as.list(rep("!= -2",length(filterimp2))), imp_var =F)

wealth.vars <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles",
                 "residence_debt", "other_estate_debt", "consumer_debt", "education_debt")

filters <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
             "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
             "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter")

wealth.comp <- full_join(filter.imp,select(data, one_of(c("pid", wealth.vars, "saving_value", "total_inheritance"))), by="pid")

wealth.vars2 <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles",
                 "residence_debt", "other_estate_debt", "consumer_debt", "education_debt", "saving_value", "total_inheritance")

filters2 <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
              "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
              "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", "saving", "inherit_filter")

for (i in 1:length(filters2)){
  wealth.comp[,wealth.vars2[i]] <- ifelse(wealth.comp[,filters2[i]] == 0, -2, wealth.comp[,wealth.vars2[i]])
}

wealth.imp <- VIM::kNN(wealth.comp, variable = wealth.vars2, dist_var = colnames(wealth.comp[, -which(names(wealth.comp) == "pid")]),
                       donorcond =  as.list(rep("!= -2",length(wealth.vars2))), imp_var =F)
gg_miss_var(wealth.imp, show_pct = TRUE)

#### recode all -2 to missings for transformation and then recode them to a new logical missing code
exceptions <- c("pid", "age", "hhgr", "orbis_wealth", "sqmtrs", "hhnetto")

recode.vars <- setdiff(names(select_if(wealth.imp, is.numeric)), exceptions)

for (i in 1:length(recode.vars)){
  wealth.imp[,recode.vars[i]] <- ifelse(wealth.imp[,recode.vars[i]] == -2, NA, wealth.imp[,recode.vars[i]])
}
                 
multiple.imp <- wealth.imp %>% 
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
         lnjobduration     = normalize.log(jobduration),
         lnworkinghours    = normalize.log(workinghours),
         lnwage_gross      = normalize.log(wage_gross_m),
         lnwage_net        = normalize.log(wage_net_m),
         lnsaving          = normalize.log(saving_value),
         lninheritance     = normalize.log(total_inheritance),
         lnorbis           = normalize.log(orbis_wealth),
         lnsqmtrs          = normalize.log(sqmtrs),
         lnhhnetto         = normalize.log(hhnetto)) 

lnwealthvars <- c("lnresidence", "lnestate", "lnassets", "lnbuilding", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
                  "lnresidence_debt", "lnestate_debt", "lnconsumer_debt", "lnstudent_debt")
lnrecode.vars <- c("lnresidence", "lnestate", "lnassets", "lnbuilding", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
                   "lnresidence_debt", "lnestate_debt", "lnconsumer_debt", "lnstudent_debt", "lnjobduration",
                   "lnworkinghours", "lnwage_gross", "lnwage_net", "lnsaving", "lninheritance")

rerecode.vars <- c(wealth.vars, "jobduration", "workinghours", "wage_gross_m", "wage_net_m", "saving_value", "total_inheritance")

###### Truth comes here ########
truth <- multiple.imp
for (i in 1:length(lnrecode.vars)){
  truth[,lnrecode.vars[i]] <- ifelse(is.na(truth[,lnrecode.vars[i]]), 
                                            -log(mean(1+truth[,rerecode.vars[i]], na.rm = T))/log(sd(1+truth[,rerecode.vars[i]], na.rm =T)),
                                     truth[,lnrecode.vars[i]])
}

truth <- select(truth, -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))
gg_miss_var(truth, show_pct = TRUE)

from <- c(filters, rep("lmstatus", 6), "saving", "inherit_filter")
to <- c(lnwealthvars, "lnjobduration", "lnworkinghours", "lnwage_gross", "lnwage_net", "compsize", "superior", "lnsaving", "lninheritance")
whitelist <- as.data.frame(cbind(from,to)) 


truth.structure <- hc(truth[, -which(names(truth) == "pid")], whitelist = whitelist, score = "bic-cg")
ggnet2(truth.structure$arcs, 
       arrow.size = 9, arrow.gap = 0.025, label = T)
bnlearn::graphviz.plot(truth.structure)

###### truth done ######

#### prepare simulation #####

for (i in 1:length(lnrecode.vars)){
  multiple.imp[,lnrecode.vars[i]] <- ifelse(is.na(multiple.imp[,lnrecode.vars[i]]), 
                                            -99,
                                            multiple.imp[,lnrecode.vars[i]])
}

cond.vector <- c(lnrecode.vars,
                 "lnhhnetto", "schoolingF", "schoolingM", "trainingF", 
                 "trainingM", "compsize", "superior", "education")

x.vars <- c("age", "sex", "ost", "bik", "wuma7", "inherit_filter",
            "citizen", "gborn", "kidsu16", "partner", "saving")

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
                          ##### Simulation ####
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####



k <- 100
set.seed(1234)
numCores <- detectCores() -1
miss.mechanism <- list("MCAR" = make.mcar, "MNAR" = make.mnar)
miss.mechanism2 <- list("MAR" = make.mar)
miss.mech.vec <- c("MCAR", "MNAR", "MAR")

mi.multiple.imp <-  setNames(lapply(seq_along(miss.mechanism), function(m)
  lapply(1:k, function(l) miss.mechanism[[m]](multiple.imp, miss.prob=.1, cond = cond.vector))), nm=names(miss.mechanism))
mi.multiple.imp <- c(mi.multiple.imp, setNames(lapply(seq_along(miss.mechanism2), function(m)
  lapply(1:k, function(l) miss.mechanism2[[m]](multiple.imp, miss.prob = .1, cond = cond.vector, x.vars = x.vars))), nm=names(miss.mechanism2)))

for (m in 1:length(miss.mech.vec)){
  for (l in 1:k) { 
    for (i in 1:length(lnrecode.vars)){
      mi.multiple.imp[[m]][[l]][,lnrecode.vars[i]] <- ifelse(mi.multiple.imp[[m]][[l]][,lnrecode.vars[i]] == -99, 
                                                   -log(mean(1+multiple.imp[,rerecode.vars[i]], na.rm = T))/log(sd(1+multiple.imp[,rerecode.vars[i]], na.rm =T)),
                                                   mi.multiple.imp[[m]][[l]][,lnrecode.vars[i]])
    }
    mi.multiple.imp[[m]][[l]] <- select(mi.multiple.imp[[m]][[l]], -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))
  } 
}
  
#mi.structure <- structural.em(mi.multiple.imp[,names(mi.multiple.imp) != "pid"], maximize = "hc",
#                                fit = "mle", maximize.args = list(score = "bic-cg", whitelist = whitelist) , impute = "bayes-lw", max.iter = 5, return.all = T) 

# superior is the bad guy!!!! 

#mi.multiple.imp[[6]][,discrete.imp.vars] <- NULL 
mi.structure <- setNames(lapply(1:length(miss.mech.vec), function(m)
                    mclapply(1:k, function(l) structural.em(mi.multiple.imp[[m]][[l]][,names(mi.multiple.imp[[m]][[l]]) != "pid"], maximize = "hc",
                          fit = "mle", maximize.args = list(score = "bic-cg", whitelist = whitelist) , impute = "bayes-lw", max.iter = 2, return.all = T),
                            mc.cores = numCores)), nm = miss.mech.vec)

save(mi.structure, file = "structure.RDA")
lapply(1:length(miss.mech.vec), function(m) sapply(1:k, function(l) unlist(bnlearn::compare(truth.structure, mi.structure[[m]][[l]]$dag))))

for (m in 1:length(miss.mech.vec)){
  for (l in 1:k){
    rel_label <- miss_var_summary(mi.multiple.imp[[m]][[l]][,names(mi.multiple.imp[[m]][[l]]) != "pid"], order = T)
    reliability <- rel_label$pct_miss
    names(reliability) <- rel_label$variable
    arc.reversal(object = mi.structure[[m]][[l]]$dag)
  }
}

bn <-  setNames(lapply(1:length(miss.mech.vec), function(m)
        lapply(1:k, function(l) bn.fit(mi.structure[[m]][[l]]$dag, mi.structure[[m]][[l]]$imputed, method = "mle"))), nm = miss.mech.vec)

bn.imp <- setNames(mclapply(1:length(miss.mech.vec), function(m)
            mclapply(1:k, function(l) bn.parents.imp(bn=bn[[m]][[l]], 
                         dat=mi.multiple.imp[[m]][[l]]), mc.cores = numCores), mc.cores = numCores), nm = miss.mech.vec)

save(bn.imp, file = "bnimp.RDA")

#### Begin with second BNRC imputation :


structure.test <- mi.structure$MCAR[[1]]$dag
bnlearn::mb(structure.test,cond.vector[1])

# Bayesian Network Reliability Chain (BNRC) algorithm:
# Given Structure G_t retrieve Markov blanket of node X_t that is the most reliable, meaning where there is the lowest missing 
# percentage. Whenever X_t is missing draw value from the conditional distribution, that is, conditional on the Markov blanket set. 
# Should one variable in the Markov blanket set be missing, give that observation the mean as an initial value. 
# This way, fully impute X_t. 

# Now, continue to the nex least reliable node X_t+1 and do the same. Should X_t be included in the Markov blanket set of X_t+1
# do not use the mean, but the just imputed values. Should there be a variable in the Markov blanket set that is missing and has not yet been 
# imputed, again fill those missings in by the mean. This way, proceed up the reliability order until all nodes are filled in. 
# More reliable variables will thus more likely have missing observations in der Markov parent set, while less reliable ones
# have less missings in their conditioning set. 

# This completes the first iteration of the chain. For the second iteration, go back to the first node X_t set the initially imputed values back
# to missing and impute them by drawing from the Markov blanket conditional distribution. Now we don't need to use any mean imputation because the Markov blanket
# only consists of complete variables from the previous iteration. Go through the reliability order, everytime updating the previously generated imputed values by the
# new ones. By the end of this iteration, all nodes are complete and none of them directly depends on the mean anymore. 

# Now got back the first node X_t and repeat this procedure, that is dropping all that were missing initially and imputing given the
# Markov blanket given the imputed values from the previous iteration. 

# Repeat going through this chain frequently enough so that the imputed values are (almost surely) independent from the initial mean imputation.
# find some stopping criterion 
# this could also be thought of as a bootstrap case... 




#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
                              #### MICE ####
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
## the reason why mice performs poorly is the inability to follow logical constraints... sometimes "0" values will be impuated

for (m in 1:length(miss.mech.vec)){
  for (l in 1:k){
  mi.multiple.imp[[m]][[l]] <- mi.multiple.imp[[m]][[l]] %>% 
    mutate(employed = ifelse(lmstatus %in% c(1,2,4), 1,0))
  }
}  
imp <- mice(mi.multiple.imp[[1]][[1]], maxit = 0, print=F)
meth <- imp$method
pred <- imp$predictorMatrix
rel_label <- miss_var_summary(mi.multiple.imp[[1]][[1]][,names(mi.multiple.imp[[1]][[1]]) != "pid"], order = T)
reliability <- rel_label$pct_miss
names(reliability) <- rel_label$variable
#pred[,"pid"] <- 0

indepvars.all <- c("age", "sex", "ost", "bik", "wuma7", "employed", "inherit_filter",
                   "citizen", "famstd", "gborn", "kidsu16", "partner", "saving", "lnsaving", 
                   "lninheritance", "lnhhnetto", "schoolingF", "schoolingM", "compsize",
                   "education")

miss <- names(reliability[reliability>0])
pred[,] <- 0

for (i in 1:length(miss)){
  pred[miss[i], indepvars.all[indepvars.all != miss[i]]] <- 1
}

#pred[lnwealthvars, c("lnorbis",filters)] <- 1
pred[lnwealthvars, c("lnorbis")] <- 1

# more variables for residence variables:
indepvars.residence <- c("sex", "ost", "lnwage_gross", "lnwage_net", "lnresidence_debt", "lnestate_debt", "selfempl",
                         "lnvehicles", "partner", "age", "lnlife", "lninheritance", "bik", "wuma7")
indepvars.residence.debt <- c("sex", "ost", "lnwage_gross", "lnwage_net", "lnresidence", "lnestate", "selfempl",
                              "lnvehicles", "partner", "age", "lnlife", "lninheritance", "bik", "wuma7")
pred["lnresidence_debt", c(indepvars.residence.debt, "lnestate_debt")] <- 1
pred["lnestate_debt", c(indepvars.residence.debt, "lnresidence_debt")] <- 1
pred["lnresidence", c(indepvars.residence, "lnestate")] <- 1
pred["lnestate", c(indepvars.residence, "lnresidence")] <- 1
pred["consumer_debt_filter", c(indepvars.residence, "lnresidence", "lnestate")] <- 1
pred["lninheritance", c("lntangibles", "lnresidence", "lnestate")] <- 1
pred["lnbuilding", c(indepvars.residence, "lnresidence", "lnestate")] <- 1


mice.imp <- setNames(mclapply(1:length(miss.mech.vec), function(m)
              mclapply(1:k, function(l) mice(mi.multiple.imp[[m]][[l]], maxit = 15, predictorMatrix = pred ,print=F, m=1), mc.cores = numCores),
                mc.cores = numCores), nm = miss.mech.vec)

mice.imp.complete <- setNames(mclapply(1:length(miss.mech.vec), function(m)
                      lapply(1:k, function(l) mice::complete(mice.imp[[m]][[l]],action="long")),mc.cores = numCores), nm = miss.mech.vec)



save(mice.imp, file = "mice.RDA")


##### 1st. Levels of Statistical Consistency: continuous vars ####

continuous.imp.vars <- c(lnrecode.vars, "lnhhnetto")
lvl1.bn <- setNames(mclapply(1:length(miss.mech.vec), function(m)
              sapply(1:k,
                  function(l) sapply(1:length(continuous.imp.vars),
                      function(i) 
                        ks.test(bn.imp[[m]][[l]][,continuous.imp.vars[i]],truth[,continuous.imp.vars[i]])$statistic)),
                          mc.cores = numCores), nm = miss.mech.vec)
lvl1.bn <- setNames(lapply(1:length(miss.mech.vec), function(m)
              as.data.frame(t(lvl1.bn[[m]]))), nm = miss.mech.vec)
for (m in 1:length(miss.mech.vec)){ 
  colnames(lvl1.bn[[m]]) <- continuous.imp.vars
}

lvl1.mice <- setNames(mclapply(1:length(miss.mech.vec), function(m)
              sapply(1:k,
                function(l) 
                  sapply(1:length(continuous.imp.vars), function(i) 
                    ks.test(mice.imp.complete[[m]][[l]][,continuous.imp.vars[i]],truth[,continuous.imp.vars[i]])$statistic)),
                      mc.cores = numCores), nm = miss.mech.vec)

lvl1.mice <- setNames(lapply(1:length(miss.mech.vec), function(m)
              as.data.frame(t(lvl1.mice[[m]]))), nm = miss.mech.vec)
for (m in 1:length(miss.mech.vec)){ 
  colnames(lvl1.mice[[m]]) <- continuous.imp.vars
}

setNames(lapply(1:length(miss.mech.vec), function(m) 
  sapply(lvl1.bn[[m]], mean, na.omit =T) < sapply(lvl1.mice[[m]], mean, na.omit =T)), nm = miss.mech.vec)

#### Latex table representation: 

table.imp <- list("BN" = lvl1.bn, "MICE" = lvl1.mice)

level1.table <-   sapply(seq_along(table.imp), function(i)
                        sapply(table.imp[[i]][[1]], mean, na.omit =T))
for (m in 2:3){
level1.table <- cbind(level1.table, sapply(seq_along(table.imp), function(i)
                    sapply(table.imp[[i]][[m]], mean, na.omit =T)))
}
colnames(level1.table) <- rep(names(table.imp),3)
level1.table <- round(level1.table, digits = 4)

level1.table.sd <-   sapply(seq_along(table.imp), function(i)
                      sapply(table.imp[[i]][[1]], sd, na.omit =T))
for (m in 2:3){
  level1.table.sd <- cbind(level1.table.sd, sapply(seq_along(table.imp), function(i)
                      sapply(table.imp[[i]][[m]], sd, na.omit =T)))
}
colnames(level1.table) <- rep(names(table.imp),3)

level1.table.sd <- apply(round(level1.table.sd, digits = 4), 2, function(i) paste("(", i, ")", sep=""))

output <- NULL
table.names <- NULL
table.empty <- rep("",length(continuous.imp.vars))
for (i in 1:nrow(level1.table)){
  output <- rbind(output, level1.table[i,])
  table.names <- c(table.names,continuous.imp.vars[i])
  output <- rbind(output, level1.table.sd[i,])
  table.names <- c(table.names,table.empty[i])
}

rownames(output) <- table.names

output2 <- output[1:24,]

stargazer(output2, digits = 4, title = "Comparison of BNimp and MICE recovering the marginal continuous distributions",
          out = "level1cont.tex", colnames = T)





##### 2nd. Levels of Statistical Consistency: continuous vars####

lvl2.bn <- setNames(mclapply(1:length(miss.mech.vec), function(m)
            sapply(1:k, function(l) 
              bd.test(bn.imp[[m]][[l]][,continuous.imp.vars],truth[,continuous.imp.vars])$statistic),
               mc.cores = numCores), nm = miss.mech.vec)

lvl2.mice <- setNames(mclapply(1:length(miss.mech.vec), function(m)
              sapply(1:k, function(l) 
                bd.test(mice.imp.complete[[m]][[l]][,continuous.imp.vars],truth[,continuous.imp.vars])$statistic),
                 mc.cores = numCores), nm = miss.mech.vec)

lapply(1:length(miss.mech.vec), function(m) mean(lvl2.bn[[m]])<mean(lvl2.mice[[m]]))

##### 1st. Levels of Statistical Consistency: discrete vars ####

discrete.imp.vars <- c("trainingF", "trainingM", "schoolingF", "schoolingM", "education", "superior", "compsize")
lvl1.discrete.bn <- setNames(mclapply(1:length(miss.mech.vec), function(m)
                      sapply(1:k,
                       function(l) sapply(1:length(discrete.imp.vars),
                        function(i) 1-sum(diag(table(bn.imp[[m]][[l]][,discrete.imp.vars[i]],
                          truth[,discrete.imp.vars[i]])))/sum(table(bn.imp[[m]][[l]][,discrete.imp.vars[i]],
                           truth[,discrete.imp.vars[i]])))),
                            mc.cores = numCores), nm = miss.mech.vec)
lvl1.discrete.bn <- setNames(lapply(1:length(miss.mech.vec), function(m)
                      as.data.frame(t(lvl1.discrete.bn[[m]]))), nm = miss.mech.vec)

for (m in 1:length(miss.mech.vec)){ 
  colnames(lvl1.discrete.bn[[m]]) <- discrete.imp.vars
}

lvl1.discrete.mice <- setNames(mclapply(1:length(miss.mech.vec), function(m)
                        sapply(1:k,
                          function(l) sapply(1:length(discrete.imp.vars),
                            function(i) 1-sum(diag(table(mice.imp.complete[[m]][[l]][,discrete.imp.vars[i]],
                              truth[,discrete.imp.vars[i]])))/sum(table(mice.imp.complete[[m]][[l]][,discrete.imp.vars[i]],
                                truth[,discrete.imp.vars[i]])))),
                                  mc.cores = numCores), nm = miss.mech.vec)

lvl1.discrete.mice <- setNames(lapply(1:length(miss.mech.vec), function(m)
                        as.data.frame(t(lvl1.discrete.mice[[m]]))), nm = miss.mech.vec)
for (m in 1:length(miss.mech.vec)){ 
  colnames(lvl1.discrete.mice[[m]]) <- discrete.imp.vars
}

setNames(lapply(1:length(miss.mech.vec), function(m) sapply(lvl1.discrete.bn[[m]], mean) < sapply(lvl1.discrete.mice[[m]], mean)), nm = miss.mech.vec)

##### 1st. Levels of Statistical Consistency: discrete and continuous vars ####

lvl2.discrete.bn <- setNames(mclapply(1:length(miss.mech.vec), function(m)
                        sapply(1:k, function(l) bd.test(x = select(bn.imp[[m]][[l]], 
                            one_of(continuous.imp.vars, discrete.imp.vars)), y = select(truth, 
                              one_of(continuous.imp.vars, discrete.imp.vars)))$statistic),
                                mc.cores = numCores), nm = miss.mech.vec)

lvl2.discrete.mice <- setNames(mclapply(1:length(miss.mech.vec), function(m)
                        sapply(1:k, function(l) bd.test(x = select(mice.imp.complete[[m]][[l]], 
                            one_of(continuous.imp.vars, discrete.imp.vars)), y = select(truth, 
                              one_of(continuous.imp.vars, discrete.imp.vars)))$statistic),
                                 mc.cores = numCores), nm = miss.mech.vec)

lvl2.discrete.bn
lvl2.discrete.mice

setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.discrete.bn[[m]]) < mean(lvl2.discrete.mice[[m]])), nm = miss.mech.vec)

setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.discrete.bn[[m]])), nm = miss.mech.vec)
setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.discrete.mice[[m]])), nm = miss.mech.vec)
