##### Bayesian Network #####

rm(list = ls())
source("packages.R")
#source(".path.R")
source("functions.R")
mypath<- "/soep/kgoebler/data"
mydata <- import(paste(mypath, "topwealth_cleaned.dta", sep = "/"))

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
         nkids                           = factor(nkids, ordered = T),
         livingcond                      = factor(livingcond, ordered = T),
         education                       = factor(education, ordered = T),
         job_training                    = factor(job_training),
         compsize                        = ifelse(compsize %in% c(1,2),1, ifelse(compsize %in% c(3,4),2, ifelse(compsize %in% c(5,6,7),3, ifelse(compsize == -2, -2, NA)))),
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
         bik                             = forcats::fct_rev(factor(bik, ordered = T)),
         ggk                             = factor(ggk, ordered = T),
         wuma7                           = factor(wuma7, ordered = F),
         wuma3                           = factor(wuma3, ordered = T),
         wuma5                           = factor(wuma5, ordered = T),
         ost                             = factor(ost),
         hhgr                            = factor(hhgr, ordered = T))


###### Preparing Monte Carlo Study #####

### subsetting useful variables:
# omit "schoolingF", "schoolingM", "trainingF", "trainingM"

targetvars <- c("pid", "age", "sex", "ost", "bula", "bik", "ggk", "wuma7", "wuma3", "wuma5", "hhgr", "orbis_wealth", "job_training", "lmstatus", "educationjob", "jobduration",
                "selfempl", "compsize", "superior", "workinghours", "overtime", "wage_gross_m", "wage_net_m",
                "second_empl", "inherit_filter", "total_inheritance", "citizen", "famstd",
                "owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
                "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
                "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", 
                "residence_value", "other_estate_value", "assets", "building_contract",
                "life_insure", "business_holdings", "vehicles", "tangibles",
                "residence_debt", "other_estate_debt", "consumer_debt", "education_debt",
                "gborn", "education", 
                "nkids", "partner", "hhtyp", "livingcond", "sqmtrs", "hhnetto", 
                "saving", "saving_value", "kidsu16")

data <- dplyr::select(mydata, dplyr::one_of(targetvars))
#gg_miss_var(data, show_pct = TRUE)
#ggsave("missing.pdf")

#all except 12 wealth components plus inheritance and saving which will also be imputed when filter is imputed 

filterimp <-  c("pid", "age", "sex",  "ost", "bula", "bik", "ggk", "wuma7", "wuma3", "wuma5", "hhgr", "orbis_wealth", "job_training", "lmstatus", "educationjob", "jobduration",
                "selfempl", "compsize", "superior", "workinghours", "overtime", "wage_gross_m", "wage_net_m",
                "second_empl", "inherit_filter", "citizen", "famstd",
                "owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
                "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
                "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", 
                "gborn", "education", 
                "nkids", "partner", "hhtyp", "livingcond", "sqmtrs", "hhnetto", 
                "saving", "kidsu16")
 
filterimp2 <- c("age", "sex", "ost", "bula", "bik", "ggk", "wuma7", "wuma3", "wuma5", "hhgr", "orbis_wealth", "job_training", "lmstatus", "educationjob", "jobduration",
                "selfempl", "compsize", "superior", "workinghours", "overtime", "wage_gross_m", "wage_net_m",
                "second_empl", "inherit_filter", "citizen", "famstd",
                "owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
                "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
                "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", 
                "gborn", "education", 
                "nkids", "partner", "hhtyp", "livingcond", "sqmtrs", "hhnetto", 
                "saving", "kidsu16") 
  
filter <- dplyr::select(data, dplyr::one_of(filterimp))
#gg_miss_var(filter, show_pct = TRUE)

# first impute filter variables 
filter.imp <- VIM::kNN(filter, variable = filterimp2, dist_var = colnames(filter[, -which(names(filter) == "pid")]),
                       donorcond =  as.list(rep("!= -2",length(filterimp2))), imp_var =F)

wealth.vars <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles",
                 "residence_debt", "other_estate_debt", "consumer_debt", "education_debt")

filters <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
             "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
             "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter")

wealth.comp <- dplyr::full_join(filter.imp,dplyr::select(data, dplyr::one_of(c("pid", wealth.vars, "saving_value", "total_inheritance"))), by="pid")

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
#gg_miss_var(wealth.imp, show_pct = TRUE)

#### recode all -2 to missings for transformation and then recode them to a new logical missing code
exceptions <- c("pid", "age", "hhgr", "orbis_wealth", "sqmtrs", "hhnetto")

recode.vars <- setdiff(names(dplyr::select_if(wealth.imp, is.numeric)), exceptions)

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

truth <- dplyr::select(truth, -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))
#gg_miss_var(truth, show_pct = TRUE)

from <- c(filters, rep("lmstatus", 6), "saving", "inherit_filter")
to <- c(lnwealthvars, "lnjobduration", "lnworkinghours", "lnwage_gross", "lnwage_net", "compsize", "superior", "lnsaving", "lninheritance")
whitelist <- as.data.frame(cbind(from,to)) 


truth.structure <- hc(truth[, -which(names(truth) == "pid")], whitelist = whitelist, score = "bic-cg")
#bnplot <- ggnet2(truth.structure$arcs,
#       arrow.size = 9, arrow.gap = 0.025, label = T)
#ggsave("truthstruct.pdf", plot = bnplot)
###### truth done ######

#### prepare simulation #####

for (i in 1:length(lnrecode.vars)){
  multiple.imp[,lnrecode.vars[i]] <- ifelse(is.na(multiple.imp[,lnrecode.vars[i]]), 
                                            -99,
                                            multiple.imp[,lnrecode.vars[i]])
}

cond.vector <- c(lnrecode.vars,
                 "lnhhnetto", "compsize", "superior", "education")

x.vars <- c("age", "sex", "ost", "bik", "wuma7", "inherit_filter",
            "citizen", "gborn", "kidsu16", "partner", "saving")

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
                          ##### Simulation ####
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

set.seed(12)

k <- 500
numCores <- detectCores() -3
plan(multiprocess, workers = numCores)
miss.mechanism <- list("MCAR" = make.mcar, "MNAR" = make.mnar)
miss.mechanism2 <- list("MAR" = make.mar)
miss.mech.vec <- c("MCAR", "MNAR", "MAR")
miss.prob <- list("0.1" = .1, "0.2" = .2, "0.3" = .3)

# mi.multiple.imp <-  setNames(lapply(seq_along(miss.mechanism), function(m)
#                       setNames(lapply(seq_along(miss.prob), function(p)
#                         lapply(1:k, function(l) miss.mechanism[[m]](multiple.imp, miss.prob=miss.prob[[p]], cond = cond.vector))), nm= names(miss.prob))), nm=names(miss.mechanism))
# mi.multiple.imp <- c(mi.multiple.imp, setNames(lapply(seq_along(miss.mechanism2), function(m)
#                     setNames(lapply(seq_along(miss.prob), function(p)
#                       lapply(1:k, function(l) miss.mechanism2[[m]](multiple.imp, miss.prob = miss.prob[[p]], cond = cond.vector, x.vars = x.vars))), nm= names(miss.prob))), nm=names(miss.mechanism2)))
# 
# 
# 
# for (m in 1:length(miss.mech.vec)){
#   for (p in 1:length(miss.prob)){
#     for (l in 1:k) {
#       for (i in 1:length(lnrecode.vars)){
#         mi.multiple.imp[[m]][[p]][[l]][,lnrecode.vars[i]] <- ifelse(mi.multiple.imp[[m]][[p]][[l]][,lnrecode.vars[i]] == -99,
#                                                      -log(mean(1+multiple.imp[,rerecode.vars[i]], na.rm = T))/log(sd(1+multiple.imp[,rerecode.vars[i]], na.rm =T)),
#                                                      mi.multiple.imp[[m]][[p]][[l]][,lnrecode.vars[i]])
#       }
#       mi.multiple.imp[[m]][[p]][[l]] <- select(mi.multiple.imp[[m]][[p]][[l]], -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))
#     }
#   }
# }
# 
# save(mi.multiple.imp, file = paste(mypath, "data.RDA", sep = "/"))
load(paste(mypath, "data.RDA", sep = "/"))


# load("data.RDA")
# load("structure.RDA")
# load("bn.RDA")
# load("bnimp.RDA")
# load("bnrcimp.RDA")
# load("mice.RDA")
# load("micedata.RDA")
# 
# sapply(1:k, function(i) sum(is.na(bnrc$MCAR$`0.3`[[i]])))

# mi.structure <- setNames(lapply(1:length(miss.mech.vec), function(m)
#                   setNames(lapply(seq_along(miss.prob), function(p)
#                     future_lapply(future.seed = T, 1:k, function(l) structural.em(mi.multiple.imp[[m]][[p]][[l]][,names(mi.multiple.imp[[m]][[p]][[l]]) != "pid"], maximize = "hc",
#                           fit = "mle", maximize.args = list(score = "bic-cg", whitelist = whitelist) , impute = "bayes-lw", max.iter = 2, return.all = T))),
#                            nm= names(miss.prob))), nm = miss.mech.vec)

#save(mi.structure, file = paste(mypath, "structure.RDA", sep = "/"))
load(paste(mypath, "structure.RDA", sep = "/"))

#dag.compare <- lapply(1:length(miss.mech.vec), function(m) lapply(1:length(miss.prob), function(p) 
#  sapply(1:k, function(l) unlist(bnlearn::compare(truth.structure, mi.structure[[m]][[p]][[l]]$dag)))))

# mi.structure.rev <- mi.structure
#  for (m in 1:length(miss.mech.vec)){
#   for (p in 1:length(miss.prob)){
#     for (l in 1:k){
#       rel_label <- miss_var_summary(mi.multiple.imp[[m]][[p]][[l]][,names(mi.multiple.imp[[m]][[p]][[l]]) != "pid"], order = T)
#       reliability <- rel_label$pct_miss
#       names(reliability) <- rel_label$variable
#       arc.reversal(object = mi.structure.rev[[m]][[p]][[l]]$dag)
#     }
#   }
# }
# 
# bn <-  setNames(lapply(1:length(miss.mech.vec), function(m)
#         setNames(lapply(seq_along(miss.prob), function(p)
#           future_lapply(future.seed = T, 1:k, function(l) bn.fit(mi.structure[[m]][[p]][[l]]$dag, mi.structure[[m]][[p]][[l]]$imputed, method = "mle"))),
#            nm= names(miss.prob))), nm = miss.mech.vec)
# 
# save(bn, file = paste(mypath, "bn.RDA", sep = "/"))
load(paste(mypath, "bn.RDA", sep = "/"))

# print("Now things are getting serious!")
# bn.imp <- setNames(lapply(1:length(miss.mech.vec), function(m)
#             setNames(lapply(seq_along(miss.prob), function(p)
#               future_lapply(future.seed = T, 1:k, function(l) bn.parents.imp(bn=bn[[m]][[p]][[l]], dag = mi.structure.rev[[m]][[p]][[l]]$dag,
#                 dat=mi.multiple.imp[[m]][[p]][[l]]))), nm=names(miss.prob))),nm=miss.mech.vec)
# print("bn.imp done without errors!!!!!!!")
# save(bn.imp, file = paste(mypath, "bnimp.RDA", sep = "/"))

load(paste(mypath, "bnimp.RDA", sep = "/"))

# print("Now to the really interesting part... please let there be no errors!")
# bnrc <- setNames(lapply(1:length(miss.mech.vec), function(m)
#           setNames(lapply(seq_along(miss.prob), function(p) 
#             future_lapply(future.seed = T, 1:k, function(l) bnrc.imp(bn=bn[[m]][[p]][[l]], 
#               data=mi.multiple.imp[[m]][[p]][[l]], cnt.break = 5, returnfull = F))),
#                 nm= names(miss.prob))), nm = miss.mech.vec)
# print("Hurray, no errors!")
# save(bnrc, file = paste(mypath, "bnrcimp.RDA", sep = "/"))

load(paste(mypath, "bnrcimp.RDA", sep = "/"))

#### Algorithm done

# Bayesian Network Reliability Chain (BNRC) algorithm:
# Given Structure G_t retrieve Markov blanket of node X_t that is the most reliable, meaning where there is the lowest missing 
# percentage. Whenever X_t is missing draw value from the conditional distribution, that is, conditional on the Markov blanket set. 
# Should one variable in the Markov blanket set be missing, give that observation the mean as an initial value. 
# This way, fully impute X_t. 

# Now, continue to the nex least reliable node X_t+1 and do the same. Should X_t be included in the Markov blanket set of X_t+1
# do not use the mean, but the just imputed values. Should there be a variable in the Markov blanket set that is missing and has not yet been 
# imputed, ignore those in the first round... hopefully . This way, proceed up the reliability order until all nodes are filled in. 
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
  for (p in 1:length(miss.prob)){
    for (l in 1:k){
    mi.multiple.imp[[m]][[p]][[l]] <- mi.multiple.imp[[m]][[p]][[l]] %>% 
      mutate(employed = ifelse(lmstatus %in% c(1,2,4), 1,0))
    }
  }  
}  
imp <- mice(mi.multiple.imp[[1]][[1]][[1]], maxit = 0, print=F)
meth <- imp$method
pred <- imp$predictorMatrix
rel_label <- miss_var_summary(mi.multiple.imp[[1]][[1]][[1]][,names(mi.multiple.imp[[1]][[1]][[1]]) != "pid"], order = T)
reliability <- rel_label$pct_miss
names(reliability) <- rel_label$variable
#pred[,"pid"] <- 0

indepvars.all <- c("age", "sex", "ost", "bik", "wuma7", "employed", "inherit_filter",
                   "citizen", "famstd", "gborn", "kidsu16", "partner", "saving", "lnsaving", 
                   "lninheritance", "lnhhnetto", "compsize",
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

#### specify possible range for logical constraints
post <- make.post(mi.multiple.imp[[1]][[1]][[1]])

#post["lnresidence"]      <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[m]][[p]][[1]], owner==1)$lnresidence, na.rm=T), Inf))"
post["lnresidence"]      <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.2638, Inf))"
#post["lnestate"]         <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], other_estate==1)$lnestate, na.rm=T), Inf))"
post["lnestate"]         <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.5252, Inf))"
#post["lnassets"]         <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], assets_filter==1)$lnassets, na.rm=T), Inf))"
post["lnassets"]         <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.4922, Inf))"
#post["lnbuilding"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], building_contract_filter==1)$lnbuilding, na.rm=T), Inf))"
post["lnbuilding"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.9343, Inf))"
#post["lnlife"]           <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], life_insure_filter==1)$lnlife, na.rm=T), Inf))"
post["lnlife"]           <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.5128, Inf))"
#post["lnbusiness"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], business_holdings_filter==1)$lnbusiness, na.rm=T), Inf))"
post["lnbusiness"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.9272, Inf))"
#post["lnvehicles"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], vehicles_filter==1)$lnvehicles, na.rm=T), Inf))"
post["lnvehicles"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.5062, Inf))"
#post["lntangibles"]      <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], tangibles_filter==1)$lntangibles, na.rm=T), Inf))"
post["lntangibles"]      <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.41, Inf))"
#post["lnresidence_debt"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], residence_debt_filter==1)$lnresidence_debt, na.rm=T), Inf))"
post["lnresidence_debt"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.438, Inf))"
#post["lnestate_debt"]    <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], other_estate_debt_filter==1)$lnestate_debt, na.rm=T), Inf))"
post["lnestate_debt"]    <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.3949, Inf))"
#post["lnconsumer_debt"]  <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], consumer_debt_filter==1)$lnconsumer_debt, na.rm=T), Inf))"
post["lnconsumer_debt"]  <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.3812, Inf))"
#post["lnstudent_debt"]   <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], education_debt_filter==1)$lnstudent_debt, na.rm=T), Inf))"
post["lnstudent_debt"]   <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.1447, Inf))"

#post["lnjobduration"]    <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], employed==1)$lnjobduration, na.rm=T), Inf))"
post["lnjobduration"]    <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-1.23, Inf))"
#post["lnworkinghours"]   <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], employed==1)$lnworkinghours, na.rm=T), Inf))"
post["lnworkinghours"]   <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-1.363, Inf))"
#post["lnwage_gross"]     <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], employed==1)$lnwage_gross, na.rm=T), Inf))"
post["lnwage_gross"]     <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.8104, Inf))"
#post["lnwage_net"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], employed==1)$lnwage_net, na.rm=T), Inf))"
post["lnwage_net"]       <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.8014, Inf))"
#post["lnsaving"]         <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], saving==1)$lnsaving, na.rm=T), Inf))"
post["lnsaving"]         <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.5134, Inf))"

#post["lninheritance"]    <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(min(filter(mi.multiple.imp[[1]][[1]][[1]], inherit_filter==1)$lninheritance, na.rm=T), Inf))"
post["lninheritance"]    <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(-0.531, Inf))"

#post["superior"]         <- "imp[[j]][, i] <- squeeze(as.numeric(imp[[j]][, i]), c(as.numeric(levels(mi.multiple.imp[[1]][[1]][[1]]$superior)[2]), as.numeric(levels(mi.multiple.imp[[1]][[1]][[1]]$superior)[3])))"
post["superior"]         <- "imp[[j]][, i] <- squeeze(as.numeric(imp[[j]][, i]), c(0, 1))"

#post["compsize"]         <- "imp[[j]][, i] <- squeeze(as.numeric(imp[[j]][, i]), c(as.numeric(levels(mi.multiple.imp[[1]][[1]][[1]]$compsize)[2]), as.numeric(levels(mi.multiple.imp[[1]][[1]][[1]]$compsize)[4])))"
post["compsize"]         <- "imp[[j]][, i] <- squeeze(as.numeric(imp[[j]][, i]), c(1, 3))"


mice.imp <- setNames(lapply(1:length(miss.mech.vec), function(m)
              setNames(lapply(seq_along(miss.prob), function(p) 
                future_lapply(future.seed = T, 1:10, function(l) mice(mi.multiple.imp[[m]][[p]][[l]], maxit = 15, predictorMatrix = pred, post = post, print=F, m=1))),
                  nm=names(miss.prob))), nm=miss.mech.vec)

mice.imp.complete <- setNames(lapply(1:length(miss.mech.vec), function(m)
                      setNames(lapply(seq_along(miss.prob), function(p) 
                        future_lapply(future.seed = T, 1:10, function(l) mice::complete(mice.imp[[m]][[p]][[l]],action="long"))),
                          nm=names(miss.prob))),nm = miss.mech.vec)


save(mice.imp, file = paste(mypath,"mice.RDA", sep = "/"))
save(mice.imp.complete, file = paste(mypath,"micedata.RDA", sep = "/"))

#### trying to solve the nearest neighbor problem... post processing?
#Another alternative is to split the data into two parts, and specify different a predictor matrix in each. You can combine the mids objects by rbind.
# the way would be to define a "custom made" pmm function where structural zeroes are not considered in the pmm algorithm!


##### 1st. Levels of Statistical Consistency: continuous vars ####
# 
continuous.imp.vars <- c(lnrecode.vars, "lnhhnetto")
# 
# lvl1.bn <- ks.list(data = bn.imp)
# 
# lvl1.bnrc <- ks.list(data = bnrc)
# 
# lvl1.mice <- ks.list(data = mice.imp.complete)
# 
# setNames(lapply(1:length(miss.mech.vec), function(m) 
#   setNames(lapply(seq_along(miss.prob), function(p) 
#     sapply(lvl1.bn[[m]][[p]], mean, na.omit =T) < sapply(lvl1.mice[[m]][[p]], mean, na.omit =T)),nm=names(miss.prob))), nm = miss.mech.vec)
# setNames(lapply(1:length(miss.mech.vec), function(m) 
#   setNames(lapply(seq_along(miss.prob), function(p) 
#     sapply(lvl1.bnrc[[m]][[p]], mean, na.omit =T) < sapply(lvl1.mice[[m]][[p]], mean, na.omit =T)),nm=names(miss.prob))), nm = miss.mech.vec)
# setNames(lapply(1:length(miss.mech.vec), function(m) 
#   setNames(lapply(seq_along(miss.prob), function(p) 
#     sapply(lvl1.bnrc[[m]][[p]], mean, na.omit =T) < sapply(lvl1.bn[[m]][[p]], mean, na.omit =T)),nm=names(miss.prob))), nm = miss.mech.vec)
# 
# #### Latex table representation: 
# 
# table.imp <- list("BN" = lvl1.bn, "BNRC" = lvl1.bnrc, "MICE" = lvl1.mice)
# lvl1.table <- make.lvl1.table()
# 
# 
# #output2 <- output[1:24,]
# #table.names2 <- c("Residence", "", "Estate", "", "Assets", "", "Build. Loan", "", "Life Ins.", 
# #                  "", "Business Ass.", "", "Vehicles", "", "Tangibles", "",
# #                  "Res. debt", "", "Est. debt", "", "Consumer debt", "", "Student debt", "")
# #rownames(output2) <- table.names2
# 
# 
# #stargazer(output2, digits = 4, title = "Comparison of BNimp and MICE recovering the marginal continuous distributions",
# #          out = "level1cont.tex", colnames = T, notes = "Source: SOEP v36; Author's calculations. Mean value over 100 Monte Carlo draws. Monte Carlo Error in brackets")
# 
# 
# ##### 2nd. Levels of Statistical Consistency: continuous vars####
# 
# # potentially leave out... will do the same with more covariates later:
# #lvl2.cont.bn <- bd.cont(data = bn.imp)
# #lvl2.cont.bnrc <- bd.cont(data = bnrc)
# #lvl2.cont.mice <- bd.cont(data = mice.imp.complete)
# 
# #lapply(1:length(miss.mech.vec), function(m) mean(lvl2.cont.bn[[m]])<mean(lvl2.cont.mice[[m]]))
# #lapply(1:length(miss.mech.vec), function(m) mean(lvl2.cont.bnrc[[m]])<mean(lvl2.cont.bn[[m]]))
# 
# ##### 1st. Levels of Statistical Consistency: discrete vars ####
# 
discrete.imp.vars <- c("education", "superior", "compsize")
# 
# lvl1.discrete.bn <- misclass.error(data = bn.imp)
# lvl1.discrete.bnrc <- misclass.error(data = bnrc)
# lvl1.discrete.mice <- misclass.error(data = mice.imp.complete)
# 
# #### Latex Tables 1. level discrete vars
# 
# table.disc.imp <- list("BN" = lvl1.discrete.bn, "BNRC" = lvl1.discrete.bnrc, "MICE" = lvl1.discrete.mice)
# lvl1.misclass <- make.lvl1.misclass.table()
# #stargazer(output, digits = 4, title = "Comparison of BNimp and MICE recovering the true classification of discrete variables",
# #          out = "level1disc.tex", colnames = T, notes = "Source: SOEP v36; Author's calculations. Misclassification error mean value over 100 Monte Carlo draws. Simulation standard errors in brackets")
# 
# ##### 1st. Levels of Statistical Consistency: discrete and continuous vars ####
# 
# #lvl2.bn <- bd.full(data=bn.imp)
# #lvl2.bnrc <- bd.full(data=bnrc)
# #lvl2.mice <- bd.full(data=mice.imp.complete)

print("beginning with ball test")

lvl2.bn <- setNames(lapply(1:length(miss.mech.vec), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:k, function(l) bd.test(x = dplyr::select(bn.imp[[m]][[p]][[l]], 
     dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth, 
      dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)),
        nm= names(miss.prob))), nm = miss.mech.vec)
print("bnimp ball is played")

lvl2.bnrc <- setNames(lapply(1:length(miss.mech.vec), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:k, function(l) bd.test(x = dplyr::select(bnrc[[m]][[p]][[l]], 
      dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth, 
         dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)),
          nm= names(miss.prob))), nm = miss.mech.vec)

print("bnrc ball is in goal")

lvl2.mice <- setNames(lapply(1:length(miss.mech.vec), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:k, function(l) bd.test(x = dplyr::select(mice.imp.complete[[m]][[p]][[l]], 
     dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth, 
      dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)),
        nm= names(miss.prob))), nm = miss.mech.vec)

print("mice ball alea iacta est")

save(lvl2.bn,file = paste(mypath,"bd_bn.RDA", sep = "/"))
save(lvl2.bnrc,file = paste(mypath,"bd_bnrc.RDA", sep = "/"))
save(lvl2.mice,file = paste(mypath,"bd_mice.RDA", sep = "/"))


# pboptions(type = "txt")
# bntesting <- pbsapply(1:k, function(l) bd.test(x = dplyr::select(bn.imp[[2]][[1]][[l]],
#                         dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth,
#                         dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)
# bnrctesting <- pbsapply(1:k, function(l) bd.test(x = dplyr::select(bnrc[[2]][[1]][[l]],
#                        dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth,
#                        dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)
# micetesting <- pbsapply(1:k, function(l) bd.test(x = dplyr::select(mice.imp.complete[[2]][[1]][[l]],
#                        dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth,
#                        dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)
# 
# boxplotdata <- data.frame(ball_d = c(bntesting, bnrctesting, micetesting),
#                 Method = factor(c(rep(1,k), rep(2,k), rep(3,k)), ordered = F, labels= c("BN.imp", "BNRC.imp", "MICE.imp")))
# 
# box <- ggplot(boxplotdata, aes(x=Method, y=ball_d)) +
#        geom_boxplot(aes(group=Method, color = Method)) +
#        xlab("") +
#        theme(legend.position = "bottom") +
#        ylab("Ball divergence") +
#        ggtitle("MNAR Statistical constistency given 10% missing occurance")
# 
# bntestingmar <- pbsapply(1:k, function(l) bd.test(x = dplyr::select(bn.imp[[3]][[2]][[l]],
#                                                                  dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth,
#                                                                                                                                            dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)
# bnrctestingmar <- pbsapply(1:k, function(l) bd.test(x = dplyr::select(bnrc[[3]][[2]][[l]],
#                                                                    dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth,
#                                                                                                                                              dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)
# micetestingmar <- pbsapply(1:k, function(l) bd.test(x = dplyr::select(mice.imp.complete[[3]][[2]][[l]],
#                                                                    dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth,
#                                                                                                                                              dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)
# 
# boxplotdatamar <- data.frame(ball_d = c(bntestingmar, bnrctestingmar, micetestingmar),
#                           Method = factor(c(rep(1,k), rep(2,k), rep(3,k)), ordered = F, labels= c("BN.imp", "BNRC.imp", "MICE.imp")))
# 
# boxmar <- ggplot(boxplotdatamar, aes(x=Method, y=ball_d)) +
#   geom_boxplot(aes(group=Method, color = Method)) +
#   xlab("") +
#   theme(legend.position = "bottom") +
#   ylab("Ball divergence") +
#   ggtitle("MAR Statistical constistency given 20% missing occurance")


#### plotting mean over repetitions:

#bnrc.mean <- setNames(lapply(1:length(miss.mech.vec), function(m)
#              as.data.frame(sapply(seq_along(miss.prob), function(p)
#                sapply(1:k, function(i)
#                  mean(lvl2.bnrc[[m]][[p]][1:i]))))),nm=miss.mech.vec)
#for (m in 1:length(miss.mech.vec)){
#  colnames(bnrc.mean[[m]]) <- names(miss.prob)
#}

# bn.mean <- summ.reps(data = lvl2.bn)
# bnrc.mean <- summ.reps(data = lvl2.bnrc)
# mice.mean <- summ.reps(data = lvl2.mice)
# reps <- 1:k

#try boxplot:
#boxplotdata <- setNames(lapply(1:length(miss.mech.vec), function(m)
#                setNames(lapply(seq_along(miss.prob), function(p)
#                  data.frame(ball_d = c(bn.mean[[m]][[p]], bnrc.mean[[m]][[p]],mice.mean[[m]][[p]]),
#                             Method = factor(c(rep(1,k), rep(2,k), rep(3,k)), ordered = F, labels= c("BN.imp", "BNRC.imp", "MICE.imp")))), 
#                             nm = names(miss.prob))), nm = miss.mech.vec)


#box <-  setNames(lapply(1:length(miss.mech.vec), function(m)
#        lapply(seq_along(miss.prob), function(p)
#        ggplot(boxplotdata[[m]][[p]], aes(x=Method, y=ball_d)) +
#        geom_boxplot(aes(group=Method, color = Method)) +
#        xlab("") +
#        theme(legend.position = "bottom") +
#        ylab("Ball divergence"))), nm = miss.mech.vec) 
#        #ggtitle(paste("Statistical constistency given", 10*p ,"% missing occurance")))

#plot_grid(box[[1]][[1]], box[[1]][[2]], box[[1]][[3]],
#          box[[2]][[1]], box[[2]][[2]], box[[2]][[3]],
#          box[[3]][[1]], box[[3]][[2]], box[[3]][[3]],
#          labels = c("10% missing", "20% missing", "30% missing"), ncol = 3, nrow = 3)



#setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.bn[[m]]) < mean(lvl2.mice[[m]])), nm = miss.mech.vec)
#setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.bnrc[[m]]) < mean(lvl2.mice[[m]])), nm = miss.mech.vec)

#setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.bn[[m]])), nm = miss.mech.vec)
#setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.mice[[m]])), nm = miss.mech.vec)
#setNames(lapply(1:length(miss.mech.vec), function(m) mean(lvl2.bnrc[[m]])), nm = miss.mech.vec)


### Make table for presentation:

#presentation <- select(data, one_of("age", "sex",wealth.vars))
#presentation$female <- as.numeric(presentation$sex) -1
#presentation <- presentation[,c("age", "female",wealth.vars)]
#stargazer(presentation, out = "summarywealth.tex", notes = "SOEPv36 - TopW Data; Author's calculations")




