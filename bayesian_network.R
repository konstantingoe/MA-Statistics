####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
                          ##### Bayesian Networks #####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
#mypath<- "/soep/kgoebler/data"
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

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
###### Preparing Monte Carlo Study #####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

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


####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
                  ###### Generate the Truth  ########
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

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
vertexlabels <- paste("X",1:62, sep = "")

bnplot <- ggnet2(truth.structure$arcs,
                 arrow.size = 7, arrow.gap = 0.027, node.size = 12.9)
original.names <- bnplot$data$label

bnplot <- ggnet2(truth.structure$arcs,
       arrow.size = 7, arrow.gap = 0.027, label = vertexlabels, node.size = 12.9)

ggsave("truthstruct.pdf", plot = bnplot)

explanation <- cbind(vertexlabels,original.names)

stargazer(explanation, out = "vertexnames.tex", label = "vertex", digits=2, title = "Naming conventions of vertices in the \\textit{true} DAG learned on Sample $P_{\\text{beta}}$")


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
numCores <- detectCores() -3
plan(multiprocess, workers = numCores)
k <- 500

miss.mechanism <- list("MCAR" = make.mcar, "MNAR" = make.mnar)
miss.mechanism2 <- list("MAR" = make.mar)
miss.mech.vec <- c("MCAR", "MNAR", "MAR")
miss.prob <- list("0.1" = .1, "0.2" = .2, "0.3" = .3)

mi.multiple.imp <-  setNames(lapply(seq_along(miss.mechanism), function(m)
                      setNames(lapply(seq_along(miss.prob), function(p)
                        lapply(1:k, function(l) miss.mechanism[[m]](multiple.imp, miss.prob=miss.prob[[p]], cond = cond.vector))), nm= names(miss.prob))), nm=names(miss.mechanism))
mi.multiple.imp <- c(mi.multiple.imp, setNames(lapply(seq_along(miss.mechanism2), function(m)
                    setNames(lapply(seq_along(miss.prob), function(p)
                      lapply(1:k, function(l) miss.mechanism2[[m]](multiple.imp, miss.prob = miss.prob[[p]], cond = cond.vector, x.vars = x.vars))), nm= names(miss.prob))), nm=names(miss.mechanism2)))



for (m in 1:length(miss.mech.vec)){
  for (p in 1:length(miss.prob)){
    for (l in 1:k) {
      for (i in 1:length(lnrecode.vars)){
        mi.multiple.imp[[m]][[p]][[l]][,lnrecode.vars[i]] <- ifelse(mi.multiple.imp[[m]][[p]][[l]][,lnrecode.vars[i]] == -99,
                                                     -log(mean(1+multiple.imp[,rerecode.vars[i]], na.rm = T))/log(sd(1+multiple.imp[,rerecode.vars[i]], na.rm =T)),
                                                     mi.multiple.imp[[m]][[p]][[l]][,lnrecode.vars[i]])
      }
      mi.multiple.imp[[m]][[p]][[l]] <- select(mi.multiple.imp[[m]][[p]][[l]], -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))
    }
  }
}

save(mi.multiple.imp, file = paste(mypath, "data.RDA", sep = "/"))
#load(paste(mypath, "data.RDA", sep = "/"))

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
                      #### BN imputations  ####
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

mi.structure <- setNames(lapply(1:length(miss.mech.vec), function(m)
                  setNames(lapply(seq_along(miss.prob), function(p)
                    future_lapply(future.seed = T, 1:k, function(l) structural.em(mi.multiple.imp[[m]][[p]][[l]][,names(mi.multiple.imp[[m]][[p]][[l]]) != "pid"], maximize = "hc",
                          fit = "mle", maximize.args = list(score = "bic-cg", whitelist = whitelist) , impute = "bayes-lw", max.iter = 2, return.all = T))),
                           nm= names(miss.prob))), nm = miss.mech.vec)

save(mi.structure, file = paste(mypath, "structure.RDA", sep = "/"))
#load(paste(mypath, "structure.RDA", sep = "/"))

#dag.compare <- lapply(1:length(miss.mech.vec), function(m) lapply(1:length(miss.prob), function(p) 
#  sapply(1:k, function(l) unlist(bnlearn::compare(truth.structure, mi.structure[[m]][[p]][[l]]$dag)))))

mi.structure.rev <- mi.structure
 for (m in 1:length(miss.mech.vec)){
  for (p in 1:length(miss.prob)){
    for (l in 1:k){
      rel_label <- miss_var_summary(mi.multiple.imp[[m]][[p]][[l]][,names(mi.multiple.imp[[m]][[p]][[l]]) != "pid"], order = T)
      reliability <- rel_label$pct_miss
      names(reliability) <- rel_label$variable
      arc.reversal(object = mi.structure.rev[[m]][[p]][[l]]$dag)
    }
  }
}
 
bn <-  setNames(lapply(1:length(miss.mech.vec), function(m)
        setNames(lapply(seq_along(miss.prob), function(p)
          future_lapply(future.seed = T, 1:k, function(l) bn.fit(mi.structure[[m]][[p]][[l]]$dag, mi.structure[[m]][[p]][[l]]$imputed, method = "mle"))),
           nm= names(miss.prob))), nm = miss.mech.vec)

save(bn, file = paste(mypath, "bn.RDA", sep = "/"))
#load(paste(mypath, "bn.RDA", sep = "/"))

bn.imp <- setNames(lapply(1:length(miss.mech.vec), function(m)
            setNames(lapply(seq_along(miss.prob), function(p)
              future_lapply(future.seed = T, 1:k, function(l) bn.parents.imp(bn=bn[[m]][[p]][[l]], dag = mi.structure.rev[[m]][[p]][[l]]$dag,
                dat=mi.multiple.imp[[m]][[p]][[l]]))), nm=names(miss.prob))),nm=miss.mech.vec)

save(bn.imp, file = paste(mypath, "bnimp.RDA", sep = "/"))

# Initiating BN_MBRC algorithm 
bnrc.nm50 <- setNames(lapply(1:length(miss.mech.vec), function(m)
          setNames(lapply(seq_along(miss.prob), function(p)
            future_lapply(future.seed = T, 1:k, function(l) bnrc.nomean(bn=bn[[m]][[p]][[l]],
              data=mi.multiple.imp[[m]][[p]][[l]], cnt.break = 50, returnfull = F))),
                nm= names(miss.prob))), nm = miss.mech.vec)
save(bnrc.nm50, file = paste(mypath, "bnrc_nmimp50.RDA", sep = "/"))


#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
                              #### MICE ####
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

for (m in 1:length(miss.mech.vec)){
  for (p in 1:length(miss.prob)){
    for (l in 1:k){
    mi.multiple.imp[[m]][[p]][[l]] <- mi.multiple.imp[[m]][[p]][[l]] %>%
      mutate(employed = ifelse(lmstatus %in% c(1,2,4), 1,0))
    }
  }
}

post.mice <- list()
pred.mice <- list()
for (m in 1:length(miss.mech.vec)){
  imp <- mice(mi.multiple.imp[[m]][[1]][[1]], maxit = 0, print=F)
  meth <- imp$method
  pred <- imp$predictorMatrix
  rel_label <- miss_var_summary(mi.multiple.imp[[m]][[1]][[1]][,names(mi.multiple.imp[[m]][[1]][[1]]) != "pid"], order = T)
  reliability <- rel_label$pct_miss
  names(reliability) <- rel_label$variable

  indepvars.all <- c("age", "sex", "ost", "bik", "wuma7", "employed", "inherit_filter",
                     "citizen", "famstd", "gborn", "kidsu16", "partner", "saving", "lnsaving", 
                     "lninheritance", "lnhhnetto", "compsize",
                     "education")
  
  miss <- names(reliability[reliability>0])
  pred[,] <- 0
  
  for (i in 1:length(miss)){
    pred[miss[i], indepvars.all[indepvars.all != miss[i]]] <- 1
  }
  pred["superior","employed"] <- 0
  pred[lnwealthvars, c("lnorbis")] <- 1
  
  # more variables for residence variables:
  indepvars.residence <- c("sex", "ost", "lnwage_gross", "lnwage_net", "lnresidence_debt", "lnestate_debt",
                           "lnvehicles", "partner", "age", "lnlife", "lninheritance", "bik", "wuma7")
  indepvars.residence.debt <- c("sex", "ost", "lnwage_gross", "lnwage_net", "lnresidence", "lnestate",
                                "lnvehicles", "partner", "age", "lnlife", "lninheritance", "bik", "wuma7")
  pred["lnresidence_debt", c(indepvars.residence.debt, "lnestate_debt")] <- 1
  pred["lnestate_debt", c(indepvars.residence.debt, "lnresidence_debt")] <- 1
  pred["lnresidence", c(indepvars.residence, "lnestate")] <- 1
  pred["lnestate", c(indepvars.residence, "lnresidence")] <- 1
  pred["consumer_debt_filter", c(indepvars.residence, "lnresidence", "lnestate")] <- 1
  pred["lninheritance", c("lntangibles", "lnresidence", "lnestate")] <- 1
  pred["lnbuilding", c(indepvars.residence, "lnresidence", "lnestate")] <- 1
  
  #### specify possible range for logical constraints: Has to be hardcoded
  post <- make.post(mi.multiple.imp[[m]][[1]][[1]])
  
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
  
  
  post.mice[[m]] <- post
  pred.mice[[m]] <- pred
}

mice.imp <- setNames(lapply(1:length(miss.mech.vec), function(m)
              setNames(lapply(seq_along(miss.prob), function(p)
                future_lapply(future.seed = T, 1:k, function(l) mice(mi.multiple.imp[[m]][[p]][[l]], maxit = 15, predictorMatrix = pred.mice[[m]], post = post.mice[[m]], print=F, m=1))),
                  nm=names(miss.prob))), nm=miss.mech.vec)

mice.imp.complete <- setNames(lapply(1:length(miss.mech.vec), function(m)
                      setNames(lapply(seq_along(miss.prob), function(p) 
                        lapply(1:500, function(l) mice::complete(mice.imp[[m]][[p]][[l]],action="long"))),
                          nm=names(miss.prob))),nm = miss.mech.vec)


save(mice.imp, file = paste(mypath,"mice.RDA", sep = "/"))
save(mice.imp.complete, file = paste(mypath,"micedata.RDA", sep = "/"))

#load(paste(mypath, "bnimp.RDA", sep = "/"))
#äload(paste(mypath, "bnrcimp.RDA", sep = "/"))


####++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
##### 1st. Levels of Statistical Consistency: continuous vars ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

continuous.imp.vars <- c(lnrecode.vars, "lnhhnetto")

lvl1.bn <- ks.list(data = bn.imp)
lvl1.bnrc_nm <- ks.list(data = bnrc.nm50)
lvl1.mice <- ks.list(data = mice.imp.complete)

#### Latex table representation:

### MCAR, MAR and MNAR table:

mcartable <- mcarlvltbl(missmech = "MCAR")
martable <-  mcarlvltbl(missmech = "MAR")
mnartable <- mcarlvltbl(missmech = "MNAR")

### Table for thesis body 

stargazer(mcartable[1:36,], title = "Comparison of BN algorithms and MICE recovering the marginal continuous distributions with MCAR data",
           out = "level1mcar.tex", colnames = T, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. Mean of KS distances over 500 Monte Carlo draws. Monte Carlo Error in brackets and underneath the test power.")

stargazer(martable[1:36,], title = "Comparison of BN algorithms and MICE recovering the marginal continuous distributions with MAR data",
          out = "level1mar.tex", colnames = T, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. Mean of KS distances over 500 Monte Carlo draws. Monte Carlo Error in brackets and underneath the test power.")

stargazer(mnartable[1:36,], title = "Comparison of BN algorithms and MICE recovering the marginal continuous distributions with MNAR data",
          out = "level1mnar.tex", colnames = T, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. Mean of KS distances over 500 Monte Carlo draws. Monte Carlo Error in brackets and underneath the test power.")

### Appendix Tables: 

stargazer(mcartable[37:57,], title = "Comparison of BN algorithms and MICE recovering the marginal continuous distributions with MCAR data on the remaining non-wealth-items",
          out = "level1appendix_mcar.tex", colnames = T, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. Mean of KS distances over 500 Monte Carlo draws. Monte Carlo Error in brackets and underneath the test power.")

stargazer(martable[37:57,], title = "Comparison of BN algorithms and MICE recovering the marginal continuous distributions with MAR data on the remaining non-wealth-items",
          out = "level1appendix_mar.tex", colnames = T, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. Mean of KS distances over 500 Monte Carlo draws. Monte Carlo Error in brackets and underneath the test power.")

stargazer(mnartable[37:57,], title = "Comparison of BN algorithms and MICE recovering the marginal continuous distributions with MNAR data on the remaining non-wealth-items",
          out = "level1appendix_mnar.tex", colnames = T, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. Mean of KS distances over 500 Monte Carlo draws. Monte Carlo Error in brackets and underneath the test power.")


####++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
##### 1st. Levels of Statistical Consistency: discrete vars ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

discrete.imp.vars <- c("education", "superior", "compsize")

lvl1.discrete.bn <- misclass.error(data = bn.imp)
lvl1.discrete.bnrc <- misclass.error(data = bnrc.nm50)
lvl1.discrete.mice <- misclass.error(data = mice.imp.complete)

#### Latex Tables 1. level discrete vars

table.disc.imp <- list("BN" = lvl1.discrete.bn, "BNRC" = lvl1.discrete.bnrc, "MICE" = lvl1.discrete.mice)

lvl1.misclass <- make.lvl1.misclass.table()

stargazer(lvl1.misclass, digits = 4, title = "Comparison of BNimp and MICE recovering the true classification of discrete variables",
          out = "level1disc.tex", colnames = T, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. MR mean value over 500 Monte Carlo draws. Monte Carlo Error in brackets")


####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
##### 2nd. Levels of Statistical Consistency: discrete and continuous vars ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

print("beginning with BD test")

lvl2.bn <- setNames(lapply(1:length(miss.mech.vec), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:k, function(l) bd.test(x = dplyr::select(bn.imp[[m]][[p]][[l]], 
     dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth, 
      dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))[["complete.info"]])),
        nm= names(miss.prob))), nm = miss.mech.vec)

lvl2.bnrc <- setNames(lapply(1:length(miss.mech.vec), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:k, function(l) bd.test(x = dplyr::select(bnrc[[m]][[p]][[l]], 
      dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth, 
         dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))[["complete.info"]])),
          nm= names(miss.prob))), nm = miss.mech.vec)

lvl2.mice <- setNames(lapply(1:length(miss.mech.vec), function(m)
  setNames(lapply(seq_along(miss.prob), function(p) 
    sapply(1:k, function(l) bd.test(x = dplyr::select(mice.imp.complete[[m]][[p]][[l]], 
     dplyr::one_of(continuous.imp.vars, discrete.imp.vars)), y = dplyr::select(truth, 
      dplyr::one_of(continuous.imp.vars, discrete.imp.vars)))[["complete.info"]])),
        nm= names(miss.prob))), nm = miss.mech.vec)

save(lvl2.bn,file = paste(mypath,"bd_bn.RDA", sep = "/"))
save(lvl2.bnrc,file = paste(mypath,"bd_bnrc.RDA", sep = "/"))
save(lvl2.mice,file = paste(mypath,"bd_mice.RDA", sep = "/"))

# load("bd_bn.RDA")
# load("bd_bnrc_nm5.RDA")
# load("bd_mice.RDA")

#### retrieveing mean and sd over repetitions:

lvl2.bn.bd <- lvl2.extract(data = lvl2.bn)
lvl2.bnrc.bd <- lvl2.extract(data = lvl2.bnrc.nm5)
lvl2.mice.bd <- lvl2.extract(data = lvl2.mice)

#### make latex table
level2table <- make.lvl2.table()
stargazer(level2table, title = "Ball divergence between BN imputation routines and MICE",
          out = "level2.tex", colnames = T, summary = F, notes = "Source: SOEP Sample $P_{\\text{beta}}$; Author's calculations. BD mean value over 500 Monte Carlo draws. Monte Carlo Error in brackets")

### plot boxplots
miss.mech.vec.new <- c("MCAR", "MAR", "MNAR")
labelsmath <- c("$BN_\\Pi$", "$BN_{MBRC}$", "MICE")
labl <- c("BN_\u03a0", "BN_MBRC", "MICE")

boxpd <- setNames(lapply(c(1,3,2), function(m)
                    setNames(lapply(seq_along(miss.prob), function(p)
                    data_frame(BD = c(lvl2.bn[[m]][[p]],lvl2.bnrc.nm5[[m]][[p]],lvl2.mice[[m]][[p]]),
                    Method = factor(c(rep(1, k), rep(2, k), rep(3, k)), ordered = F, labels= labl))),nm = names(miss.prob))),
                    nm = miss.mech.vec.new)
box <-    setNames(lapply(1:3, function(m)
          setNames(lapply(seq_along(miss.prob), function(p)
          ggplot(boxpd[[m]][[p]], aes(x=Method, y=BD)) +
          geom_boxplot(aes(group=Method, color = Method)) +
          xlab("") +
          scale_y_continuous(limits = c(0, .004)) +  
          theme(legend.position = "none") +
          ylab("Ball divergence") +
          ggtitle(paste("BD", 10*p,"% missings and", miss.mech.vec.new[m], "data")) +
          theme(plot.title = element_text(size = rel(1), vjust = 0), 
                  axis.title = element_text(size = rel(0.8)),
                  axis.title.y = element_text( vjust=2 ),
                  axis.title.x = element_text( vjust=-0.5))),
          nm = names(miss.prob))),nm = miss.mech.vec.new)


plot_grid(box[[1]][[1]], box[[1]][[2]], box[[1]][[3]],
          box[[2]][[1]], box[[2]][[2]], box[[2]][[3]],
          box[[3]][[1]], box[[3]][[2]], box[[3]][[3]], ncol = 3, nrow = 3)

ggsave("boxplot_level2.pdf")

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
                            ##### Analysis done ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####


####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####
                          ##### Additional tables ####
####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####

### Make additional tables for Thesis:
wealth.descr <- select(wealth.comp, one_of(wealth.vars, filters))

mat <- cbind(table(wealth.descr$owner),table(wealth.descr$owner)/nrow(wealth.descr)*100)
mis <- c(NaN, sum(is.na(wealth.descr$residence_value))/nrow(filter(wealth.descr, owner == 1))*100)
mat <- cbind(mat,mis) 
summar <- matrix(c(rep(NaN, 4), 
                 mean(filter(wealth.descr, owner == 1)$residence_value, na.rm = T), sd(filter(wealth.descr, owner == 1)$residence_value, na.rm = T), min(filter(wealth.descr, owner == 1)$residence_value, na.rm = T), max(filter(wealth.descr, owner == 1)$residence_value, na.rm = T)),
                 nrow = 2, ncol = 4, byrow = T)/1000
mat.d <- cbind(mat,summar)

for (i in 2:12){
  mat.f <- cbind(table(wealth.descr[,filters[i]]), table(wealth.descr[,filters[i]])/nrow(wealth.descr)*100)
  if (nrow(mat.f)==3){
    mat.f <- cbind(c(nrow(wealth.descr[wealth.descr[,filters[i]] != 1,]),nrow(wealth.descr[wealth.descr[,filters[i]] == 1,])), c(nrow(wealth.descr[wealth.descr[,filters[i]] != 1,])/nrow(wealth.descr)*100, nrow(wealth.descr[wealth.descr[,filters[i]] == 1,])/nrow(wealth.descr)*100))
  }
  mis.f <- c(NaN, sum(is.na(wealth.descr[,wealth.vars[i]]))/nrow(wealth.descr[wealth.descr[,filters[i]] == 1,])*100)
  mat.f <- cbind(mat.f,mis.f)
  summar.f <- matrix(c(rep(NaN, 4), 
                     mean(wealth.descr[wealth.descr[,filters[i]] == 1,][,wealth.vars[i]], na.rm = T), sd(wealth.descr[wealth.descr[,filters[i]] == 1,][,wealth.vars[i]], na.rm = T), min(wealth.descr[wealth.descr[,filters[i]] == 1,][,wealth.vars[i]], na.rm = T), max(wealth.descr[wealth.descr[,filters[i]] == 1,][,wealth.vars[i]], na.rm = T)),
                     nrow = 2, ncol = 4, byrow = T)/1000
  mat.df <- cbind(mat.f,summar.f)
  mat.d <- rbind(mat.d,mat.df)
}

colnames(mat.d) <- c("N", "Share (in %)", "Miss Share in (in %)", "Mean", "St. Dev.", "Min", "Max")
rownames(mat.d) <- c(rep(c("n/appl.", "appl."),12))

stargazer(mat.d, out = "summarywealth.tex", label = "descr", digits=1, title = "The twelve asset and debt components in 1,000€ as surveyed in the wealth module" , notes = "SOEPv36 - Sample $P_{\\text{beta}}$; Author's calculations")

#### Remaining Variables ####

### continuous variables
remaining.cont <- c("age", "orbis_wealth", "jobduration", "workinghours", "wage_gross_m", "wage_net_m", "sqmtrs", "hhnetto", "saving_value", "total_inheritance")
remaining.descr <- select(data, one_of(remaining.cont))

mat1 <- cbind(length(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] == -2 & !is.na(remaining.descr[remaining.cont[1]])]),length(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] == -2 & !is.na(remaining.descr[remaining.cont[1]])])/nrow(remaining.descr)*100)
mat2 <- cbind(length(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2]),length(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2])/nrow(remaining.descr)*100)
mat <- rbind(mat1,mat2)

mis <- c(NaN, sum(is.na(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2]))/length(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2])*100)
mat <- cbind(mat,mis) 

summar <- matrix(c(rep(NaN, 4), 
                 mean(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2], na.rm = T), sd(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2], na.rm = T), min(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2], na.rm = T), max(remaining.descr[,remaining.cont[1]][remaining.descr[remaining.cont[1]] != -2], na.rm = T)),
                 nrow = 2, ncol = 4, byrow = T)
mat.remain <- cbind(mat,summar)


for (i in 2:10){
  mat1 <- cbind(length(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] == -2 & !is.na(remaining.descr[remaining.cont[i]])]),length(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] == -2 & !is.na(remaining.descr[remaining.cont[i]])])/nrow(remaining.descr)*100)
  mat2 <- cbind(length(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2]),length(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2])/nrow(remaining.descr)*100)
  mat <- rbind(mat1,mat2)
  
  mis <- c(NaN, sum(is.na(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2]))/length(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2])*100)
  mat <- cbind(mat,mis) 
  
  summar <- matrix(c(rep(NaN, 4), 
                     mean(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2], na.rm = T), sd(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2], na.rm = T), min(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2], na.rm = T), max(remaining.descr[,remaining.cont[i]][remaining.descr[remaining.cont[i]] != -2], na.rm = T)),
                   nrow = 2, ncol = 4, byrow = T)
  mat.r <- cbind(mat,summar)
  mat.remain <- rbind(mat.remain,mat.r)
}

colnames(mat.remain) <- c("N", "Share (in %)", "Miss Share in (in %)", "Mean", "St. Dev.", "Min", "Max")
rownames(mat.remain) <- c(rep(c("n/appl.", "appl."),10))

#jobduration n/appl are partial retirement with zero hours, social year individuals and non-employed
# while for wage non-applicables its only social year and non-employed

stargazer(mat.remain, out = "summaryremain.tex", label = "remaindescr", digits=1, title = "Descriptives of the remaining continuous variables in Sample $P_{\\text{beta}}$" , notes = "SOEPv36 - Sample $P_{\\text{beta}}$; Author's calculations")


### discrete variables
remaining.discrete.vars <- setdiff(names(data),c(remaining.cont, wealth.vars, filters, "pid", "inherit_filter", "saving", "bula", "bik", "ggk", "hhtyp"))
remaining.discrete <- select(data, one_of(remaining.discrete.vars))

tab <- rbind(rep(NaN,2),cbind(table(remaining.discrete[,remaining.discrete.vars[1]])/nrow(remaining.discrete)*100, c(sum(is.na(remaining.discrete[,remaining.discrete.vars[1]]))/nrow(remaining.discrete)*100,rep(NaN, nlevels(remaining.discrete[,remaining.discrete.vars[1]]) -1))))

for (i in 2:length(remaining.discrete.vars)){
  tab.loop <- rbind(rep(NaN,2),cbind(table(remaining.discrete[,remaining.discrete.vars[i]])/nrow(remaining.discrete)*100, c(sum(is.na(remaining.discrete[,remaining.discrete.vars[i]]))/nrow(remaining.discrete)*100,rep(NaN, nlevels(remaining.discrete[,remaining.discrete.vars[i]]) -1))))
  tab <- rbind(tab,tab.loop)
}

colnames(tab) <- c("Share (in %)", "Missing Share (in %)")

varlables <- c("Sex","Male","Female",
               "West/East", "West","East",
               "House type", "Agricultural dwelling", "Freestanding dwelling", "Terraced house", "Small residential build.", "Medium residential build.", "Large residential build.", "High-rise building",
               "House condition", "Excellent", "Good", "Partly dilapidated", "Dilapidated",
               "Safety of residential area", "Very safe", "Safe", "Less safe", "Somewhat dangerous",
               "Number of household members", "1","2","3","4","5","6","7",
               "Training on the job (in 2018)", "No", "Yes",
               "Labor market status", "Full time employed", "Part time employed", "Traineeship", "Marginal employment", "Partial retirement", "Social year", "Non-employed",
               "Education required for job", "Not applicable", "None", "Vocational training", "Technical college", "Higher education",
               "Self-employment", "Not applicable", "No", "Yes",
               "Company size", "Not applicable", "Small", "Medium", "Large",
               "Superior", "Not applicable", "No", "Yes",
               "Overtime", "Not applicable", "Yes", "No", "No, b/c self-emnployment",
               "Secondary emplyoment", "No", "Yes",
               "German citizenship", "No", "Yes",
               "Marital status", "Single, never married", "Married/Life partnership", "Divorced", "Widowed",
               "Born in Germany", "No", "Yes", 
               "Education", "No completion", "Lower secondary", "Upper secondary", "Tertiary or higher",
               "Number of kids", "No kids", "1 kid", "2 kids", "3 or more kids",
               "Steady partner", "No", "Yes", 
               "Residential area", "Area with old buildings", "Area with new buildings", "Mixture of old and new buildings", "Commercial center", "Industrial area",
               "Kids under 16 in HH", "No", "Yes")

rownames(tab) <- varlables

stargazer(tab, out = "summarydiscrete.tex", label = "remaindiscrete", digits=2, title = "Descriptives of the remaining discrete variables in Sample $P_{\\text{beta}}$" , notes = "SOEPv36 - Sample $P_{\\text{beta}}$; Author's calculations")

####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#####






