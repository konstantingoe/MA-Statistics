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

from <- c(filters, rep("lmstatus", 6))
to <- c(lnwealthvars, "lnjobduration", "lnworkinghours", "lnwage_gross", "lnwage_net", "compsize", "superior")
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

cond.vector <- c(lnrecode.vars, "lnhhnetto", "schoolingF", "schoolingM", "trainingF", "trainingM", "compsize", "superior", "education")

#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
                          ##### Simulation ####
#### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

data <- multiple.imp
test <- select(test, -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))

gg_miss_var(test, show_pct = T)
patterns.vars <- c(rerecode.vars, "hhnetto", "schoolingF", "schoolingM", "trainingF", "trainingM", "compsize", "superior", "education")
patterns <- md.pattern(select(data, one_of(patterns.vars)))
patterns <- patterns[-nrow(patterns),]
patterns <- patterns[,-ncol(patterns)]


other.vars <- setdiff(names(test), cond.vector)
colnames(patterns.others) <- other.vars
patterns.others <- matrix(data = 1, nrow = nrow(patterns), ncol = length(other.vars))

patterns.final <- as.data.frame(cbind(patterns.others, patterns)) 

result <- ampute(test, prop = .1, bycases = T, patterns = patterns.final, mech = "MAR")
gg_miss_var(result$amp, show_pct = TRUE)



mar <- produce_NA(test[,cond.vector], mechanism="MAR", perc.missing = 0.1)



# MNAR


gg_miss_var(data1, show_pct = TRUE)
data1 <- select(data, one_of(cond))  

prop.m = .1  # 10% missingness
sort.data <- NULL
nmar <- NULL
data.nmar <- NULL
for (i in 1:ncol(data1)){
  if (is.numeric(data1[,i])){ 
    sort.data <-  sort(data1[data1[,i] != -99,i], decreasing=TRUE)
    nmar   <-  sort.data[ceiling(prop.m*length(data1[,i]))]
    data1[,i] <-  ifelse(data1[,i] > nmar, NA, data1[,i]) 
  } else {
    
  }
}

sort.y = apply(data1, 2, sort, decreasing=TRUE)
nmar   = sort.y[ceiling(prop.m*length(y))]
y.nmar = ifelse(y>nmar, NA, y)  # doesn't show up when heavier


# Generate the true data
y1 <- rbinom(100, size=1, prob=0.1)
# Generate the missing process. Depends on the "true" observed value
r  <- rbinom(length(y1), size=1, prob=c(.1, .1)[y1+1])
y  <- y1
y[r==1] <- NA


p <- .1 

make.mnar <- function(data, prob=prob, cond = NULL){
  data1 <- select(data, one_of(cond))  
  for (i in 1: ncol(data1)){
    if (is.numeric(data1[,i])){ 
      data1[data1[,i] != -99,i][data1[data1[,i] != -99,i] > median(data1[data1[,i] != -99,i])] = ifelse(sample(c(T, F),
                              length(data1[data1[,i] != -99,i][data1[data1[,i] != -99,i] > median(data1[data1[,i] != -99,i])]),
                              replace=T, prob=c(p*2, 1-p*2)),
                              NA,
                              data1[data1[,i] != -99,i][data1[data1[,i] != -99,i] > median(data1[data1[,i] != -99,i])])
    } else {
      print("still gotta work on that")
    }
  }
  data <- cbind(select(data, -cond),data1)
  return(data)
}



sum(is.na(data1))/length(data1[data1 != -99])



data <- multiple.imp
test <- make.mnar(data, prob = .1, cond = cond.vector)
test <- select(test, -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))

gg_miss_var(test, show_pct = TRUE)











k <- 5
set.seed(1234)
numCores <- detectCores() -1

mi.multiple.imp <-  lapply(1:k, function(l) make.mcar(multiple.imp, prob=.1, cond = cond.vector))
for (l in 1:k) { 
  for (i in 1:length(lnrecode.vars)){
    mi.multiple.imp[[l]][,lnrecode.vars[i]] <- ifelse(mi.multiple.imp[[l]][,lnrecode.vars[i]] == -99, 
                                                 -log(mean(1+multiple.imp[,rerecode.vars[i]], na.rm = T))/log(sd(1+multiple.imp[,rerecode.vars[i]], na.rm =T)),
                                                 mi.multiple.imp[[l]][,lnrecode.vars[i]])
  }
  mi.multiple.imp[[l]] <- select(mi.multiple.imp[[l]], -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))
} 

  
#mi.structure <- structural.em(mi.multiple.imp[,names(mi.multiple.imp) != "pid"], maximize = "hc",
#                                fit = "mle", maximize.args = list(score = "bic-cg", whitelist = whitelist) , impute = "bayes-lw", max.iter = 5, return.all = T) 

# superior is the bad guy!!!! 

#mi.multiple.imp[[6]][,discrete.imp.vars] <- NULL 
mi.structure <- mclapply(1:k, function(l) structural.em(mi.multiple.imp[[l]][,names(mi.multiple.imp[[l]]) != "pid"], maximize = "hc",
                              fit = "mle", maximize.args = list(score = "bic-cg", whitelist = whitelist) , impute = "bayes-lw", max.iter = 2, return.all = T), mc.cores = numCores)


sapply(1:k, function(l) unlist(bnlearn::compare(truth.structure, mi.structure[[l]]$dag)))

for (l in 1:k){
  rel_label <- miss_var_summary(mi.multiple.imp[[l]][,names(mi.multiple.imp[[l]]) != "pid"], order = T)
  reliability <- rel_label$pct_miss
  names(reliability) <- rel_label$variable
  arc.reversal(object = mi.structure[[l]]$dag)
}
bn <-  lapply(1:k, function(l) bn.fit(mi.structure[[l]]$dag, mi.structure[[l]]$imputed, method = "mle"))

bn.imp <- mclapply(1:k, function(l) bn.parents.imp(bn=bn[[l]], 
                         dat=mi.multiple.imp[[l]]), mc.cores = numCores)


#### MICE ####
for (l in 1:k){
mi.multiple.imp[[l]] <- mi.multiple.imp[[l]] %>% 
  mutate(employed = ifelse(lmstatus %in% c(1,2,4), 1,0))
}
imp <- mice(mi.multiple.imp[[1]], maxit = 0, print=F)
meth <- imp$method
pred <- imp$predictorMatrix
rel_label <- miss_var_summary(mi.multiple.imp[[1]][,names(mi.multiple.imp[[1]]) != "pid"], order = T)
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


mice.imp <- mclapply(1:k, function(l) mice(mi.multiple.imp[[l]], maxit = 15, predictorMatrix = pred ,print=F, m=1), mc.cores = numCores)

mice.imp.complete <- lapply(1:k, function(l) mice::complete(mice.imp[[l]],action="long"))


##### 1st. Levels of Statistical Consistency: continuous vars ####

continuous.imp.vars <- c(lnrecode.vars, "lnhhnetto")
lvl1.bn <- sapply(1:k,
                  function(l) sapply(1:length(continuous.imp.vars),
                      function(i) 
                        ks.test(bn.imp[[l]][,continuous.imp.vars[i]],truth[,continuous.imp.vars[i]])$statistic))
lvl1.bn <- as.data.frame(t(lvl1.bn))
colnames(lvl1.bn) <- continuous.imp.vars

lvl1.mice <- sapply(1:k,
                    function(l) 
                      sapply(1:length(continuous.imp.vars), function(i) 
                        ks.test(mice.imp.complete[[l]][,continuous.imp.vars[i]],truth[,continuous.imp.vars[i]])$statistic))
lvl1.mice <- as.data.frame(t(lvl1.mice))
colnames(lvl1.mice) <- continuous.imp.vars

sapply(lvl1.bn, mean, na.omit =T) < sapply(lvl1.mice, mean, na.omit =T)

##### 2nd. Levels of Statistical Consistency: continuous vars####

lvl2.bn <- sapply(1:k, function(l) 
  bd.test(bn.imp[[l]][,continuous.imp.vars],truth[,continuous.imp.vars])$statistic)

lvl2.mice <- sapply(1:k, function(l) 
  bd.test(mice.imp.complete[[l]][,continuous.imp.vars],truth[,continuous.imp.vars])$statistic)

mean(lvl2.bn)<mean(lvl2.mice)

##### 1st. Levels of Statistical Consistency: discrete vars ####

discrete.imp.vars <- c("trainingF", "trainingM", "schoolingF", "schoolingM", "education", "superior", "compsize")
lvl1.discrete.bn <- sapply(1:k,
                           function(l) sapply(1:length(discrete.imp.vars),
                            function(i) 1-sum(diag(table(bn.imp[[l]][,discrete.imp.vars[i]],
                                                truth[,discrete.imp.vars[i]])))/sum(table(bn.imp[[l]][,discrete.imp.vars[i]]
                                                   ,truth[,discrete.imp.vars[i]]))))
lvl1.discrete.bn <- as.data.frame(t(lvl1.discrete.bn))
colnames(lvl1.discrete.bn) <- discrete.imp.vars

lvl1.discrete.mice <- sapply(1:k,
                           function(l) sapply(1:length(discrete.imp.vars),
                                              function(i) 1-sum(diag(table(mice.imp.complete[[l]][,discrete.imp.vars[i]],
                                                                           truth[,discrete.imp.vars[i]])))/sum(table(mice.imp.complete[[l]][,discrete.imp.vars[i]]
                                                                                                                     ,truth[,discrete.imp.vars[i]]))))
lvl1.discrete.mice <- as.data.frame(t(lvl1.discrete.mice))
colnames(lvl1.discrete.mice) <- discrete.imp.vars

sapply(lvl1.discrete.bn, mean) < sapply(lvl1.discrete.mice, mean)

##### 1st. Levels of Statistical Consistency: discrete and continuous vars ####

lvl2.discrete.bn <- sapply(1:k, function(l) bd.test(x = select(bn.imp[[l]], 
                            one_of(continuous.imp.vars, discrete.imp.vars)), y = select(truth, 
                              one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)

lvl2.discrete.mice <- sapply(1:k, function(l) bd.test(x = select(mice.imp.complete[[l]], 
                            one_of(continuous.imp.vars, discrete.imp.vars)), y = select(truth, 
                              one_of(continuous.imp.vars, discrete.imp.vars)))$statistic)

lvl2.discrete.bn
lvl2.discrete.mice

mean(lvl2.discrete.bn) < mean(lvl2.discrete.mice)

mean(lvl2.discrete.bn)
mean(lvl2.discrete.mice)
