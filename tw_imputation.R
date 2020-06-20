##### Bayesian Network #####

rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
#mypath<- "/soep/kgoebler/data"
mydata <- import(paste(path, "tw_cleaned.dta", sep = "/"))
set.seed(12)

#potentially <- set_na(mydata$residence_debt_filter, na =c("Does not apply" = -2), as.tag = T)
# first impute filter information and then based on these impute wealth components:

##### Filter Imputation


factorvars <- c("sex", "bula_sta", "job_training", "selfempl", "superior", "second_empl", "citizen", "famstd",
                "owner", "residence_debt_filter", "other_estate", "other_estate_debt_filter",
                "assets_filter", "building_contract_filter", "life_insure_filter", "business_holdings_filter",
                "vehicles_filter", "tangibles_filter", "consumer_debt_filter", "education_debt_filter",
                "gborn", "kidsu16", "partner", "inheritance_dummy", "emp_status", "labormarket_dummy")

orderedfactorvars <- c("hhgr", "bik", "ggk", "wuma3", "compsize", "overtime", "residence_value_limits",
                       "residence_debt_limits", "other_estate_value_limits", "other_estate_debt_limits", 
                       "assets_limits", "building_contract_limits", "life_insure_limits", "business_holdings_limits",
                       "vehicles_limits", "tangibles_limits", "consumer_debt_limits", "education_debt_limits",
                       "education", "housecond", "nkids")

continousvars <- setdiff(names(mydata),c(factorvars,orderedfactorvars))

mydata$hhgr <- ifelse(mydata$hhgr > 4, 4, mydata$hhgr)

for (i in 1:length(factorvars)){
  mydata[,factorvars[i]] <- factor(mydata[,factorvars[i]], ordered = F)
}

for (i in 1:length(orderedfactorvars)){
  mydata[,orderedfactorvars[i]] <- factor(mydata[,orderedfactorvars[i]], ordered = T)
}

gg_miss_var(mydata, show_pct = TRUE)

data <- mydata

#all except 12 wealth components plus inheritance and saving which will also be imputed when filter is imputed 

wealth.vars <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles",
                 "residence_debt", "other_estate_debt", "consumer_debt", "education_debt")
wealth.vars2 <- c(wealth.vars, "total_inheritance")

filters <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
             "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
             "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter")
filters2 <- c(filters, "inheritance_dummy")


filter <- dplyr::select(data, dplyr::one_of(setdiff(names(mydata), wealth.vars2)))

gg_miss_var(filter, show_pct = TRUE)


filterimp <- setdiff(names(filter), "pid")
# first impute filter variables 
filter.imp <- VIM::kNN(filter, variable = filterimp, dist_var = colnames(filter[, -which(names(filter) == "pid")]),
                       donorcond =  as.list(rep("!= -2",length(filterimp))), imp_var =F)


data[,filters2] <- filter.imp[,filters2]
gg_miss_var(data[,filters2], show_pct = TRUE)


wealth.perc <- c("owner_perc","other_estate_perc", "assets_perc")

wealth.limits <- c("residence_value_limits", "other_estate_value_limits", "assets_limits", "building_contract_limits",
                   "life_insure_limits", "business_holdings_limits", "vehicles_limits", "tangibles_limits", 
                   "residence_debt_limits", "other_estate_debt_limits", "consumer_debt_limits", "education_debt_limits")

wealth.comp <- data

### recode values that correspond to the filter logic just imputed: 

for (i in 1:length(filters2)){
  wealth.comp[,wealth.vars2[i]] <- ifelse(wealth.comp[,filters2[i]] == 0, -2, wealth.comp[,wealth.vars2[i]])
}

for (i in 1:length(filters)){
  wealth.comp[,wealth.limits[i]] <- ifelse(wealth.comp[,filters[i]] == 0, -2, wealth.comp[,wealth.limits[i]])
  wealth.comp[,wealth.limits[i]] <- factor(wealth.comp[,wealth.limits[i]], ordered = T)
}

for (i in 1:3){
  wealth.comp[,wealth.perc[i]] <- ifelse(wealth.comp[,filters[i]] == 0, -2, wealth.comp[,wealth.perc[i]])
}

gg_miss_var(wealth.comp, show_pct = TRUE)


#### recode all -2 to missings for transformation and then recode them to a new logical missing code
exceptions <- c("pid", "age", "hhgr", "sqmtrs", "hhnetto", wealth.perc, wealth.limits)

recode.vars <- setdiff(names(dplyr::select_if(wealth.comp, is.numeric)), exceptions)

for (i in 1:length(recode.vars)){
  wealth.comp[,recode.vars[i]] <- ifelse(wealth.comp[,recode.vars[i]] == -2, 0, wealth.comp[,recode.vars[i]])
}


multiple.imp <- wealth.comp %>% 
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
         lninheritance     = normalize.log(total_inheritance),
         lnsqmtrs          = normalize.log(sqmtrs),
         lnhhnetto         = normalize.log(hhnetto))

lnwealthvars <- c("lnresidence", "lnestate", "lnassets", "lnbuilding", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
                  "lnresidence_debt", "lnestate_debt", "lnconsumer_debt", "lnstudent_debt")

lnrecode.vars <- c(lnwealthvars, "lnjobduration",
                   "lnworkinghours", "lnwage_gross", "lninheritance")

rerecode.vars <- c(wealth.vars, "jobduration", "workinghours", "wage_gross_m", "total_inheritance")

from <- c(filters, wealth.limits, filters[1:3], rep("emp_status", 5), "inheritance_dummy")

to <- c(lnwealthvars, lnwealthvars, wealth.perc, "lnjobduration", "lnworkinghours", "lnwage_gross", "compsize", "superior", "lninheritance")
whitelist <- as.data.frame(cbind(from,to)) 


#bnplot <- ggnet2(truth.structure$arcs,
#       arrow.size = 9, arrow.gap = 0.025, label = T)
#ggsave("truthstruct.pdf", plot = bnplot)


mi.multiple.imp <- multiple.imp
mi.multiple.imp <- dplyr::select(mi.multiple.imp, -all_of(c(recode.vars, "sqmtrs", "hhnetto")))

gg_miss_var(mi.multiple.imp, show_pct = TRUE)

numCores <- detectCores() -1
plan(multisession, workers = numCores)

mi.structure <- bnlearn::structural.em(mi.multiple.imp[,names(mi.multiple.imp) != "pid"], maximize = "hc", maximize.args = list(score = "bic-cg", restart = 0, whitelist = whitelist), 
                                       fit = "mle", fit.args = list(replace.unidentifiable = T), impute = "bayes-lw", max.iter = 10, return.all = T, debug = F)

vertexlabels <- paste("X",1:(ncol(mi.multiple.imp) -1), sep = "")
bnplot <- ggnet2(mi.structure$dag$arcs,
                 arrow.size = 7, arrow.gap = 0.027, label = vertexlabels, node.size = 12.9)
#ggsave("truthstruct.pdf", plot = bnplot)

bn <-  bn.fit(mi.structure$dag, mi.structure$imputed, method = "mle")

bnrc <- bnrc.nomean(bn=bn, data=mi.multiple.imp, cnt.break = 15, returnfull = T)

bnimp <- bn.parents.imp(bn=bn, dag=mi.structure$dag, dat = mi.multiple.imp)

gg_miss_var(bnrc$finalData, show_pct = TRUE)

#final.log <- bnrc$finalData
# choose imputation method:

chooseimp <- bnrc$finalData 

chooseimp <- chooseimp %>% 
  mutate(residence_value_impflag = factor(ifelse(is.na(mi.multiple.imp$lnresidence),1,0), ordered = F),
         other_estate_impflag = factor(ifelse(is.na(mi.multiple.imp$lnestate),1,0), ordered = F),
         assets_impflag = factor(ifelse(is.na(mi.multiple.imp$lnassets),1,0), ordered = F),
         building_impflag = factor(ifelse(is.na(mi.multiple.imp$lnbuilding),1,0), ordered = F),
         lifeinsure_impflag = factor(ifelse(is.na(mi.multiple.imp$lnlife),1,0), ordered = F),
         business_impflag = factor(ifelse(is.na(mi.multiple.imp$lnbusiness),1,0), ordered = F),
         vehicles_impflag = factor(ifelse(is.na(mi.multiple.imp$lnvehicles),1,0), ordered = F),
         tangibles_impflag = factor(ifelse(is.na(mi.multiple.imp$lntangibles),1,0), ordered = F),
         residence_debt_impflag = factor(ifelse(is.na(mi.multiple.imp$lnresidence_debt),1,0), ordered = F),
         estate_debt_implag = factor(ifelse(is.na(mi.multiple.imp$lnestate_debt),1,0), ordered = F),
         consumerdebt_impflag = factor(ifelse(is.na(mi.multiple.imp$lnconsumer_debt),1,0), ordered = F),
         educationdebt_impflag = factor(ifelse(is.na(mi.multiple.imp$lnstudent_debt),1,0), ordered = F))

#### rerecode of log vars

conditions <- c(filters, "jobduration", "workinghours", "wage_gross_m", "inheritance_dummy") 

for (i in 1:length(lnrecode.vars)){
  chooseimp[,rerecode.vars[i]] <-  ifelse(is.na(multiple.imp[,rerecode.vars[i]]), 
                                          (sd(multiple.imp[,rerecode.vars[i]], na.rm = T)^chooseimp[,lnrecode.vars[i]]) * mean(multiple.imp[,rerecode.vars[i]], na.rm = T), 
                                          multiple.imp[,rerecode.vars[i]])
}

for (i in 1:length(rerecode.vars)){
  chooseimp[,rerecode.vars[i]] <-  ifelse(multiple.imp[,conditions[i]] == 0 & !is.na(multiple.imp[,conditions[i]]), -2, chooseimp[,rerecode.vars[i]])
}

# those ln without conditions:

chooseimp$hhnetto <- ifelse(is.na(multiple.imp$hhnetto), (sd(multiple.imp$hhnetto, na.rm = T)^chooseimp$lnhhnetto) * mean(multiple.imp$hhnetto, na.rm = T),
                            multiple.imp$hhnetto)

chooseimp$sqmtrs <- ifelse(is.na(multiple.imp$sqmtrs), (sd(multiple.imp$sqmtrs, na.rm = T)^chooseimp$lnsqmtrs) * mean(multiple.imp$sqmtrs, na.rm = T),
                           multiple.imp$sqmtrs) 

#chooseimp[,c(wealth.limits, lnrecode.vars, "lnhhnetto", "lnsqmtrs")] <- NULL
chooseimp[,c(lnrecode.vars, "lnhhnetto", "lnsqmtrs")] <- NULL

for (i in 1:length(wealth.perc)){
  chooseimp[,wealth.perc[i]] <- ifelse(chooseimp[,wealth.perc[i]] > 1, 1, chooseimp[,wealth.perc[i]])
}

chooseimp <- chooseimp %>% 
  mutate(owner_perc = as_reliable_num(owner_perc),
         other_estate_perc = as_reliable_num(other_estate_perc),
         assets_perc = as_reliable_num(assets_perc))

save(chooseimp, file = "sc19_imputed.RDA")
foreign::write.dta(chooseimp, file = "sc19imputed.dta", convert.factors = "numeric")

