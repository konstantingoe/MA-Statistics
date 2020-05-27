##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
#mypath<- "/soep/kgoebler/data"
mydata <- import(paste(mypath, "SC19_ImputationReadySample.dta", sep = "/"))
set.seed(12)

#potentially <- set_na(mydata$residence_debt_filter, na =c("Does not apply" = -2), as.tag = T)
# first impute filter information and then based on these impute wealth components:

##### Filter Imputation


factorvars <- c("sex", "bula_sta", "job_training", "selfempl", "superior", "second_empl", "citizen", "famstd",
                "owner", "residence_debt_filter", "other_estate", "other_estate_debt_filter",
                "assets_filter", "building_contract_filter", "life_insure_filter", "business_holdings_filter",
                "vehicles_filter", "tangibles_filter", "consumer_debt_filter", "education_debt_filter",
                "gborn", "kidsu16", "partner", "inheritance_dummy", "emp_status", "labormarket_dummy",
                "p10000_2017", "p02000_2017", "p00200_2017", "p00020_2017", "e10000_2017", "e02000_2017", "e00020_2017",
                "f10000_2017", "f02000_2017", "f00020_2017",
                "e00200_2017", "e00200_2017", "b10000_2017", "b02000_2017",
                "t10000_2017", "t02000_2017", "c10000_2017", "c02000_2017", "l10000_2017", "l02000_2017",
                "h02000_2017", "h10000_2017", "v10000_2017", "v02000_2017", "s10000_2017", "s02000_2017",
                "residence_debt_filter_2017", "other_estate_debt_filter_2017")

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

#### recode imputed vars from 2017 to NA

impflags <- c("p02000_2017", "p00200_2017", "p00020_2017", "e02000_2017", "e00020_2017", 
              "e00200_2017", "f02000_2017", "f00020_2017", "b02000_2017", "t02000_2017", "c02000_2017", 
              "l02000_2017", "h02000_2017", "v02000_2017", "s02000_2017")

## now the corresponding 2017 vars:

oldimpvars <- c("p0100a_2017", "p0010a_2017", "p00010_2017", "e0100a_2017", "e00010_2017",
                "e0010a_2017", "f0100a_2017", "f00010_2017", "b0100a_2017", "t0100a_2017", "c0100a_2017",
                "l0100a_2017", "h0100a_2017", "v0100a_2017", "s0100a_2017") 

for (i in 1:length(impflags)){
  mydata[,oldimpvars[i]] <- ifelse(mydata[,impflags[i]] == 2, NA, mydata[,oldimpvars[i]])
}

## also set all obervations from 2017 that are not in 2019 to missing:

mydata$firstsample <- ifelse(mydata$p10000_2017 == -2, 1,0)

varsfrom2017 <- names(select(mydata, contains("2017")))

mydata[mydata$firstsample == 1,varsfrom2017] <- NA

mydata <- select(mydata, -c(impflags, "firstsample"))

varsfrom2017 <- names(select(mydata, contains("2017")))

mydata <- droplevels(mydata)

gg_miss_var(mydata, show_pct = TRUE)

#ggsave("missing.pdf")
# tag new clean file

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

filter.2017 <- c("p10000_2017", "e10000_2017", "f10000_2017", "l10000_2017", "h10000_2017", "b10000_2017",
                 "v10000_2017", "t10000_2017", "residence_debt_filter_2017", "other_estate_debt_filter_2017",
                 "c10000_2017", "s10000_2017")

vars.2017 <- c("p0100a_2017", "e0100a_2017", "f0100a_2017", "l0100a_2017", "h0100a_2017", "b0100a_2017",
               "v0100a_2017", "t0100a_2017", "p0010a_2017", "e0010a_2017", "c0100a_2017", "s0100a_2017") 


filter <- dplyr::select(data, dplyr::one_of(setdiff(names(mydata), c(wealth.vars2, vars.2017))))

gg_miss_var(filter, show_pct = TRUE)


filterimp <- setdiff(names(filter), "pid")
# first impute filter variables 
filter.imp <- VIM::kNN(filter, variable = filterimp, dist_var = colnames(filter[, -which(names(filter) == "pid")]),
                       donorcond =  as.list(rep("!= -2",length(filterimp))), imp_var =F)


data[,c(filters2, filter.2017)] <- filter.imp[,c(filters2, filter.2017)]
gg_miss_var(data[,c(filters2, filter.2017)], show_pct = TRUE)


wealth.perc <- c("owner_perc","other_estate_perc", "assets_perc")

wealth.perc.2017 <- c("p00010_2017", "e00010_2017", "f00010_2017")

wealth.limits <- c("residence_value_limits", "other_estate_value_limits", "assets_limits", "building_contract_limits",
                   "life_insure_limits", "business_holdings_limits", "vehicles_limits", "tangibles_limits", 
                   "residence_debt_limits", "other_estate_debt_limits", "consumer_debt_limits", "education_debt_limits")

wealth.comp <- data

### recode values that correspond to the filter logic just imputed: 

for (i in 1:length(filters2)){
  wealth.comp[,wealth.vars2[i]] <- ifelse(wealth.comp[,filters2[i]] == 0, -2, wealth.comp[,wealth.vars2[i]])
}


for (i in 1:length(filter.2017)){
  wealth.comp[,vars.2017[i]] <- ifelse(wealth.comp[,filter.2017[i]] == 0, -2, wealth.comp[,vars.2017[i]])
}

for (i in 1:length(filters)){
  wealth.comp[,wealth.limits[i]] <- ifelse(wealth.comp[,filters[i]] == 0, -2, wealth.comp[,wealth.limits[i]])
  wealth.comp[,wealth.limits[i]] <- factor(wealth.comp[,wealth.limits[i]], ordered = T)
}

for (i in 1:3){
  wealth.comp[,wealth.perc[i]] <- ifelse(wealth.comp[,filters[i]] == 0, -2, wealth.comp[,wealth.perc[i]])
}

for (i in 1:3){
  wealth.comp[,wealth.perc.2017[i]] <- ifelse(wealth.comp[,filter.2017[i]] == 0, -2, wealth.comp[,wealth.perc.2017[i]])
}


gg_miss_var(wealth.comp, show_pct = TRUE)


#### recode all -2 to missings for transformation and then recode them to a new logical missing code
exceptions <- c("pid", "age", "hhgr", "sqmtrs", "hhnetto", wealth.perc, wealth.perc.2017, wealth.limits)

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
         lnhhnetto         = normalize.log(hhnetto),
         lnresidence.2017       = normalize.log(p0100a_2017),
         lnestate.2017          = normalize.log(e0100a_2017),
         lnassets.2017          = normalize.log(f0100a_2017),
         lnbuilding.2017        = normalize.log(l0100a_2017),
         lnlife.2017            = normalize.log(h0100a_2017),
         lnbusiness.2017        = normalize.log(b0100a_2017),
         lnvehicles.2017        = normalize.log(v0100a_2017),
         lntangibles.2017       = normalize.log(t0100a_2017),
         lnresidence_debt.2017  = normalize.log(p0010a_2017),
         lnestate_debt.2017     = normalize.log(e0010a_2017),
         lnconsumer_debt.2017   = normalize.log(c0100a_2017),
         lnstudent_debt.2017    = normalize.log(s0100a_2017))

lnwealthvars <- c("lnresidence", "lnestate", "lnassets", "lnbuilding", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
                  "lnresidence_debt", "lnestate_debt", "lnconsumer_debt", "lnstudent_debt")
lnwealthvars.2017 <- paste0(lnwealthvars,".2017")

lnrecode.vars <- c(lnwealthvars, "lnjobduration",
                   "lnworkinghours", "lnwage_gross", "lninheritance", lnwealthvars.2017)

rerecode.vars <- c(wealth.vars, "jobduration", "workinghours", "wage_gross_m", "total_inheritance", lnwealthvars.2017)

from <- c(filters, wealth.limits, filters[1:3], rep("emp_status", 5), "inheritance_dummy",
          filter.2017, lnwealthvars.2017, filter.2017[1:3], wealth.perc.2017)

to <- c(lnwealthvars, lnwealthvars, wealth.perc, "lnjobduration", "lnworkinghours", "lnwage_gross", "compsize", "superior", "lninheritance",
        lnwealthvars.2017, lnwealthvars, wealth.perc.2017, wealth.perc)
whitelist <- as.data.frame(cbind(from,to)) 


#bnplot <- ggnet2(truth.structure$arcs,
#       arrow.size = 9, arrow.gap = 0.025, label = T)
#ggsave("truthstruct.pdf", plot = bnplot)


mi.multiple.imp <- multiple.imp
mi.multiple.imp <- select(mi.multiple.imp, -c(recode.vars, "sqmtrs", "hhnetto"))

gg_miss_var(mi.multiple.imp, show_pct = TRUE)

numCores <- detectCores() -1
plan(multisession, workers = numCores)

mi.structure <- bnlearn::structural.em(mi.multiple.imp[,names(mi.multiple.imp) != "pid"], maximize = "hc", maximize.args = list(score = "bic-cg", restart = 0, whitelist = whitelist), 
                                       fit = "mle", impute = "bayes-lw", max.iter = 10, return.all = T, debug = F)

vertexlabels <- paste("X",1:(ncol(mi.multiple.imp) -1), sep = "")
bnplot <- ggnet2(mi.structure$dag$arcs,
                 arrow.size = 7, arrow.gap = 0.027, label = vertexlabels, node.size = 12.9)
#ggsave("truthstruct.pdf", plot = bnplot)

bn <-  bn.fit(mi.structure$dag, mi.structure$imputed, method = "mle")

bnrc <- bnrc.nomean(bn=bn, data=mi.multiple.imp, cnt.break = 2, returnfull = T)

bnimp <- bn.parents.imp(bn=bn, dag=mi.structure$dag, dat = mi.multiple.imp)

gg_miss_var(bnrc$finalData, show_pct = TRUE)

#final.log <- bnrc$finalData
# choose imputation method:

chooseimp <- bnrc$finalData 

#### rerecode of log vars

conditions <- c(filters, "jobduration", "workinghours", "wage_gross_m", "inheritance_dummy", filter.2017) 

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

chooseimp[,c(wealth.limits, lnrecode.vars, "lnhhnetto", "lnsqmtrs")] <- NULL

for (i in 1:length(wealth.perc)){
  chooseimp[,wealth.perc[i]] <- ifelse(chooseimp[,wealth.perc[i]] > 1, 1, chooseimp[,wealth.perc[i]])
}

chooseimp <- chooseimp %>% 
  mutate(owner_perc = as_reliable_num(owner_perc),
         other_estate_perc = as_reliable_num(other_estate_perc),
         assets_perc = as_reliable_num(assets_perc))

save(chooseimp, file = "topw_imp.RDA")
foreign::write.dta(chooseimp, file = "topw_imp.dta")




