##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
#mypath<- "/soep/kgoebler/data"
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))
set.seed(12)

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
         bik                             = forcats::fct_rev(factor(bik, ordered = T)),
         ggk                             = factor(ggk, ordered = T),
         wuma7                           = factor(wuma7, ordered = F),
         wuma3                           = factor(wuma3, ordered = T),
         wuma5                           = factor(wuma5, ordered = T),
         ost                             = factor(ost),
         hhgr                            = factor(hhgr, ordered = T),
         schicht                         = factor(schicht, ordered = T))


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
                "saving", "saving_value", "kidsu16", "owner_perc", "residence_value_limits", 
                "other_estate_perc", "other_estate_value_limits", 
                "assets_perc", "assets_limits", "building_contract_limits", "life_insure_limits",
                "business_holdings_limits", "vehicles_limits", "tangibles_limits", "residence_debt_limits",
                "other_estate_debt_limits", "consumer_debt_limits", "education_debt_limits", "schicht")

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

filter <- dplyr::select(data, dplyr::one_of(filterimp))
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

filters2 <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
              "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
              "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", "saving", "inherit_filter")

data[,filters2] <- filter.imp[,filters2]
gg_miss_var(data, show_pct = TRUE)
wealth.vars2 <- c("residence_value", "other_estate_value", "assets", "building_contract",
                  "life_insure", "business_holdings", "vehicles", "tangibles",
                  "residence_debt", "other_estate_debt", "consumer_debt", "education_debt", "saving_value", "total_inheritance")

wealth.perc <- c("owner_perc","other_estate_perc", "assets_perc")

wealth.limits <- c("residence_value_limits", "other_estate_value_limits", "assets_limits", "building_contract_limits",
                   "life_insure_limits", "business_holdings_limits", "vehicles_limits", "tangibles_limits", 
                   "residence_debt_limits", "other_estate_debt_limits", "consumer_debt_limits", "education_debt_limits")

wealth.comp <- data


for (i in 1:length(filters2)){
  wealth.comp[,wealth.vars2[i]] <- ifelse(wealth.comp[,filters2[i]] == 0, -2, wealth.comp[,wealth.vars2[i]])
}

for (i in 1:length(filters)){
  wealth.comp[,wealth.limits[i]] <- ifelse(wealth.comp[,filters[i]] == 0, -2, wealth.comp[,wealth.limits[i]])
}

for (i in 1:3){
  wealth.comp[,wealth.perc[i]] <- ifelse(wealth.comp[,filters[i]] == 0, -2, wealth.comp[,wealth.perc[i]])
}

gg_miss_var(wealth.comp, show_pct = TRUE)


#### recode all -2 to missings for transformation and then recode them to a new logical missing code
exceptions <- c("pid", "age", "hhgr", "orbis_wealth", "sqmtrs", "hhnetto", wealth.perc, wealth.limits)

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


from <- c(filters, wealth.limits, filters[1:3], rep("lmstatus", 6), "saving", "inherit_filter")
to <- c(lnwealthvars, lnwealthvars, wealth.perc, "lnjobduration", "lnworkinghours", "lnwage_gross", "lnwage_net", "compsize", "superior", "lnsaving", "lninheritance")
whitelist <- as.data.frame(cbind(from,to)) 


#bnplot <- ggnet2(truth.structure$arcs,
#       arrow.size = 9, arrow.gap = 0.025, label = T)
#ggsave("truthstruct.pdf", plot = bnplot)


mi.multiple.imp <- multiple.imp
mi.multiple.imp <- select(mi.multiple.imp, -c(recode.vars, "orbis_wealth", "sqmtrs", "hhnetto"))

mi.multiple.imp <- mi.multiple.imp %>% 
  mutate(residence_value_limits             = factor(residence_value_limits, ordered = T),
         other_estate_value_limits          = factor(other_estate_value_limits, ordered = T),
         assets_limits                      = factor(assets_limits, ordered = T),
         building_contract_limits           = factor(building_contract_limits, ordered = T),
         life_insure_limits                 = factor(life_insure_limits, ordered = T),
         business_holdings_limits           = factor(business_holdings_limits, ordered = T),
         vehicles_limits                    = factor(vehicles_limits,ordered = T),
         tangibles_limits                   = factor(tangibles_limits, ordered = T),
         residence_debt_limits              = factor(residence_debt_limits, ordered = T),
         other_estate_debt_limits           = factor(other_estate_debt_limits, ordered = T),
         consumer_debt_limits               = factor(consumer_debt_limits,ordered = T),
         education_debt_limits              = factor(education_debt_limits, ordered = T),
         owner_perc                         = factor(owner_perc, ordered = T),
         other_estate_perc                  = factor(other_estate_perc),
         assets_perc                        = factor(assets_perc, ordered = T))


gg_miss_var(mi.multiple.imp, show_pct = TRUE)

numCores <- detectCores() -1
plan(multiprocess, workers = numCores)

mi.structure <- bnlearn::structural.em(mi.multiple.imp[,names(mi.multiple.imp) != "pid"], maximize = "hc",
      fit = "mle", maximize.args = list(score = "bic-cg", whitelist = whitelist) , impute = "bayes-lw", max.iter = 2, return.all = T, debug = F)

(bnplot <- ggnet2(mi.structure$dag$arcs,
       arrow.size = 9, arrow.gap = 0.025, label = T))
#ggsave("truthstruct.pdf", plot = bnplot)

bn <-  bn.fit(mi.structure$dag, mi.structure$imputed, method = "mle")

bnrc <- bnrc.nomean(bn=bn, data=mi.multiple.imp, cnt.break = 150, returnfull = T)

bnimp <- bn.parents.imp(bn=bn, dag=mi.structure$dag, dat = mi.multiple.imp)

final.log <- bnrc$finalData

#### rerecode of log vars

conditions <- c(filters[1:12], "jobduration", "workinghours", "wage_gross_m", "wage_net_m", "saving", "inherit_filter") 

for (i in 1:length(lnrecode.vars)){
  final.log[,rerecode.vars[i]] <-  ifelse(is.na(multiple.imp[,rerecode.vars[i]]), (sd(multiple.imp[,rerecode.vars[i]], na.rm = T)^final.log[,lnrecode.vars[i]]) * mean(multiple.imp[,rerecode.vars[i]], na.rm = T), multiple.imp[,rerecode.vars[i]])
}

for (i in 1:length(rerecode.vars)){
final.log[,rerecode.vars[i]] <-  ifelse(multiple.imp[,conditions[i]] == 0 & !is.na(multiple.imp[,conditions[i]]), -2, final.log[,rerecode.vars[i]])
}

# those ln without conditions:

final.log$hhnetto <- ifelse(is.na(multiple.imp$hhnetto), (sd(multiple.imp$hhnetto, na.rm = T)^final.log$lnhhnetto) * mean(multiple.imp$hhnetto, na.rm = T),
                            multiple.imp$hhnetto)

final.log$sqmtrs <- ifelse(is.na(multiple.imp$sqmtrs), (sd(multiple.imp$sqmtrs, na.rm = T)^final.log$lnsqmtrs) * mean(multiple.imp$sqmtrs, na.rm = T),
                           multiple.imp$sqmtrs) 

final.log[,c(wealth.limits, lnrecode.vars, "lnhhnetto", "lnsqmtrs")] <- NULL

final.log <- final.log %>% 
  mutate(owner_perc = as_reliable_num(owner_perc),
         other_estate_perc = as_reliable_num(other_estate_perc),
         assets_perc = as_reliable_num(assets_perc))

print("Imputation Done, No errors detected")


save(final.log, file = "topw_imp.RDA")
foreign::write.dta(final.log, file = "topw_imp.dta")




