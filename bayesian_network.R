##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))


mydata <- mydata %>% 
  mutate(lnorbis         = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>%
  mutate(lnwage_g        = log(1+wage_gross_m) - log(mean(wage_gross_m, na.rm = T))) %>% 
  mutate(lnjobduration   = log(jobduration) - mean(log(jobduration), na.rm = T)) %>% 
  mutate(lnsqmtrs        = log(sqmtrs) - log(mean(sqmtrs, na.rm = T))) %>% 
  mutate(lnresidence     = log(residence_value)- log(mean(residence_value, na.rm = T))) %>% 
  mutate(lnestate        = log(other_estate_value)- log(mean(other_estate_value, na.rm = T))) %>% 
  mutate(lnassets        = log(assets) - log(mean(assets, na.rm = T))) %>% 
  mutate(lnbuilding      = log(building_contract) - log(mean(building_contract, na.rm = T))) %>% 
  mutate(lnlife          = log(life_insure) - log(mean(life_insure, na.rm = T))) %>% 
  mutate(business_holdings = ifelse(business_holdings==0, 1,business_holdings)) %>% 
  mutate(lnbusiness      = log(business_holdings) - log(mean(business_holdings, na.rm = T))) %>% 
  mutate(lnvehicles      = log(vehicles) - log(mean(vehicles, na.rm = T))) %>% 
  mutate(lntangibles     = log(tangibles) - log(mean(tangibles, na.rm = T))) %>% 
  mutate(lnresidencedebt = log(residence_debt)- log(mean(residence_debt, na.rm = T))) %>% 
  mutate(lnestatedebt    = log(other_estate_debt)- log(mean(other_estate_debt, na.rm = T))) %>% 
  mutate(lnconsumerdebt  = log(consumer_debt)- log(mean(consumer_debt, na.rm = T))) %>% 
  mutate(lneducationdebt = log(education_debt)- log(mean(education_debt, na.rm = T))) %>% 
  mutate(lnsqrmtrs       = log(sqmtrs) - mean(log(sqmtrs), na.rm = T)) %>%
  mutate(lnsaving        = log(1+saving_value) - mean(log(1+saving_value)), na.rm = T) %>%
  mutate(lnhhnetto       = log(1+hhnetto) - mean(log(1+hhnetto), na.rm = T)) %>%
  mutate(lninheritance   = log(1+total_inheritance) - mean(log(1+total_inheritance), na.rm = T)) %>%
  mutate(educationjob   = ifelse(educationjob <0 , NA, educationjob))

# Bayesian Network for other estate:
  #make sure I have the same variables for BN as for MICE plus a couple more
# MICE imputation covariates from SOEP:
  
  #sex, west, lnwage, years of education, debt indicator, income from rent, selfemployment dummy, car dummy, 
  #public sector, sparbuch1, nopartner dummy, age, age2, private insurance, inheritance, rural area (BIK), other regional stuff (Raumordnungsregion)
  
  
### ask Markus for regional indicators!  
  
  
estatevars <- c("sex", "lnhhnetto", "lnwage_g", "educationjob", "other_estate_debt_filter",
                "housedebt", "selfempl", "employed", "famstd", "age", "total_inheritance",
                "lnorbis", "lmstatus", "hours_actual", "inherit_filter", "other_estate_value_limits", "kidsu16",
                "livingcond", "estate_income_value", "saving_value", "owner", "lnestate", "lninheritance")
  
  
estate <- select(filter(mydata, other_estate == 1), one_of(estatevars)) %>% 
  mutate(lmstatus = factor(lmstatus)) %>% 
  mutate(owner = factor(owner)) %>% 
  mutate(sex = factor(sex)) %>% 
  mutate(famstd = factor(famstd)) %>% 
  mutate(inherit_filter = factor(inherit_filter)) %>% 
  mutate(selfempl = factor(selfempl)) %>% 
  mutate(kidsu16 = factor(kidsu16)) %>% 
  mutate(other_estate_debt_filter = factor(other_estate_debt_filter)) %>% 
  mutate(employed = factor(employed)) %>% 
  mutate(livingcond = factor(livingcond, ordered = T)) %>% 
  mutate(other_estate_value_limits = factor(other_estate_value_limits, ordered = T)) %>% 
  mutate(educationjob = factor(educationjob, ordered = T)) %>% 
  mutate(housedebt = factor(housedebt)) 
  
  
  
gg_miss_var(estate, show_pct = TRUE)


#### Quick imputation for simulation: kNN ####

estate.clean <- VIM::kNN(estate, addRF = T)

anyNA(estate.clean)
gg_miss_var(estate.clean, show_pct = TRUE)

estate.bn <- VIM::kNN(estate, addRF = T, imp_var=F)

estate.bn <- discretize(estate.bn, ordered = T, method = "interval")
learned <- hc(estate.bn)
modelstring(learned)
score(learned, data = estate.bn, type = "bic")

learned2 <- hc(estate.bn, score = "bde")
arc.strength(learned, data = estate.bn, criterion = "bic")
# from the learned score, removing any will result in a decrease of BIC

plot(learned, method="qgraph",
     label.scale.equal=T,
     node.width = 1.6,
     plot.wpdag=T)

filehead <- c(T,F,F,T,T,T,T,T,T,F,F,F,T,F,T,T,T,T,F,F,T,F,F)
varnames <- names(estate.bn)
cardinality <- c(2,50,50,4,2,2,2,2,4,5,50,50,5,50,2,5,2,5,50,50,2,50,50)


bndata <- BNDataset(data = data.matrix(estate.bn, rownames.force = NA), discreteness = filehead, variables = varnames, node.sizes = cardinality)

net <- learn.network(bndata, use.imputed.data = F,
                     initial.network = "random.chain",
                     seed = 12345)

plot(net, method="qgraph",
     label.scale.equal=F,
     node.width = 1.6,
     plot.wpdag=F)



##### learn structure

filehead <- c(T,F,T, rep(F, 3), T, F, T)
varnames <- names(residence)
cardinality <- c(3,50,2,10,50,100,2,50,5)

### maybe use cardinality from discretize... or discretized data.frame for bndata....


bndata <- BNDataset(data = as.matrix(residence), discreteness = filehead, variables = varnames, node.sizes = cardinality)
print(bndata)
layers <- c(2,1,1,3,2,2,2,3,2)


imp.dataset <- bnstruct::impute(bndata, k.impute = 50)
has.imputed.data(imp.dataset)
net <- learn.network(imp.dataset, use.imputed.data = TRUE,
                     layering = layers,
                     initial.network = "random.chain",
                     seed = 12345)

plot(net, method="qgraph",
     label.scale.equal=F,
     node.width = 1.6,
     plot.wpdag=F)


#### gives me an error at some point...
bn <- learn.network(bndata,
                    algo = "sem",
                    scoring.func = "AIC",
                    layering = layers)




