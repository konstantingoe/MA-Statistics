##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))

residence <- select(mydata, one_of(c("schicht", "age", "sex", "orbis_wealth", "jobduration", "wage_gross_m", "inherit_filter", "owner", "residence_value", "residence_value_limits"))) %>% 
  filter(owner == 1) %>% 
  mutate(inherit_filter = inherit_filter + 1) %>% 
  select(-owner)
  #mutate(lnorbis = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>% 
  #mutate(lnresidence = log(residence_value)- log(mean(residence_value, na.rm = T))) %>% 
  #mutate(lnwage_g = log(1+wage_gross_m) - log(mean(residence_value, na.rm = T)))

gg_miss_var(residence, show_pct = TRUE)

##### learn structure

filehead <- c(T,F,T, rep(F, 3), T, F, T)
varnames <- names(residence)
cardinality <- c(3,100,2,10,100,100,2,100,5)

### maybe use cardinality from discretize... or discretized data.frame for bndata....


bndata <- BNDataset(data = residence, discreteness = filehead, variables = varnames, node.sizes = cardinality)
print(bndata)

imp.dataset <- impute(bndata)
net <- learn.network(imp.dataset, use.imputed.data = TRUE,
                     layering = layers)

layers <- c(2,1,1,3,2,2,2,3,2)
net.1 <- learn.network(bndata,
                       initial.network = "random.chain",
                       seed = 12345,
                       layering = layers)
plot(net.1)

bn <- learn.network(bndata,
                    algo = "sem",
                    scoring.func = "AIC",
                    initial.network = net,
                    layering = layers)





