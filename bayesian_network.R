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
cardinality <- c(3,50,2,10,50,100,2,50,5)

### maybe use cardinality from discretize... or discretized data.frame for bndata....


bndata <- BNDataset(data = as.matrix(residence), discreteness = filehead, variables = varnames, node.sizes = cardinality)
print(bndata)
layers <- c(2,1,1,3,2,2,2,3,2)


imp.dataset <- bnstruct::impute(bndata, k.impute = 7)
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




