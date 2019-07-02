##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))



test.imp <- VIM::kNN(test, makeNA = nacond)

gg_miss_var(test.imp, show_pct = TRUE)


#  filter(owner == 1) 
#  filter(other_estate == 1) 
#  filter(assets_filter == 1) 
#  filter(building_contract_filter == 1)
#  filter(life_insure_filter == 1) 
#  filter(business_holdings_filter == 1) 
#  filter(vehicles_filter == 1) 
#  filter(tangibles_filter == 1) 

#### liabilities:

### set of filters:   
#  filter(residence_debt_filter == 1) 
#  filter(other_estate_debt_filter == 1) 
#  filter(consumer_debt_filter == 1) 
#  filter(education_debt_filter == 1) 

mydata <- mydata %>% 
  mutate(lnorbis         = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>%
  mutate(lnwage_g        = log(1+wage_gross_m) - log(mean(wage_gross_m, na.rm = T))) %>% 
  mutate(lnjobduration   = log(jobduration) - log(mean(jobduration, na.rm = T))) %>% 
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
  mutate(lnsqrmtrs       = log(sqmtrs) - log(mean(sqmtrs, na.rm = T))) %>%
  mutate(lnsaving        = log(1+saving_value) - log(mean(1+saving_value, na.rm = T))) %>%
  mutate(lnhhnetto       = log(1+hhnetto) - log(mean(1+hhnetto, na.rm = T))) %>%
  mutate(lninheritance   = log(1+total_inheritance) - log(mean(1+total_inheritance, na.rm = T))) %>%
  mutate(educationjob   = ifelse(educationjob <0 , NA, educationjob)) 


# Bayesian Network for other estate:
  #make sure I have the same variables for BN as for MICE plus a couple more
# MICE imputation covariates from SOEP:
  
  #sex, west, lnwage, years of education, debt indicator, income from rent, selfemployment dummy, car dummy, 
  #public sector, sparbuch1, nopartner dummy, age, age2, private insurance, inheritance, rural area (BIK), other regional stuff (Raumordnungsregion)
  
  
### ask Markus for regional indicators!  

preparation <- mydata %>% 
  mutate(lnresidence = ifelse(is.na(lnresidence) & owner == 0,0,lnresidence)) %>% 
  mutate(lnassets    = ifelse(is.na(lnassets) & assets_filter == 0,0,lnassets)) %>% 
  mutate(lnbuilding  = ifelse(is.na(lnbuilding) & building_contract_filter == 0,0,lnbuilding)) %>% 
  mutate(lnbusiness  = ifelse(is.na(lnbusiness) & business_holdings_filter == 0,0,lnbusiness)) %>% 
  mutate(lnvehicles  = ifelse(is.na(lnvehicles) & vehicles_filter == 0,0,lnvehicles)) %>% 
  mutate(lnestatedebt  = ifelse(is.na(lnestatedebt) & other_estate_debt_filter == 0,0,lnestatedebt))
  

  
estatevars <- c("sex", "lnhhnetto", "lnwage_g", "educationjob", "other_estate_debt_filter",
                "housedebt", "selfempl", "employed", "famstd", "age",
                "lnorbis", "lmstatus", "hours_actual", "inherit_filter", "other_estate_value_limits", "kidsu16",
                "livingcond", "estate_income_value", "saving_value", "owner", "lnestate", "other_estate_value", "lninheritance",
                "lnresidence", "lnassets", "lnbuilding", "lnbusiness", "lnvehicles", "lnestatedebt")
  
  
estate <- select(filter(preparation, other_estate == 1), one_of(estatevars)) %>% 
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

estate.clean <- VIM::kNN(select(estate, -other_estate_value_limits), addRF = T, imp_var=F)
estate_limits <- estate$other_estate_value_limits
estate.clean <- cbind(estate.clean, estate_limits)

estate.clean <- estate.clean %>% 
  mutate(estate = exp(lnestate)* mean(estate$other_estate_value, na.rm = T)) %>% 
  mutate(estate_limits = ifelse(is.na(estate_limits) & estate < 20000,1,
                                            ifelse(is.na(estate_limits) & estate >= 20000 & estate < 150000,2,
                                                   ifelse(is.na(estate_limits) & estate >= 150000 & estate < 500000,3,
                                                          ifelse(is.na(estate_limits) & estate >= 500000 & estate < 1000000,4,
                                                                 ifelse(is.na(estate_limits) & estate >= 1000000,5,estate_limits))))))


estate.clean <- select(estate.clean, -other_estate_value, -estate)
anyNA(estate.clean)
gg_miss_var(estate.clean, show_pct = TRUE)

# now create netwealth variable 

estate.final <- 

#### mixed data types: DEAL package
### 
small <- select(estate.clean, -c("educationjob", "famstd", "lmstatus", "estate_limits", "livingcond"))

net <- deal::network(small)
net
ggnet2(net)

prior <- jointprior(net)

net <- learn(net, small, prior)$nw

best <- autosearch(net, small, prior, trace = T)


mstring <- deal::modelstring(best$nw)
mstring
dag.deal <- model2network(mstring)

ggnet2(dag.deal$arcs, directed = TRUE,
       arrow.size = 9, arrow.gap = 0.025, label = T)
ggsave("bayesian_net.pdf")

mb(dag.deal, "lnestate")

#### for presentation ####

dag1 <- empty.graph(nodes = c("A", "B", "C", "D", "E", "F"))
arc.set <- matrix(c("A", "C",
                    "B", "C",
                    "C", "D",
                    "C", "E",
                    "D", "F",
                    "E", "F"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag1) <- arc.set

modelstring(dag1)
nodes(dag1)
arcs(dag1)

# Graphical Implementation
pdf("exampleBN.pdf") 
graphviz.plot(dag1)
dev.off()



hlight <- list(nodes = nodes(dag1), arcs = arcs(dag1),
               col = "grey", textCol = "grey")
pp <- graphviz.plot(dag1, highlight = hlight)

graph::nodeRenderInfo(pp) <- list(col = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45)),
                                  textCol = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45)))
graph::edgeRenderInfo(pp) <- list(col = c("A~C" = "black", "B~C" = "black"),
                                  lwd = c("A~C" = 3, "B~C" = 3))
pdf("parentsBN.pdf") 
Rgraphviz::renderGraph(pp)
dev.off()

mb(dag1, node = "C")
pp <- graphviz.plot(dag1, highlight = hlight)

graph::nodeRenderInfo(pp) <- list(col = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black"),
                                  textCol = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black"))
graph::edgeRenderInfo(pp) <- list(col = c("A~C" = "black", "B~C" = "black", "C~D" = "black", "C~E" = "black"),
                                  lwd = c("A~C" = 3, "B~C" = 3, "C~D" = 3, "C~E" = 3))
pdf("markovb.pdf") 
Rgraphviz::renderGraph(pp)
dev.off()


### better to see the Markov blanket property:

dag2 <- empty.graph(nodes = c("A", "B", "C", "D", "E", "F", "G"))
arc.set <- matrix(c("A", "C",
                    "B", "C",
                    "C", "D",
                    "C", "E",
                    "D", "F",
                    "E", "F",
                    "G", "D"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set

mb(dag2, node = "C")

pp <- graphviz.plot(dag2, highlight = hlight)

graph::nodeRenderInfo(pp) <- list(col = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black", "G" = "black"),
                                  textCol = c("A" = "black", "B" = "black", "C" = rgb(0, 0, 0.45), "D" = "black", "E" = "black", "G" = "black"))
graph::edgeRenderInfo(pp) <- list(col = c("A~C" = "black", "B~C" = "black", "C~D" = "black", "C~E" = "black", "G~D" = "black"),
                                  lwd = c("A~C" = 3, "B~C" = 3, "C~D" = 3, "C~E" = 3, "G~D" = 3))
pdf("markovb_2.pdf") 
Rgraphviz::renderGraph(pp)
dev.off()


