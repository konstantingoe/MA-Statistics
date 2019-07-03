##### Bayesian Network #####

##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))

# first impute filter information and then based on these impute wealth components:

##### Filter Imputation


mydata <- mydata %>% 
  mutate(owner = factor(owner)) %>% 
  mutate(other_estate = factor(other_estate)) %>% 
  mutate(assets_filter = factor(assets_filter)) %>% 
  mutate(building_contract_filter = factor(building_contract_filter)) %>% 
  mutate(life_insure_filter = factor(life_insure_filter)) %>% 
  mutate(business_holdings_filter = factor(business_holdings_filter)) %>% 
  mutate(vehicles_filter = factor(vehicles_filter)) %>% 
  mutate(tangibles_filter = factor(tangibles_filter)) %>% 
  mutate(residence_debt_filter = factor(residence_debt_filter)) %>% 
  mutate(other_estate_debt_filter = factor(other_estate_debt_filter)) %>% 
  mutate(consumer_debt_filter = factor(consumer_debt_filter)) %>% 
  mutate(education_debt_filter = factor(education_debt_filter)) %>% 
  mutate(lmstatus = factor(lmstatus)) %>% 
  mutate(sex = factor(sex)) %>% 
  mutate(famstd = factor(famstd)) %>% 
  mutate(inherit_filter = factor(inherit_filter)) %>% 
  mutate(selfempl = factor(selfempl)) %>% 
  mutate(kidsu16 = factor(kidsu16)) %>% 
  mutate(other_estate_debt_filter = factor(other_estate_debt_filter)) %>% 
  mutate(employed = factor(employed)) %>% 
  mutate(livingcond = factor(livingcond, ordered = T)) %>% 
  mutate(other_estate_value_limits = factor(other_estate_value_limits, ordered = T)) %>% 
  #mutate(educationjob    = ifelse(educationjob <0 , NA, educationjob)) %>% 
  mutate(educationjob    = factor(educationjob, ordered = T)) %>% 
  mutate(housedebt       = factor(housedebt)) %>% 
  mutate(lnsqrmtrs       = log(sqmtrs) - log(mean(sqmtrs, na.rm = T))) %>%
  mutate(lnsaving        = log(1+saving_value) - log(mean(1+saving_value, na.rm = T))) %>%
  mutate(lnhhnetto       = log(1+hhnetto) - log(mean(1+hhnetto, na.rm = T))) %>%
  mutate(lninheritance   = log(1+total_inheritance) - log(mean(1+total_inheritance, na.rm = T))) %>%
  mutate(lnorbis         = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>%
  mutate(lnwage_g        = log(1+wage_gross_m) - log(mean(wage_gross_m, na.rm = T))) %>% 
  mutate(lnjobduration   = log(jobduration) - log(mean(jobduration, na.rm = T)))
  
filterimp <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
               "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
               "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter", "lmstatus",
               "sex", "famstd", "inherit_filter", "selfempl","kidsu16", "employed",
               "livingcond", "educationjob", "housedebt", "lnsqrmtrs", "lnsaving", "lnhhnetto", "lninheritance",
               "lnorbis", "lnwage_g", "lnjobduration")    
  
filter <- select(mydata, one_of(filterimp))
gg_miss_var(filter, show_pct = TRUE)

filter.imp <- VIM::kNN(filter, imp_var =F)

gg_miss_var(filter.imp, show_pct = TRUE)

filters <- c("owner","other_estate", "assets_filter", "building_contract_filter", "life_insure_filter",
             "business_holdings_filter", "vehicles_filter", "tangibles_filter", "residence_debt_filter",
             "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter")
filter.post <- select(filter.imp,one_of(filters))

#### parse filters back to a subset of mydata with all of the wealth components:

wealth.vars <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles",
                 "residence_debt", "other_estate_debt", "consumer_debt", "education_debt")
other.vars <- c("pid", "lmstatus","sex", "famstd", "inherit_filter", "selfempl","kidsu16", "employed",
                "livingcond", "educationjob", "housedebt", "lnsqrmtrs", "lnsaving", "lnhhnetto", "lninheritance",
                "lnorbis", "lnwage_g", "lnjobduration")    

wealth.comp <- select(mydata, one_of(wealth.vars,other.vars))
wealth.comp <- cbind(filter.post, wealth.comp)
gg_miss_var(wealth.comp, show_pct = TRUE)


filter.imp <- setNames(lapply(seq_along(wealth.vars), function(i) 
  VIM::kNN(select(filter(wealth.comp, .data[[filters[[i]]]] == 1), one_of(wealth.vars[i], other.vars)), imp_var =F)), names(wealth.vars))


gg_miss_var(filter.imp[[1]], show_pct = TRUE)

pid <- as.data.frame(mydata$pid)
names(pid) <- "pid"

wealth.final <- left_join(pid, select(filter.imp[[1]], one_of(c("pid", wealth.vars[1]))), by = "pid")
for(i in 2:12){
  wealth.final <- left_join(wealth.final, select(filter.imp[[i]], one_of(c("pid", wealth.vars[i]))), by = "pid")
}
gg_miss_var(wealth.final, show_pct = TRUE)

asset.vars <- c("residence_value", "other_estate_value", "assets", "building_contract",
                 "life_insure", "business_holdings", "vehicles", "tangibles")
liabilities.vars <- c("residence_debt", "other_estate_debt", "consumer_debt", "education_debt")
                 
wealth.final <- wealth.final %>% 
  mutate(net_wealth = rowSums(select(wealth.final, one_of(asset.vars)), na.rm = T)) %>% 
  mutate(liabilities = rowSums(select(wealth.final, one_of(liabilities.vars)), na.rm = T)) %>% 
  mutate(netwealth = net_wealth -liabilities) %>% 
  mutate(lnnetwealth = log(1+net_wealth) - log(mean(1+net_wealth)))

gg_miss_var(wealth.final, show_pct = TRUE)
hist(wealth.final$lnnetwealth)
summary(wealth.final$netwealth)

targetvars <- c("pid","sex", "lnhhnetto", "lnwage_g", "educationjob", "total_inheritance",
                "housedebt", "selfempl", "employed", "famstd", "age",
                "lnorbis", "lmstatus", "hours_actual", "inherit_filter", "kidsu16",
                "livingcond", "estate_income_value", "saving_value", "superior", "stillfirstemp",
                "sqmtrs")

final <- select(mydata, one_of(targetvars)) 
final <- left_join(final, select(wealth.final, one_of(c("pid", "netwealth", "lnnetwealth"))), by = "pid") %>% 
  select(-pid)
gg_miss_var(final, show_pct = TRUE)
final.imp <- VIM::kNN(final, imp_var = F)
gg_miss_var(final.imp, show_pct = TRUE)


# Bayesian Network for other estate:
  #make sure I have the same variables for BN as for MICE plus a couple more
# MICE imputation covariates from SOEP:
  
  #sex, west, lnwage, years of education, debt indicator, income from rent, selfemployment dummy, car dummy, 
  #public sector, sparbuch1, nopartner dummy, age, age2, private insurance, inheritance, rural area (BIK), other regional stuff (Raumordnungsregion)
  
  
### ask Markus for regional indicators!  
#estate.clean <- estate.clean %>% 
#  mutate(estate = exp(lnestate)* mean(estate$other_estate_value, na.rm = T)) %>% 
#  mutate(estate_limits = ifelse(is.na(estate_limits) & estate < 20000,1,
#                                            ifelse(is.na(estate_limits) & estate >= 20000 & estate < 150000,2,
#                                                   ifelse(is.na(estate_limits) & estate >= 150000 & estate < 500000,3,
#                                                          ifelse(is.na(estate_limits) & estate >= 500000 & estate < 1000000,4,
#                                                                 ifelse(is.na(estate_limits) & estate >= 1000000,5,estate_limits))))))


#### mixed data types: DEAL package
### make dummies and denote as factor!

final.imp <- select(final.imp, -c("educationjob", "famstd", "lmstatus", "livingcond"))

net <- deal::network(final.imp)
plot(net)

prior <- jointprior(net)

net <- learn(net, final.imp, prior)$nw

best <- autosearch(net, final.imp, prior, trace = T)


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


