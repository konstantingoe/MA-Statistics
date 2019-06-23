##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/")) %>% 
  mutate(female = sex -1) %>% 
  mutate(married = ifelse(famstd==2,1,ifelse(is.na(famstd), NA, 0)))


#### summary table for presentation #####

presentation <- select(mydata, "age", "female", "citizen" , "married", "bwest", "kidsu16", "saving_value",
                       "hhnetto", "wage_gross_m", "employed", "jobduration", "selfempl", "inherit_filter", "total_inheritance")
names(presentation) <- c("Age", "Female", "German citizen", "Married", "Born west", "Kids < 16", "Monthly savings", "Monthly HHnetto income", "Monthly gross wage", "Employed", "Duration in current Job", "Self-Employed","Ever inherited", "Total inheritance")
stargazer(presentation, out = "summarystat.tex" ,title = "Summary Table: High Wealth Dataset", summary = T, notes = "SOEPv36 - TopW Data; Author's calculations", summary.stat = c("n", "mean", "median", "sd", "min", "max"))


presentation2 <- select(mydata, "orbis_wealth", "residence_value", "other_estate_value", "assets", "building_contract", "life_insure", "business_holdings", "vehicles", "tangibles", "residence_debt", "other_estate_debt", "consumer_debt", "education_debt") %>% 
  mutate(orbis_wealth = orbis_wealth * 1000)
names(presentation2) <- c("Total shareholdings (Orbis)", "MV primary residence", "MV other estates", "Financial Assets", "Building loan contract", "Life insurance", "Company shares", "Vehicle value", "Tangibles value", "Debt primary residene", "Debt other estates", "Consumer debt", "Student loans")
stargazer(presentation2, out = "summarystat2.tex" ,title = "Summary Table: High Wealth Dataset", summary = T, notes = "SOEPv36 - TopW Data; Author's calculations", summary.stat = c("n", "mean", "median", "sd", "min", "max"), digits = 0)



##### Testing MCAR for wealth assets Values ####
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

mydata <- mydata %>% 
  mutate(lnorbis         = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>%
  mutate(orbisscale      = range01(log(orbis_wealth), na.rm = T)) %>%
  mutate(lnwage_g        = log(1+wage_gross_m) - log(mean(wage_gross_m, na.rm = T))) %>% 
  mutate(jobduration_i   = ifelse(is.na(jobduration),median(jobduration), jobduration)) %>% 
  mutate(lnjobduration   = log(jobduration_i) - mean(log(jobduration_i))) %>% 
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
  mutate(sqmtrs = ifelse(is.na(sqmtrs),median(sqmtrs, na.rm=T),sqmtrs)) %>% 
  mutate(lnsqrmtrs = log(sqmtrs) - mean(log(sqmtrs))) %>%
  mutate(sqmtrscaled = range01(log(sqmtrs)))

# quick correlation check:
ggscatter(filter(mydata, owner == 1), x = "orbisscale", y = "lnresidence",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wealth proxy in logs", ylab = "Market value of businessholdings")

#ggqqplot(visual$lnorbis, ylab = "Wealth proxy in logs")


gg_miss_var(filter(mydata, tangibles_filter == 1), show_pct = TRUE)

#X1 <- residence.nm$lnwage_g # can also take vector... normalize by (X'X) though

########################
#### TEST MCAR      #### 
########################
### set of filters:   

#### assets:

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

conditions <- c("owner", "other_estate", "assets_filter", "building_contract_filter",
                "life_insure_filter", "business_holdings_filter", "vehicles_filter", "tangibles_filter",
                "residence_debt_filter", "other_estate_debt_filter", "consumer_debt_filter", "education_debt_filter")
vars <- c("lnresidence", "lnestate", "lnassets", "lnbuilding", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
          "lnresidencedebt", "lnestatedebt", "lnconsumerdebt", "lneducationdebt")

# test names 
test.names <- c("Residence MV", "Other Estate MV", "Assets MV", "Build. Loan Contr.",
                "Life Insurance", "Company Shares", "Vehicles MV", "Tangibles",
                "Residence Debt", "Estate Debt", "Consumer Debt", "Study Loan")

test1 <- sapply(seq_along(conditions), 
                function(p) MCAR(data = filter(mydata, .data[[conditions[[p]]]] == 1), missvar = vars[p], instrument = "orbisscale", orthonormal.basis = "cosine")$Ratio)
test2 <- sapply(seq_along(conditions), 
                function(p) MCAR(data = filter(mydata, .data[[conditions[[p]]]] == 1), missvar = vars[p], instrument = "lnorbis")$Ratio)

########################
#### Little's Test  #### 
########################

test3 <- sapply(seq_along(conditions), 
                function(p) LittleMCAR(data.frame(filter(mydata, .data[[conditions[[p]]]] == 1)[,vars[p]], filter(mydata, .data[[conditions[[p]]]] == 1)$lnorbis))$chi.square / qchisq(.95, 1))


test4 <- sapply(seq_along(conditions), 
                function(p) boot::corr(select(filter(mydata, .data[[conditions[[p]]]] == 1 & !is.na(.data[[vars[[p]]]])), one_of("lnorbis", vars[p]))))


test.mat = cbind(test1, test2, test3, test4)

colnames(test.mat) <- c("MCAR Test (Cosine)", "MCAR Test (Hermite)", "Little's Test", "Correlation")
rownames(test.mat) <- test.names
test.mat

stargazer(test.mat, out = "mcar.tex", summary = F, digits = 3, notes = "SOEPv36 - TopW Data; Given is the ratio between test statistic and critical value")


############################################
#### TEST MAR: P(D=1|X1,Y^*)=P(D=1|X1)  ###### 
############################################

MAR(data = filter(mydata, other_estate == 1), missvar = "lnestate", instrument = "lnorbis", controls = "sex")



# function to set missing values:  datami <- prodNA(data, noNA = 0.1)



