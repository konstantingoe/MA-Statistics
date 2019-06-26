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
  mutate(sqmtrs          = ifelse(is.na(sqmtrs),median(sqmtrs, na.rm=T),sqmtrs)) %>% 
  mutate(lnsqrmtrs       = log(sqmtrs) - mean(log(sqmtrs))) %>%
  mutate(sqmtrscaled     = range01(log(sqmtrs))) %>% 
  mutate(saving_value    = ifelse(is.na(saving_value), mean(saving_value, na.rm=T),saving_value)) %>% 
  mutate(lnsaving        = log(1+saving_value) - mean(log(1+saving_value))) %>%
  mutate(savingscaled    = range01(log(1+saving_value))) %>% 
  mutate(hhnetto         = ifelse(is.na(hhnetto), mean(hhnetto, na.rm=T),hhnetto)) %>% 
  mutate(lnhhnetto       = log(1+hhnetto) - mean(log(1+hhnetto))) %>%
  mutate(hhnettoscaled   = range01(log(1+hhnetto))) %>% 
  mutate(total_inheritance = ifelse(is.na(total_inheritance), mean(total_inheritance, na.rm=T),total_inheritance)) %>% 
  mutate(lninheritance   = log(1+total_inheritance) - mean(log(1+total_inheritance))) %>%
  mutate(inheritscale    = range01(log(1+total_inheritance)))



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

mar.mat <- matrix(ncol=3, nrow=3)

mar.mat[1,1] <- MAR(data = filter(mydata, other_estate == 1), missvar = "lnestate", instrument = "orbisscale", controls = "inheritscale", orthonormal.basis = "cosine")$Ratio
mar.mat[1,2] <- MAR(data = filter(mydata, other_estate == 1), missvar = "lnestate", instrument = "lnorbis", controls = "lninheritance")$Ratio

mar.mat[2,1] <- MAR(data = filter(mydata, life_insure_filter == 1), missvar = "lnlife", instrument = "orbisscale", controls = "savingscaled", orthonormal.basis = "cosine")$Ratio
mar.mat[2,2] <- MAR(data = filter(mydata, life_insure_filter == 1), missvar = "lnlife", instrument = "lnorbis", controls = "lnsaving")$Ratio

mar.mat[3,1] <- MAR(data = filter(mydata, vehicles_filter == 1), missvar = "lnvehicles", instrument = "orbisscale", controls = "hhnettoscaled", orthonormal.basis = "cosine")$Ratio
mar.mat[3,2] <- MAR(data = filter(mydata, vehicles_filter == 1), missvar = "lnvehicles", instrument = "lnorbis", controls = "lnhhnetto")$Ratio


 # function to set missing values:  datami <- prodNA(data, noNA = 0.1)

###### Delgado's test ####

#that doesn't seem to be correct!
mar.mat[1,3] <- delgado(data = filter(mydata, other_estate == 1), missvar = "lnestate", instrument = "lnorbis", controls = "lninheritance")$Ratio
mar.mat[2,3]<- delgado(data = filter(mydata, life_insure_filter == 1), missvar = "lnlife", instrument = "lnorbis", controls = "lnsaving")$Ratio
mar.mat[3,3] <- delgado(data = filter(mydata, vehicles_filter == 1), missvar = "lnvehicles", instrument = "lnorbis", controls = "lnhhnetto")$Ratio


colnames(mar.mat) <- c("MAR Test (Cosine)", "MAR Test (Hermite)", "MAR Delgado")
rownames(mar.mat) <- c("Other Estate MV | Inheritances", "Life Insurance | Monthly savings", "Vehicles MV | HH netto income")

stargazer(mar.mat,out = "mar.tex", summary = F, digits = 3, notes = "SOEPv36 - TopW Data; Given is the ratio between test statistic and critical value")

mar.mat


# it fails... why?
test1 <- delgado(data = filter(mydata, other_estate == 1), missvar = "lnestate", instrument = "lnorbis", controls = "lninheritance")$Ratio

X <- filter(mydata, other_estate == 1)[,"lninheritance"]
n <- length(X)
W <- filter(mydata, other_estate == 1)[,"lnorbis"]
Y <- filter(mydata, other_estate == 1)[,"lnestate"]
delta <- ifelse(is.na(Y),0,1)

# Bandwith as in DELGADO & MANTEIGA (2001), see page 1482
h.c <- .005*n^{-1/3}

X.mat <- mat.or.vec(n,n)+X    
K.mat <- (1/(n^2*h.c))*k.fct((X.mat-t(X.mat))/h.c)

reg.object <- npreg(delta~X, bws=h.c, ckertype='gaussian', residuals=TRUE)
epsilon_hat <- residuals(reg.object)



V <- rbinom(n, 1,(1+5^.5)/sqrt(20))
V <-replace(V, V==1, (1-5^.5)/2)
V <-replace(V, V==0, (1+5^.5)/2)

boot.eps <- epsilon_hat*V
delta_star <- fitted(reg.object)+ boot.eps
delta_star.mat <- mat.or.vec(n,n)+ delta_star

#       test.fct<-  #selbe Funktion wie du unten nur mit delta_star anstatt delta (die funktion kann nat. aus dem B-Loop raus)
#         function(w,x){
#           sum((K.mat*(delta_star.mat-t(delta_star.mat)))*as.numeric(W<=w)*as.numeric(X<=x))}
# sum(mcmapply(test.fct,W,X, mc.cores = 12)^2)

# This is the C_n^** statistic see page 1480
f.xw <- function(x,w){as.numeric(X<=x)*as.numeric(W<=w)}

# even if n^-1 is missing it is weird!
fb <- sum(1/n*(rowSums(crossprod(K.mat*(delta_star.mat-t(delta_star.mat)), mapply(f.xw,X,W))))^2)
fb
 
