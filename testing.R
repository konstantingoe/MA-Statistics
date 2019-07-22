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
  mutate(lnjobduration   = log(jobduration_i) - log(mean(jobduration_i))) %>% 
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
  mutate(lnsqrmtrs       = log(sqmtrs) - log(mean(sqmtrs))) %>%
  mutate(sqmtrscaled     = range01(log(sqmtrs))) %>% 
  mutate(saving_value    = ifelse(is.na(saving_value), mean(saving_value, na.rm=T),saving_value)) %>% 
  mutate(lnsaving        = log(1+saving_value) - log(mean(1+saving_value))) %>%
  mutate(savingscaled    = range01(log(1+saving_value))) %>% 
  mutate(hhnetto         = ifelse(is.na(hhnetto), mean(hhnetto, na.rm=T),hhnetto)) %>% 
  mutate(lnhhnetto       = log(1+hhnetto) - log(mean(1+hhnetto))) %>%
  mutate(hhnettoscaled   = range01(log(1+hhnetto))) %>% 
  mutate(total_inheritance = ifelse(is.na(total_inheritance), mean(total_inheritance, na.rm=T),total_inheritance)) %>% 
  mutate(lninheritance   = log(1+total_inheritance) - log(mean(1+total_inheritance))) %>%
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
test3 <- sapply(seq_along(conditions), 
                function(p) MCAR(data = filter(mydata, .data[[conditions[[p]]]] == 1), missvar = vars[p], instrument = "lnorbis", orthonormal.basis = "bspline")$Ratio)

colnames(test.mat) <- c("MCAR Test (Cosine)", "MCAR Test (Hermite)", "MCAR Test (B-Spline)")
rownames(test.mat) <- test.names
test.mat



    s=-1.5     ####-1, -1.5, -2
    n <- length(filter(mydata, owner==1)$lnorbis)
    W <- filter(mydata, owner==1)$lnorbis
    Y <- filter(mydata, owner==1)$lnresidence
    m <- round(sqrt(n))
    c.eig <-m
    
    delta <- ifelse(is.na(Y),0,1)
    Weights= vector()
    for(j in 1:m) Weights[j] = j^s
    
    
    h.hat <- sum(delta/n) 
    if (orthonormal.basis == "cosine"){
      if (max(W) > 1 | min(W) < 0) {
        stop("Stop, cosine basis function only takes values in [0,1]")
      } else {
        b.fct <- function(i){cos.F(range01(W), i)} 
      }
    } else if (orthonormal.basis == "hermite"){
      b.fct <- function(i){hermite(W, i)/sqrt(factorial(i))}
    } else if (orthonormal.basis == "bspline"){
      knots<- expand.knots(seq(min(W),max(W)))
      b.fct <- function(i){bSpline(W, degree = i, Boundary.knots = range(W, na.rm = TRUE))} #Boundary.knots = range(W))} #/sqrt(factorial(i))}
      }
    BasWf.mat<-matrix(unlist(mclapply(1:m, b.fct, mc.cores = detectCores() -1)), n, m)
    coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
    coef2.vec <- sort(coef0.vec^2, decreasing = TRUE, index.return=TRUE)
    
    BasWf.mat <- BasWf.mat[,coef2.vec$ix]
    coef2.vec <-coef2.vec$x
    
    Test0 <- n*sum(Weights^2*coef2.vec)
    
    
    C.mat<-matrix(rnorm(c.eig*1000000),c.eig,1000000)^2 
    Wf.mat <- BasWf.mat%*%diag(Weights)
    
    eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
    eps.h.mat <- as.vector(delta-h.hat)%*%(delta%*%Wf.mat/n)
    eps.mat   <- eps.1.mat - eps.h.mat
    Sigma.mat<-t(eps.mat)%*%eps.mat/n
    eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)[["values"]]))
    CC<-sort(eig.vec[1:c.eig]%*%C.mat)
    
    Test0
    CC[950000]
    Test0/CC[950000]
    
    matplot(W, BasWf.mat, type = "l", ylab = "Piecewise B-spline bases")
    abline(v = knots, lty = 2, col = "gray")
    
    test <- list("n*Sn"= Test0, "Critical Value"= CC[950000], "Ratio"=Test0/CC[950000],
                 "Test decision"= if (Test0 > CC[950000]){
                   print("We reject the Null of MCAR")
                 } else {
                   print("We retain the Null of MCAR")})
    return(test)
    cat(paste("The test value is",Test0))
    cat(paste("While the critical value is",CC[950000]))
    cat(paste("Their ratio is", Test0/CC[950000]))
    
    if (Test0 > CC[950000]){
      cat("Consequently, we reject the Null of MCAR")
    } else {
      cat("Consequently, we retain the Null of MCAR")
      
    }
    

















########################
#### Little's Test  #### 
########################

#test3 <- sapply(seq_along(conditions), 
#                function(p) LittleMCAR(data.frame(filter(mydata, .data[[conditions[[p]]]] == 1)[,vars[p]], filter(mydata, .data[[conditions[[p]]]] == 1)$lnorbis))$chi.square / qchisq(.95, 1))


test4 <- sapply(seq_along(conditions), 
                function(p) boot::corr(select(filter(mydata, .data[[conditions[[p]]]] == 1 & !is.na(.data[[vars[[p]]]])), one_of("lnorbis", vars[p]))))


test.mat = cbind(test1, test2, test3, test4)


colnames(test.mat) <- c("MCAR Test (Cosine)", "MCAR Test (Hermite)", "Little's Test", "Correlation")
colnames(test.mat) <- c("MCAR Test (Cosine)", "MCAR Test (Hermite)", "MCAR Test (B-Spline)","Correlation")
rownames(test.mat) <- test.names
test.mat

stargazer(test.mat, out = "mcar.tex", summary = F, digits = 3, notes = "SOEPv36 - TopW Data; Given is the ratio between test statistic and critical value")


############################################
#### TEST MAR: P(D=1|X1,Y^*)=P(D=1|X1)  ###### 
############################################

MARcond <- c("owner", "other_estate", "assets_filter",
             "life_insure_filter", "business_holdings_filter", 
             "vehicles_filter", "tangibles_filter", "other_estate_debt_filter")
MARvars <- c("lnresidence", "lnestate", "lnassets", "lnlife", "lnbusiness", "lnvehicles", "lntangibles",
             "lnestatedebt")
MARxcos    <- c("inheritscale", "inheritscale", "savingscaled", "savingscaled", "hhnettoscaled", "hhnettoscaled", "inheritscale", "inheritscale") 
MARxhermite    <- c("lninheritance", "lninheritance", "lnsaving", "lnsaving", "lnhhnetto", "lnhhnetto", "lninheritance", "lninheritance") 


MAR1 <- sapply(seq_along(MARcond), 
                function(p) MAR(data = filter(mydata, .data[[MARcond[[p]]]] == 1), missvar = MARvars[p], instrument = "orbisscale", controls = MARxcos[p], orthonormal.basis = "cosine")$Ratio)

MAR2 <- sapply(seq_along(MARcond), 
               function(p) MAR(data = filter(mydata, .data[[MARcond[[p]]]] == 1), missvar = MARvars[p], instrument = "lnorbis", controls = MARxhermite[p])$Ratio)

MAR3 <- sapply(seq_along(MARcond), 
               function(p) MAR(data = filter(mydata, .data[[MARcond[[p]]]] == 1), missvar = MARvars[p], instrument = "lnorbis", controls = MARxhermite[p], orthonormal.basis = "bsplines")$Ratio)


mar.mat <- cbind(MAR1, MAR2, MAR3)

colnames(mar.mat) <- c("MAR Test (Cosine)", "MAR Test (Hermite)", "MAR Test (B-Spline)")
rownames(mar.mat) <- c("Residence MV | Inheritances", "Other Estate MV | Inheritances", "Assets MV | Monthly savings", "Life Insurance | Monthly savings", "Company Shares | HH netto income","Vehicles MV | HH netto income", "Tangibles | Inheritances", "Estate Debt | Inheritances")



s=-1.5     ####-1, -1.5, -2
n <- length(filter(mydata, other_estate_debt_filter==1)$lnorbis)
W <- filter(mydata, other_estate_debt_filter==1)$lnorbis
Y <- filter(mydata, other_estate_debt_filter==1)$lnestatedebt
X <- filter(mydata, other_estate_debt_filter==1)$lnhhnetto
m = round(sqrt(n))
c.eig <-m^2
delta <- ifelse(is.na(Y),0,1)


Weights= vector()
for(j in 1:(m^2)) Weights[j] = j^s

# LM<- lm(delta~X+X6+X7) $ coefficients
cv.h <- crs(delta~X, cv="exhaustive", segments.max = 10,degree.max=10, knots="uniform", kernel=FALSE)
#cv.h$K# LM<- lm(delta~X+X6+X7)
# summary(LM)
h.hat <- cv.h$fitted.values



knots<- expand.knots(seq(min(X),max(X), length=(cv.h$K[1,2])), order =max(cv.h$K[1,1],2))
#BasX.mat = cbind(1,gsl.bs(X,degree=max(cv.h$K[1,1],2), nbreak=max(2,cv.h$K[1,2])))

BasX.mat = cbind(1,gsl.bs(X,degree=max(4,2), nbreak=max(2,cv.h$K[1,2])))

h.hat <- BasX.mat%*%ginv(t(BasX.mat)%*%BasX.mat)%*%t(BasX.mat)%*%delta

  knots<- expand.knots(seq(min(W),max(W)))
  b.fct <- function(i){bSpline(W, degree = i, knots = knots)} #Boundary.knots = range(W, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
  b.fctW <- function(i){bSpline(W, degree = i, Boundary.knots = range(W, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
  BasWh.mat=mat.or.vec(n,m)
  BasWh.mat <- b.fctW(m)

  knots<- expand.knots(seq(min(X),max(X)))
  b.fct <- function(i){bSpline(X, degree = i, knots = knots)} #Boundary.knots = range(X, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
  b.fctX <- function(i){bSpline(X, degree = i, Boundary.knots = range(X, na.rm = TRUE))} #knots = knots} #/sqrt(factorial(i))}
  BasXh.mat=mat.or.vec(n,m)
  BasXh.mat <- b.fctX(m)
  
BasWf.mat=mat.or.vec(n,m^2)
BasWf.mat <- mat.or.vec(n,(length(BasWh.mat[1,])*length(BasXh.mat[1,])))
for( i in 1:n){ BasWf.mat[i,] <- kronecker(BasWh.mat[i,],BasXh.mat[i,])}


coef0.vec <- t(delta-h.hat)%*%BasWf.mat/n
coef2.vec <- sort(coef0.vec^2, decreasing = TRUE, index.return=TRUE)

BasWf.mat <- BasWf.mat[,coef2.vec$ix]
coef2.vec <-coef2.vec$x

Test0 <- n*sum(Weights^2*coef2.vec)

C.mat<-matrix(rnorm(c.eig*1000000),c.eig,1000000)^2 
Wf.mat <- BasWf.mat%*%diag(Weights)

eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
eps.h.mat <- diag(as.vector(delta-h.hat))%*%BasX.mat%*%t(BasX.mat)%*%Wf.mat/n
eps.mat  <- eps.1.mat - eps.h.mat
Sigma.mat<-t(eps.mat)%*%eps.mat/n
eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
CC<-sort(eig.vec[1:c.eig]%*%C.mat)
Test0
CC[950000]
Test0/CC[950000]







 # function to set missing values:  datami <- prodNA(data, noNA = 0.1)

###### Delgado's test ####

#that doesn't seem to be correct!
#mar.mat[1,3] <- delgado(data = filter(mydata, other_estate == 1), missvar = "lnestate", instrument = "lnorbis", controls = "lninheritance")$Ratio
#mar.mat[2,3]<- delgado(data = filter(mydata, life_insure_filter == 1), missvar = "lnlife", instrument = "lnorbis", controls = "lnsaving")$Ratio
#mar.mat[3,3] <- delgado(data = filter(mydata, vehicles_filter == 1), missvar = "lnvehicles", instrument = "lnorbis", controls = "lnhhnetto")$Ratio



stargazer(mar.mat,out = "mar.tex", summary = F, digits = 3, notes = "SOEPv36 - TopW Data; Given is the ratio between test statistic and critical value")

mar.mat


