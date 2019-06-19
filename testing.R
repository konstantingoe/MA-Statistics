##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
source("functions.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))

##### Testing MAR for Residence Value ####

residence <- select(mydata, one_of(c("schicht", "age", "sex", "orbis_wealth", "jobduration", "wage_gross_m", "wage_net_m", "inherit_filter", "owner", "residence_value", "residence_value_limits", "sqmtrs", "total_inheritance"))) %>% 
  filter(owner == 1) %>% 
  select(-owner)

business <- select(mydata, one_of(c("schicht", "age", "sex", "orbis_wealth", "jobduration", "wage_gross_m", "wage_net_m", "inherit_filter", "business_holdings_filter", "business_holdings", "business_holdings_limits"))) %>%
  filter(business_holdings_filter == 1) %>% 
  select(-business_holdings_filter) %>% 
  mutate(lnorbis = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>% 
  mutate(lnbusiness = log(business_holdings) - log(mean(business_holdings, na.rm = T)))

            
            
            
            
visual <- residence %>% 
  filter(inherit_filter ==0) %>% 
  mutate(lnorbis = log(orbis_wealth)) %>% 
  mutate(lnresidence = log(residence_value))

ggscatter(business, x = "lnorbis", y = "lnbusiness",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wealth proxy in logs", ylab = "Market value of businessholdings")

ggqqplot(visual$lnorbis, ylab = "Wealth proxy in logs")


### for quick check just use listwise deletion except for residence value:

residence.nm <- residence %>% 
  #filter(!is.na(jobduration) & !is.na(residence_value_limits) & !is.na(inherit_filter) & !is.na(wage_gross_m) & !is.na(wage_net_m)) %>% 
  mutate(lnorbis = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>% 
  mutate(lnresidence = log(residence_value)- log(mean(residence_value, na.rm = T))) %>% 
  mutate(lnwage_g = log(1+wage_gross_m) - log(mean(residence_value, na.rm = T))) %>% 
  mutate(Y = ifelse(sex==2,NA, lnresidence)) %>% 
  mutate(jobduration_i = ifelse(is.na(jobduration),median(jobduration), jobduration)) %>% 
  mutate(lnjobduration = log(jobduration_i) - mean(log(jobduration_i))) %>% 
  mutate(lnsqmtrs = log(sqmtrs) - log(mean(sqmtrs, na.rm = T)))
  

gg_miss_var(residence.nm, show_pct = TRUE)

#residence.nm <- residence.nm %>% 
  #slice(1:400)
##### testing #####
### Things that do not work:

# Do I have to have square numbers as number of observations?
# Because otherwise Kronecker's product would give me a different number than n

# C.mat how do I know the C.mat value...

# What if Test0 gives me a value of Infinity?
# Hard coded critical values... what are the actual ones? also check tomorrow in the paper
# test <- (Test0 >CC[9500])

X1 <- residence.nm$lnwage_g # can also take vector... normalize by (X'X) though

########################
#### TEST MCAR      #### 
########################


MCAR(data = filter(residence.nm, !is.na(lnsqmtrs)), missvar = "residence_value", instrument = "lnsqmtrs")

MCAR(data = business, missvar = "lnbusiness", instrument = "lnorbis", orthonormal.basis = "cosine")





############################################
#### TEST MAR: P(D=1|X1,Y^*)=P(D=1|X1)  ###### 
############################################
m <- 3 #round(sqrt(nrow(residence.nm)))
c.eig <-m^2

Weights= vector()
for(j in 1:(m^2)) Weights[j] = j^s

cv.h <- crs(delta~X1, cv="exhaustive", segments.max = 10,degree.max=10, knots="uniform", kernel=FALSE)
cv.h$K# LM<- lm(delta~X+X6+X7)
# summary(LM)
h.hat <- cv.h$fitted.values



knots<- expand.knots(seq(min(X1),max(X1), length=(cv.h$K[1,2])), order =max(cv.h$K[1,1],2))
BasX.mat = cbind(1,gsl.bs(X1,degree=max(cv.h$K[1,1],2), nbreak=max(2,cv.h$K[1,2])))

BasX.mat = cbind(1,gsl.bs(X1,degree=max(4,2), nbreak=max(2,cv.h$K[1,2])))

h.hat <- BasX.mat%*%ginv(t(BasX.mat)%*%BasX.mat)%*%t(BasX.mat)%*%delta
BasWh.mat=mat.or.vec(n,m)
for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}

BasXh.mat=mat.or.vec(n,m)
for(i in 1:m){BasXh.mat[,i] <- hermite(X1, i)/sqrt(factorial(i))}



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


########################
#### Little's Test  #### 
########################
data.fr <-data.frame(residence.nm$lnresidence,residence.nm$lnorbis)
LittleMCAR(data.fr)$chi.square
LittleMCAR(data.fr)$p.value
LittleMCAR(data.fr)$amount.missing
# little rejects MCAR for residence


data.fr <-data.frame(business$lnbusiness,business$lnorbis)
LittleMCAR(data.fr)$chi.square
LittleMCAR(data.fr)$p.value
LittleMCAR(data.fr)$amount.missing
# also does not reject for business assets








