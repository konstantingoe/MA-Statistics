##### Testing MAR ######
rm(list = ls())
source("packages.R")
source(".path.R")
mydata <- import(paste(path, "topwealth_cleaned.dta", sep = "/"))

##### Testing MAR for Residence Value ####

residence <- select(mydata, one_of(c("schicht", "age", "sex", "orbis_wealth", "jobduration", "wage_gross_m", "wage_net_m", "inherit_filter", "owner", "residence_value", "residence_value_limits"))) %>% 
  filter(owner == 1) %>% 
  select(-owner)

visual <- residence %>% 
  filter(inherit_filter ==0) %>% 
  mutate(lnorbis = log(orbis_wealth)) %>% 
  mutate(lnresidence = log(residence_value))

ggscatter(visual, x = "lnorbis", y = "lnresidence",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Wealth proxy in logs", ylab = "Market value of primary residence in logs")

ggqqplot(visual$lnorbis, ylab = "Wealth proxy in logs")


### for quick check just use listwise deletion except for residence value:

residence.nm <- residence %>% 
  filter(!is.na(jobduration) & !is.na(residence_value_limits) & !is.na(inherit_filter) & !is.na(wage_gross_m) & !is.na(wage_net_m)) %>% 
  mutate(lnorbis = log(orbis_wealth) - log(mean(orbis_wealth, na.rm = T))) %>% 
  mutate(lnresidence = log(residence_value)- log(mean(residence_value, na.rm = T))) %>% 
  mutate(lnwage_g = log(1+wage_gross_m) - log(mean(residence_value, na.rm = T)))

gg_miss_var(residence.nm, show_pct = TRUE)

residence.nm <- residence.nm %>% 
  slice(1:400)
##### testing #####
### Things that do not work:

# Do I have to have square numbers as number of observations?
# Because otherwise Kronecker's product would give me a different number than n

# C.mat how do I know the C.mat value...

# What if Test0 gives me a value of Infinity?
# Hard coded critical values... what are the actual ones? also check tomorrow in the paper
# test <- (Test0 >CC[9500])


s.vec = c(-1, -1.5, -2)

n <- nrow(residence.nm)
#N <- 1000
m <- sqrt(nrow(residence.nm))
c.eig <-m^2

delta <- ifelse(is.na(residence.nm$residence_value),0,1)
ones<-rep(1,1,n)

cos.F<-function(x,j){sqrt(2)*cos((j)*pi*x)}

set.seed(73457)

s =s.vec[1]
Weights= vector()
for(j in 1:(m^2)) {Weights[j] = j^s}
  
W <- residence.nm$lnorbis
X <- residence.nm$lnwage_g
Y <- residence.nm$lnresidence

cv.h <- crs(delta~X, cv="exhaustive",degree.max=3, segments.max = 5, knots="uniform")
h.hat <- cv.h$fitted.values
sum((delta-h.hat)^2)
knots<- expand.knots(seq(min(X),max(X), length=(cv.h$K[2])), order =cv.h$K[1])
BasX.mat = gsl.bs(X,degree=cv.h$K[1], nbreak=(cv.h$K[2]))
BasX.mat =cbind(ones,BasX.mat)

BasWh.mat=mat.or.vec(n,m)
for(i in 1:m){BasWh.mat[,i] <- hermite(W, i)/sqrt(factorial(i))}
BasXh.mat=mat.or.vec(n,m)
for(i in 1:m){BasXh.mat[,i] <- hermite(X, i)/sqrt(factorial(i))}


BasWf.mat=mat.or.vec(n,m^2)
BasWf.mat <- mat.or.vec(n,(length(BasWh.mat[1,])*length(BasXh.mat[1,])))
for( i in 1:n){ BasWf.mat[i,] <- kronecker(BasWh.mat[i,],BasXh.mat[i,])}


coef0.vec <- t(BasWf.mat)%*%(delta-h.hat)/n
Test0 <- n*sum((Weights*coef0.vec) *(Weights*coef0.vec))

C.mat<-matrix(rnorm(c.eig*10000),c.eig,10000)^2 
Wf.mat <- BasWf.mat%*%diag(Weights)# 

eps.1.mat <- diag(as.vector(delta-h.hat))%*%Wf.mat
eps.h.mat <- diag(as.vector(delta-h.hat))%*%BasX.mat%*%t(BasX.mat)%*%Wf.mat/n
eps.mat  <- eps.1.mat - eps.h.mat
Sigma.mat<-t(eps.mat)%*%eps.mat/n
eig.vec<-rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
CC<-sort(eig.vec[1:c.eig]%*%C.mat)
test <- (Test0 >CC[m])







