####

source("packages.R")


dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
dag
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")
dag <- set.arc(dag, from = "E", to = "R")
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")
dag

# identical:

dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
                    byrow = TRUE, ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set

modelstring(dag2)
nodes(dag2)
arcs(dag2)
#DAG's are acyclical!!! 

A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")

A.prob <- array(c(0.30, 0.50, 0.20), dim = 3,
                dimnames = list(A = A.lv))
A.prob

S.prob <- array(c(0.60, 0.40), dim = 2,
                dimnames = list(S = S.lv))
S.prob

O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2, 2),
                dimnames = list(O = O.lv, E = E.lv))
O.prob

R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim = c(2, 2),
                dimnames = list(R = R.lv, E = E.lv))
R.prob

E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                  0.36, 0.70, 0.30, 0.90, 0.10), dim = c(2, 3, 2),
                  dimnames = list(E = E.lv, A = A.lv, S = S.lv))
T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                  0.24, 0.18, 0.70, 0.21, 0.09), dim = c(3, 2, 2),
                  dimnames = list(T = T.lv, O = O.lv, R = R.lv))

#creating the BN
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
all.equal(dag2, dag3)

cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob,
            R = R.prob, T = T.prob)
bn <- custom.fit(dag2, cpt)
nparams(bn)
arcs(bn)
bn$T
R.cpt <- coef(bn$R)
bn

# now with real data

survey <- read.table("survey.txt", header = TRUE)
head(survey)

# parameters to estimate: conditional probabilities in the local distributions

#fit the parameters for the local distributions:
bn.mle <- bn.fit(dag, data = survey, method = "mle")
bn.mle$O
bn.bayes <- bn.fit(dag, data = survey, method = "bayes",
                   iss = 10)
bn.bayes$O

#conditional independence tests: focus on presence of different arcs
# since each arc encodes probabilistic dependence the test can be used to assess whether 
# that dependence is supported by the data

# if test rejects the Null, arc can be included in the DAG

# number of degrees of freedom for education -> travel:

(nlevels(survey[, "T"]) - 1) * (nlevels(survey[, "E"]) - 1) *
(nlevels(survey[, "O"]) * nlevels(survey[, "R"]))

# mutual information test from information theory:
ci.test("T", "E", c("O", "R"), test = "mi", data = survey)
# Pearsons X^2 test:
ci.test("T", "E", c("O", "R"), test = "x2", data = survey)

# that way we can remove arcs that are not supported by the data:
ci.test("T", "O", "R", test = "x2", data = survey)

#to do this for all:

arc.strength(dag, data = survey, criterion = "x2")

# network scores focus on the DAG as a whole: GOF statistics that measure how well
# the DAG mirrors the dependence structure of the data (e.g. BIC)

#Bayesian Dirichlet equivalent uniform (BDeu) posterior probability of
#the DAG associated with a uniform prior over both the space of the DAGs and
#of the parameters

#the higher BIC/BD the better the fit of the DAG to the data

score(dag, data = survey, type = "bic")
score(dag, data = survey, type = "bde", iss = 10)
# fot the BDe we have to specify imaginary sample size for computation of the posterior estimates
# corresponds to the weight assigned to the flat prior distribution 
score(dag, data = survey, type = "bde", iss = 1)
# the lower the iss the closer BDe is to BIC

# evaluate a DAG that also includes Education -> Transport:

dag4 <- set.arc(dag, from = "E", to = "T")
nparams(dag4, survey)
score(dag4, data = survey, type = "bic")
# not beneficial 

# also useful to compare completely different DAG's e.g. by randomly selecting one:

rnd <- random.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(rnd)
score(rnd, data = survey, type = "bic")

#yet there are learning algorithms: searching for the DAG that maximises a given network score

# e.g. hill climbing 

learned <- hc(survey)
modelstring(learned)
score(learned, data = survey, type = "bic")

learned2 <- hc(survey, score = "bde")
arc.strength(learned, data = survey, criterion = "bic")
# from the learned score, removing any will result in a decrease of BIC
# this is not true when using the DAG that we specified:
arc.strength(dag, data = survey, criterion = "bic")
# removing O-->T would increase BIC 

#testing conditional independence via d-separation 
dsep(dag, x = "S", y = "R")
dsep(dag, x = "O", y = "R")
path(dag, from = "S", to = "R")
dsep(dag, x = "S", y = "R", z = "E")
dsep(dag, x = "O", y = "R", z = "E")
dsep(dag, x = "A", y = "S")
dsep(dag, x = "A", y = "S", z = "E")

#----------------------------------------------------#
#       Exact Inference                              #
#----------------------------------------------------#
# transform BN into a tree
junction <- compile(as.grain(bn))

#attitudes of women towards car
#and train use compared to the whole survey sample

querygrain(junction, nodes = "T")$T
jsex <- setEvidence(junction, nodes = "S", states = "F")
querygrain(jsex, nodes = "T")$T

# women show about the same preferences towards car and train use as the interviewees as a whole


#living in a small city affects car and train use?
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jres, nodes = "T")$T

jedu <- setEvidence(junction, nodes = "E", states = "high")
SxT.cpt <- querygrain(jedu, nodes = c("S", "T"),
                      type = "joint")
SxT.cpt
querygrain(jedu, nodes = c("S", "T"), type = "marginal")


querygrain(jedu, nodes = c("T", "S"), type = "conditional")

dsep(bn, x = "S", y = "T", z = "E")

SxT.ct = SxT.cpt * nrow(survey)
chisq.test(SxT.ct)

#----------------------------------------------------#
#       Approximate Inference                        #
#----------------------------------------------------#

#using monte carlo simulations to randomly generate observations from the BN


# 5000 * nparam(BN)
cpquery(bn, event = (S == "M") & (T == "car"),
         evidence = (E == "high"))

#10^6 * nparam(BN)
cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = (E == "high"), n = 10^6)

#probability of a man travelling by car given that his Age is young and his
#Education is uni or that he is an adult, regardless of his Education.
cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = ((A == "young") & (E == "uni")) | (A == "adult"))

SxT <- cpdist(bn, nodes = c("S", "T"),
              evidence = (E == "high"))
head(SxT)

prop.table(table(SxT))

# Graphical Implementation
graphviz.plot(dag)
graphviz.plot(dag, layout = "fdp")
graphviz.plot(dag, layout = "circo")
hlight <- list(nodes = nodes(dag), arcs = arcs(dag),
               col = "grey", textCol = "grey")
pp <- graphviz.plot(dag, highlight = hlight)
graph::edgeRenderInfo(pp) <- list(col = c("S~E" = "black", "E~R" = "black"),
   lwd = c("S~E" = 3, "E~R" = 3))
graph::nodeRenderInfo(pp) <- list(col = c("S" = "black", "E" = "black", "R" = "black"),
         textCol = c("S" = "black", "E" = "black", "R" = "black"),
         fill = c("E" = "grey"))
Rgraphviz::renderGraph(pp)

#Plotting Conditional Probability Distributions

bn.fit.barchart(bn.mle$T, main = "Travel",
                xlab = "Pr(T | R,O)", ylab = "")
bn.fit.dotplot(bn.mle$T, main = "Travel",
                xlab = "Pr(T | R,O)", ylab = "")
Evidence <- factor(c(rep("Unconditional",3), rep("Female", 3),
            rep("Small City",3)),
            levels = c("Unconditional", "Female", "Small City"))
Travel <- factor(rep(c("car", "train", "other"), 3),
                levels = c("other", "train", "car"))
distr <- data.frame(Evidence = Evidence, Travel = Travel,
                      Prob = c(0.5618, 0.2808, 0.15730, 0.5620, 0.2806,
                              0.1573, 0.4838, 0.4170, 0.0990))
head(distr)

barchart(Travel ~ Prob | Evidence, data = distr,
         layout = c(3, 1), xlab = "probability",
         scales = list(alternating = 1, tck = c(1, 0)),
         strip = strip.custom(factor.levels =
                              c(expression(Pr(T)),
                              expression(Pr({T} * " | " * {S == F})),
                              expression(Pr({T} * " | " * {R == small})))),
         panel = function(...) {
         panel.barchart(...)
         panel.grid(h = 0, v = -1)
         })

#------------------------------------------------------------------#
#####       Continuous Case: Gaussian BN                        ####
#------------------------------------------------------------------#

# Model continuous data under multivariate normal assumption:

dag.bnlearn <- model2network("[G][E][V|G:E][N|V][W|V][C|N:W]")
dag.bnlearn

nano <- nodes(dag.bnlearn)
 for (n1 in nano) {
   for (n2 in nano) {
     if (dsep(dag.bnlearn, n1, n2))
       cat(n1, "and", n2, "are independent.\n")
     }#FOR
   }#FOR

for (n1 in nano[nano != "V"]) {
   for (n2 in nano[nano != "V"]) {
     if (n1 < n2) {
       if (dsep(dag.bnlearn, n1, n2, "V"))
         cat(n1, "and", n2, "are independent given V.\n")
       }#THEN
     }#FOR
   }#

#Probabilistic representation
disE <- list(coef = c("(Intercept)" = 50), sd = 10)
disG <- list(coef = c("(Intercept)" = 50), sd = 10)
disV <- list(coef = c("(Intercept)" = -10.35534,
                      E = 0.70711, G = 0.5), sd = 5)
disN <- list(coef = c("(Intercept)" = 45, V = 0.1),
                sd = 9.949874)

disW <- list(coef = c("(Intercept)" = 15, V = 0.7),
             sd = 7.141428)
disC <- list(coef = c("(Intercept)" = 0, N = 0.3, W = 0.7),
               sd = 6.25)
dis.list = list(E = disE, G = disG, V = disV, N = disN,
                  W = disW, C = disC)

gbn.bnlearn <- custom.fit(dag.bnlearn, dist = dis.list)
print(gbn.bnlearn)

# we have created a linear Gaussian Bayesian Network:
# with the following assumptions:
# 1. each node follows a normal distribution
# 2. root nodes are solely described by the marginal distribution
# 3. each node has a variance that is specific to that node and does not depend on the values of the parents
# 4. the local distribution of each node can be equivalently expressed as a Gaussian linear model which includes an intercept and the node’s parents as explanatory variables

# concentrate on GBN:
gbn.rbmn <- bnfit2nbn(gbn.bnlearn)
gema.rbmn <- nbn2gema(gbn.rbmn)
mn.rbmn <- gema2mn(gema.rbmn)
print8mn(mn.rbmn)
str(mn.rbmn)

#Estimating the Parameters: Correlation Coefficients
cropdata1 <- import("cropdata1.txt")
dim(cropdata1)
round(head(cropdata1), 2)

# bn.fit automatically adapts to the data type 

est.para <- bn.fit(dag.bnlearn, data = cropdata1)
#assign the return value of a fit to directly to the corresponding node
est.para$C <- lm(C ~ N + W, data = cropdata1)

est.para$C <- penalized(C ~ N + W, lambda1 = 0, lambda2 = 1.5,
                        data = cropdata1)

est.para$E
est.para$C

# interecept true=0 , estimated 0 2.4069 
# fit null intercept:
est.para$C <- lm(C ~ N + W - 1, data = cropdata1)
est.para$C
lmC <- lm(C ~ N + W, data = cropdata1[, c("N", "W", "C")])
coef(lmC)
confint(lmC)

#Tests and Scores

cormat <- cor(cropdata1[, c("C", "W", "N")])
invcor <- cor2pcor(cormat)
dimnames(invcor) <- dimnames(cormat)
invcor
invcor["C", "W"]

#similarly:
ci.test("C", "W", "N", test = "cor", data = cropdata1)
#use learning algorithm
stru1 <- iamb(cropdata1, test = "cor")
#differs from bn.learn slightly --> the V -N arc is missing thus 
#we can make this arc mandatory by putting it on a whitelist: 
wl <- matrix(c("V", "N"), ncol = 2)
wl
stru2 <- iamb(cropdata1, test = "cor", whitelist = wl)
all.equal(dag.bnlearn, stru2)

#more data learns the DAG correctly
cropdata2 <- import("cropdata2.txt")
stru3 <- iamb(cropdata2, test = "cor")
all.equal(dag.bnlearn, stru3)

#### Network Scores of GBNs ####

score(dag.bnlearn, data = cropdata2, type = "bic-g")

score(dag.bnlearn, data = cropdata2, type = "bge")

##### Inference with GBN #####

# again we are interested in the probability of an event or in
#the distribution of some random variables

#nbn is defined via the GBN's local distribution:
print8nbn(gbn.rbmn)
#str(gbn.rbmn)

#gema describes the GBN by two generating matrices:
#1. vector of expectations and 2. a matrix to be multiplied by a N(0, 1) white noise
print8gema(gema.rbmn)
#read as: V = 50 + 7.071E1 + 5E2 + 5E3, where E1,...,E6 are i.i.d. N(0,1) variables. 

#use condi4joint() for conditional joint distributions of one or more nodes 


print8mn(condi4joint(mn.rbmn, par = "C", pour = "V", x2 = 80))
print8mn(condi4joint(mn.rbmn, par = "V", pour = "C", x2 = 80))
#symmetric distribution 

unlist(condi4joint(mn.rbmn, par = "C", pour = "V", x2 = NULL))

#### Approximate Inference ####

nbs <- 4
VG <- rnorm(nbs, mean = 50, sd = 10)
VE <- rnorm(nbs, mean = 50, sd = 10)
VV <- rnorm(nbs, mean = -10.355 + 0.5 * VG + 0.707 * VE,
                 sd = 5)
VN <- rnorm(nbs, mean = 45 + 0.1 * VV, sd = 9.95)
cbind(VV, VN)

#or quicker: 
sim <- rbn(gbn.bnlearn, n = 4)
sim[, c("V", "N")]

#make probability assertions about intervals:

head(cpdist(gbn.bnlearn, nodes = c("C", "N", "W"),
             evidence = (C > 80)))
#likelihood weighting due to the fact that single values have probability zero in continuous cases
head(cpdist(gbn.bnlearn, nodes = c("V"),
            evidence = list(G = 10, E = 90), method = "lw"))

cpquery(gbn.bnlearn, event = (V > 70),
         evidence = list(G = 10, E = 90), method = "lw")

# Plotting GBN's
igraph.options(print.full = TRUE)
dag0.igraph <- graph.formula(G-+V, E-+V, V-+N, V-+W,
                             N-+C, W-+C)
dag0.igraph
dag.igraph <- igraph.from.graphNEL(as.graphNEL(dag.bnlearn))
V(dag.igraph)
E(dag.igraph)


par(mfrow = c(2, 2), mar = rep(3, 4), cex.main = 2)
 plot(dag.igraph, main = "\n1: defaults")
 dag2 <- dag.igraph
 V(dag2)$label <- V(dag2)$name
 plot(dag2, main = "\n2: with labels")
 ly <- matrix(c(2, 3, 1, 1, 2, 3,
                 1, 4, 4, 2, 3, 2), 6)
 plot(dag2, layout = ly, main = "\n3: positioning")
 colo <- c("black", "darkgrey", "darkgrey", rep(NA, 3))
 lcolo <- c(rep("white", 3), rep(NA, 3))
 par(mar = rep(0, 4), lwd = 1.5)
 plot(dag2, layout = ly, frame = TRUE,
        main = "\n4: final",
        vertex.color = colo, vertex.label.color = lcolo,
        vertex.label.cex = 3, vertex.size = 50,
        edge.arrow.size = 0.8, edge.color = "black")
 
 
 # display conditional probabilities
 
 gbn.fit <- bn.fit(dag.bnlearn, cropdata2)
 bn.fit.qqplot(gbn.fit)
 bn.fit.qqplot(gbn.fit$V)
 try(bn.fit.qqplot(gbn.bnlearn))
 C.EV <- condi4joint(mn.rbmn, par = "C", pour = c("E", "V"),
                     x2 = NULL)
 C.EV$rho
 dsep(gbn.bnlearn, "E", "C", "V")

 set.seed(5678)
 cropdata3 <- cpdist(gbn.bnlearn, nodes = c("E", "V", "C"),
                     evidence = TRUE, n = 1000)
  plot(cropdata3$V, cropdata3$C, type = "n",
         main = "C | V, E; E is the point size")
  cexlim <- c(0.1, 2.4)
  cexE <- cexlim[1] + diff(cexlim) / diff(range(cropdata3$E)) *
   (cropdata3$E - min(cropdata3$E))
  points(cropdata3$V, cropdata3$C, cex = cexE)
  cqa <- quantile(cropdata3$C, seq(0, 1, 0.1))
 abline(h = cqa, lty = 3)
 
 
#--------------------------------------------------------------------#
####                Hybrid Bayesian Networks                      ####
#--------------------------------------------------------------------# 

# Actually we can mix discrete and continuous variables and
# we can use any kind of distribution. 
 
 library(rjags)
 
 sp <- c(0.5, 0.5)
 mu <- c(6.1, 6.25)
 sigma <- 0.05
 jags.data <- list(sp = sp, mu = mu, sigma = sigma,
                   cdiam = 6.20)
 model1 <- jags.model(file = "inclu.sc.jam", data = jags.data)

 update(model1, n.iter = 10000)
 simu1 <- coda.samples(model = model1, variable.names = "csup",
                       n.iter = 20000, thin = 20)
 sim1 <- simu1[[1]] 
 sum(sim1 == 1) / length(sim1)

 # quite close to the theoretical value:
 d.s1 <- dnorm(6.2, mean = mu[1], sd = sigma)
 d.s2 <- dnorm(6.2, mean = mu[2], sd = sigma)
 d.s1 / (d.s1 + d.s2)

 # discretizing continuous variables
 
 limits <- c(6.16, 6.19)
 dsd <- matrix(c(diff(c(0, pnorm(limits, mu[1], sigma), 1)),
                 diff(c(0, pnorm(limits, mu[2], sigma), 1))),
                 3, 2)
 dimnames(dsd) <- list(D = c("thin", "average", "thick"),
                       S = c("s1", "s2")) 
 dsd

 #joint distribution by multiplying dsd by the probability of each s (law of total probability)
 
 jointd <- dsd * sp
# conditional probability of S given D:
 dds <- t(jointd / rowSums(jointd))
 dds 

 
 ###### Using different distributions than multinomial/multinormal #####
 
 dat0 <- list(p.PR = c(0.7, 0.2, 0.1),
              a.CL = 3, b.CL = 1,
              g.G1 = c(1, 3, 10),
              k.G2 = 10,
              m.TR = 5, s.TR = 2.5,
              r.LO = 1/3, d.LO = 1)

 # exploring 
 
 exp.loss <- rep(NA, 3)
 names(exp.loss) <- paste("PR=", 1:3, sep = "")
 qua.loss <- exp.loss
 for (PR in 1:3) {
    dat1 <- dat0
    dat1$PR <- PR
    mopest <- jags.model(file = "inclu.pest.jam", data = dat1,
                         quiet = TRUE)
    update(mopest, 3000)
    sipest <-
    coda.samples(model = mopest, variable.names = "LO",
                     n.iter = 50000)
    summa <- summary(sipest)
    exp.loss[PR] <- summa$statistics["Mean"]
    qua.loss[PR] <- summa$quantiles["75%"]
    }#FOR
  mean3 <- mean(sipest[[1]][, "LO"])
  round(c(exp.loss, MEAN = mean(exp.loss)), 1) 
 

 ###### Theoretic Motivation #####
  
X <- paste("[X1][X3][X5][X6|X8][X2|X1][X7|X5][X4|X1:X2]",
           "[X8|X3:X7][X9|X2:X7][X10|X1:X9]", sep = "")
dag <- model2network(X)
 
skel <- skeleton(dag) 
vstructs(dag) 
cp1 <- cpdag(dag) 
dsep(dag, x = "X9", y = "X5", z = c("X2", "X7", "X10"))
# identify markov blanket nodes
mb(dag, node = "X9")
mb(dag, node = "X7")

par.X9 <- bnlearn::parents(dag, node = "X9")
ch.X9 <- bnlearn::children(dag, node = "X9") 
sp.X9 <- sapply(ch.X9, bnlearn::parents, x = dag) 
sp.X9 <- sp.X9[sp.X9 != "X9"]
unique(c(par.X9, ch.X9, sp.X9))

V <- setdiff(nodes(dag), "X9")
S <- mb(dag, "X9")
sapply(setdiff(V, S), dsep, bn = dag, y = "X9", z = S)

V <- setdiff(nodes(dag), "X7")
S <- mb(dag, "X7")
sapply(setdiff(V, S), dsep, bn = dag, y = "X7", z = S)

belongs <- logical(0)
for (node in S)
  belongs[node] <- "X7" %in% mb(dag, node)
belongs


#### Moral Graphs ####

#Just another graphical representation derived from the DAG
mg1 <- moral(dag)
all.equal(moral(dag),
          moral(set.arc(dag, from = "X7", to = "X3")))

mg2 <- dag
vs <- vstructs(dag)
for (i in seq(nrow(vs)))
   mg2 <- set.edge(mg2, from = vs[i, "X"], to = vs[i, "Y"],
                   check.cycles = FALSE)
mg2 <- skeleton(mg2)
all.equal(mg1, mg2)

#Moralization transforms BN into Markov Network 

###################################
#                                 #
#### Bayesan Network Learning #####
#                                 #
###################################


#Grow Shrink structure learning algorithm 

bn.cor <- gs(cropdata1, test = "cor", alpha = 0.05)
modelstring(bn.cor)
#missing the V-N arc; the small sample size seems to reduce the power of the test
# use Fischer's Z- test
bn.zf <- gs(cropdata1, test = "zf", alpha = 0.05)
# or Monte Carlo test
bn.mc <- gs(cropdata1, test = "mc-cor", B = 1000)
all.equal(bn.zf,bn.mc)
all.equal(bn.cor, bn.mc)
#still not the real structure
bn.iamb <- iamb(cropdata1, test = "cor", alpha = 0.05)
all.equal(bn.cor, bn.iamb)
gs(cropdata1, test = "cor", alpha = 0.05, debug = TRUE)

#include by hand:
bn.cor <- gs(cropdata1, test = "cor", alpha = 0.05,
             whitelist = c("V", "N"))
all.equal(bn.cor, dag.bnlearn)

# Score based algorithms

learned <- hc(survey, score = "bic")
modelstring(learned)
score(learned, data = survey, type = "bic")

learned <- hc(survey, score = "bic", debug = T)
#start search at random graph
hc(survey, score = "bic", start = random.graph(names(survey)))

# Hybrid algorithms:

# MMHC is implemented in bnlearn in the mmhc function
mmhc(survey)

rsmax2(survey, restrict = "mmpc", maximize = "hc")


#rsmax2(survey, restrict = "si.hiton.pc", test = "x2", 
#       maximize = "tabu", score = "bde", maximize.args = list(iss = 5))
#

#-----------------------------------------------------------------------------#
                    ###### Parameter Learning ######
#-----------------------------------------------------------------------------#

#probability to find a man driving a car
#given he has high school education


cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = (E == "high"), n = 10^6)

particles <- rbn(bn, 10^6)
head(particles, n = 5)

partE <- particles[(particles[, "E"] == "high"), ]
nE <- nrow(partE)

partEq <-partE[(partE[, "S"] == "M") & (partE[, "T"] == "car"), ]
nEq <- nrow(partEq)
nEq/nE

###### Mutilated Networks and likelihood sampling ####

mutbn <- mutilated(bn, list(E = "high"))
mutbn$E
particles <- rbn(bn, 10^6)
partQ <- particles[(particles[, "S"] == "M") &
                        (particles[, "T"] == "car"), ]
nQ <- nrow(partQ)
nQ/10^6

w <- logLik(bn, particles, nodes = "E", by.sample = TRUE)
wEq <- sum(exp(w[(particles[, "S"] == "M") &
                  (particles[, "T"] == "car")]))
wE <- sum(exp(w))
wEq/wE

# or alternatively:

cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = list(E = "high"), method = "lw")


###### Causal BNs #####


data(marks)
head(marks)
latent <- factor(c(rep("A", 44), "B",
                   rep("A", 7), rep("B", 36)))
modelstring(hc(marks[latent == "A", ]))
modelstring(hc(marks[latent == "B", ]))
modelstring(hc(marks))

#discretizing the BN to make it multinomial 
dmarks <- discretize(marks, breaks = 2, method = "interval")

modelstring(hc(cbind(dmarks, LAT = latent)))


# example for imputation:

# missing data imputation.
with.missing.data = gaussian.test
with.missing.data[sample(nrow(with.missing.data), 500), "F"] = NA
fitted = bn.fit(model2network("[A][B][E][G][C|A:B][D|B][F|A:D:E:G]"),
                gaussian.test)
imputed = impute(fitted, with.missing.data)
# predicting a variable in the test set.
training = bn.fit(model2network("[A][B][E][G][C|A:B][D|B][F|A:D:E:G]"),
                  gaussian.test[1:2000, ])
test = gaussian.test[2001:nrow(gaussian.test), ]
predicted = predict(training, node = "F", data = test)
# obtain the conditional probabilities for the values of a single variable
# given a subset of the rest, they are computed to determine the predicted
# values.
fitted = bn.fit(model2network("[A][C][F][B|A][D|A:C][E|B:F]"), learning.test)
evidence = data.frame(A = factor("a", levels = levels(learning.test$A)),
                      F = factor("b", levels = levels(learning.test$F)))
predicted = predict(fitted, "C", evidence,
                    method = "bayes-lw", prob = TRUE)
attr(predicted, "prob")











