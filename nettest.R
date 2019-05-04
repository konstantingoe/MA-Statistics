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
#       Approximate Inference                        #
#----------------------------------------------------#

#using monte carlo simulations to randomly generate observations from the BN

# true probability:

gRain::querygrain(bn, event = (S == "M") & (T == "car"),
           evidence = (E == "high"))

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

graphviz.plot(dag)









