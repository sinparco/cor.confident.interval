#Load Library
library("devtools")
library(psych)

devtools::install_github("dstanley4/learnSampling", force=TRUE)

#Calculating confidence intervals with "r" and "n"
r.con(r=.21, n=100)
## Confience interals are read as [lower and upper brackets]

#Loading Popuation Data
set.seed(8)
library(learnSampling)
lower.pop <- get_cor_data(.01, 100000)
upper.pop <- get_cor_data(.39, 100000)
head(lower.pop)
head(upper.pop)

#Confirming population correlation
rho.lower <- cor(lower.pop$x, lower.pop$y)
rho.upper <- cor(upper.pop$x, upper.pop$y)
print(rho.lower)
print(rho.upper)

#### Lower Bound Population ####
ci.lower.bound.correlations <- get_cor_samples(pop.data=lower.pop,n=100,
                                               number.of.samples = 10000,
                                               number.of.decimals = 2)

##Sorting the Data
ci.lower.bound.correlations.sorted <- sort_samples_by_r(ci.lower.bound.correlations)

lower.cilowerbound <- ci.lower.bound.correlations.sorted$r[251]
upper.cilowerbound <- ci.lower.bound.correlations.sorted$r[9750]
print(lower.cilowerbound)
print(upper.cilowerbound)

library(ggplot2)
lower.bound.hist <- qplot(r, data=ci.lower.bound.correlations, binwidth =.05)
print(lower.bound.hist )

#### Upper Bound Distribution ####
ci.upper.bound.correlations <- get_cor_samples(pop.data=upper.pop,n=100,
                                               number.of.samples = 10000,
                                               number.of.decimals = 2)

##Sorting the Data
ci.upper.bound.correlations.sorted <- sort_samples_by_r(ci.upper.bound.correlations)

lower.ciupperbound <- ci.upper.bound.correlations.sorted$r[251]
upper.ciupperbound <- ci.upper.bound.correlations.sorted$r[9750]
print(lower.ciupperbound)
print(upper.ciupperbound)

upper.bound.hist <- qplot(r, data=ci.upper.bound.correlations, binwidth =.05)
print(upper.bound.hist )

#### Graphing both Distributions ####
both.hist <- ggplot(ci.upper.bound.correlations, aes(r))
both.hist <- both.hist + geom_histogram(aes(x= ci.upper.bound.correlations$r, y=..count..),
                                        fill = "red", binwidth = .05)
both.hist <- both.hist + geom_histogram(aes(x= ci.lower.bound.correlations$r, y=..count..),
                                        fill = "blue", binwidth = .05)
print(both.hist)

##Can try for r=.30, n=50 or 500 or 1000

#### Correlation Power Analysis ####
library(pwr)
pwr.r.test(r=.35, power=.80, alternative="two.sided")

##if we want to take into account effect size (.35) estimate, then use the lower bound of CIinstead
psych:: r.con(r=.35, n=100)
pwr.r.test(r=.1649, power=.8, alternative="two.sided")

#Why power = .80? Because we want to have 80% chance of finding a significant relation if there is one.
#Trying on Undergrad Thesis
pwr.r.test(r=.215, power= .95, alternative="two.sided")

#Replication
library(predictionInterval)
pi.r(r=.41,n=70,rep.n=214)
