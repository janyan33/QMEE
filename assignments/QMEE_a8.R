library(dplyr)
library(R2jags)
library(rjags)
library(dotwhisker)
library(emdbook)
library(lattice)

## Utility for constructing an automatically named list
named_list <- lme4:::namedList

## Loading data in 
attr <- read.csv("data/bbsna_attributes.csv", stringsAsFactors = TRUE) %>% 
        filter(replicate != "prelim") %>% 
        filter(notes != "died")

## Creating named list
attr_dat <- with(attr,
                named_list(N = nrow(attr),            ## total obs
                           nsex = length(levels(sex)), ## number of categories
                           sex = as.numeric(sex),      ## numeric index
                           prox_strength))                   ## bedbug strength

## Bayes model (I'm honestly not sure if this is done correctly at all)
## I used a gamma-log link function b/c I think that's what I should do when my response variable (strength) is a positive continuous variable
## JD: It's _one_ good thing to do; lognormal is another

## JD: No need to have lots of different prec variables all set to tau
sex_model <- function() {
  for (i in 1:N) { 
    ## Gamma model?
    sex_eff[i] <- b_sex*(sex[i] - 1) # effect of sex; factor with 2 levels
    pred[i] <- exp(sex_eff[i] + int)
    prec[i] <- tau
    prox_strength[i] ~ dgamma(pred[i], prec[i])
  }
  ## priors
  b_sex ~ dnorm(0, .001) 
  int ~ dnorm(0, .001) # intercept I think 
  tau ~ dgamma(0.001, 0.001) # precision I think
}  

jags_1 <- jags(data = attr_dat,
           inits = NULL,
           parameters = c("b_sex", "tau", "int"),
           model.file = sex_model)

jags_1

# Rhat looks fine? close to 1
# n.eff seems too small so maybe my samples are highly auto-correlated and my model is bad..
# JD: In this case you probably need to just run a little longer

## Plotting stuff
bb <- jags_1$BUGSoutput
mm <- as.mcmc.bugs(bb)
plot(jags_1) ## large-format graph
xyplot(mm)  ## prettier trace plot
## trace plot looks ok I think? looks like a catepillar with no obvious patterns..
## JD: Trace plots on top look like you need to run a little longer

densityplot(mm) ## prettier density plot
print(dwplot(jags_1)) 

## Analogous frequentist fit 
freq_model <- lm(data = attr, prox_strength ~ sex)
summary(freq_model)
dwplot(freq_model)

## JD: OK, and what do you see when you compare?
## Grade: 1.9/3

