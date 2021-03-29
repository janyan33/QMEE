library(rjags)
library(R2jags)
library(coda)
library(emdbook)
library(lattice)

## Generate fake data
set.seed(411)
N <- 40

## Predictor variables
a <- runif(N) # runif = random uniform distribution
b <- runif(N)
c <- runif(N)
y <- rnorm(N, mean = 2 + 1*a+4*b+1*c, sd = 1)

dat <- data.frame(y, a, b, c)

## Frequentist model
freq_model <- lm(y ~ a+b+c, data = dat)
summary(freq_model)

## Bayes model? 

model { 
  for (i in 1:N){
    y[i] ~ norm(pred[i], tau)
    pred[i] <- ma*a[i] + mb*b[i] + mc*c[i] + int
  }
  ## Include priors for all parameters (these are all very "vague/"flat"/"weak"/"uninformative")
  ## Exclude silly/impossible answers in our priors
  ma ~ dnorm(0, .0001) # normal distribution with a mean of 0 and very little precision (high variance)
  mb ~ dnorm(0, .0001)
  mc ~ dnorm(0, .0001)
  int ~ dnorm(0, .0001)
  tau ~ dgamma(.001, .001) # standard to use gamma dist for prior of precision; precision can't be negative
}

j <- jags(data = list(y = y, a = a, b = b, c = c, N = nrow(dat)), 
          inits = NULL,
          parameters = c("ma", "mb", "mc", "tau"),
          model.file = "bayes.bug")

plot(j)

jj <- as.mcmc.bugs(j$BUGSoutput)
plot(jj)
xyplot(jj, layout = c(2,3))
densityplot(jj)
