## setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

library(tidyverse); theme_set(theme_classic())
library(emmeans)
library(dotwhisker)
library(car)

## Loading in association data
attr <- read.csv("data/bbsna_attributes.csv", stringsAsFactors = TRUE) %>% 
  filter(notes != "died") %>% 
  filter(replicate != "prelim")

## Linear model from last week 
lm_model <- lm(data = attr, prox_strength ~ sex*treatment)
summary(lm_model)
plot(lm_model)

## Fitting a generalized linear model
# My response variable (strength) is a positive continous variable so I decided on using a Gamma distribution
gamma_model <- glm(data = attr, prox_strength ~ sex*treatment, 
             family = Gamma(link = "log")) # Need to specify log bc default is "inverse"
summary(gamma_model) # Don't need to worry about overdispersion when using a Gamma distribution
plot(gamma_model)

## PLOTS DISCUSSION
## Residuals vs. fitted: 
# The red line looks flat so it looks like there's no bias or non-linear relationships that are not accounted for by the model

## Scale-location (heteroscedasticity):
# The red line fluctuates a bit with fitted values which indicates there's some heteroscedasticity (change in variance across the dataset) but I don't think the pattern is strong enough to be any cause for concern

## BMB: Hmm, I'm slightly worried about this one. Scale-location actually
## looked better for the LM.
m2 <- update(lm_model, y=TRUE, qr=TRUE)  ## required for boring technical reasons
MASS::boxcox(m2)
## suggests sqrt transform or nothing

## Residuals vs. leverage:
# Since I can't even see the Cook's distance contour lines, there seems to be no particularly influential data point meaning I shouldn't treat any points as outliers

## Q-Q plot
# The values mostly fall on the diagonal meaning the residuals are normally distributed so this is not cause for concern.
# The left end deviates a bit from the diagonal line meaning the model might be underpredicting a bit there but again I don't think it's cause for concern

## Plotting coefficients 
dwplot(gamma_model) # 95% confidence interval for treatmentlow doesn't overlap 0 meaning the effect (strength is lower in low vs. high treatments) is clear
## BMB: can you interpret the QUANTITATIVE meaning of the parameter?

## Pairwise comparisions
e1 <- emmeans(gamma_model, c("sex", "treatment"))
summary(e1)
pairs(e1)
plot(e1, comparisons=TRUE) # Looking at the picture
## BMB: add arros for 'overlappingness' (see the vignette)

## Visualizing strength means using a boxplot
ggplot(data = attr, aes(y = prox_strength, x = treatment, fill = sex)) + geom_boxplot() 

## grade: 2.1



