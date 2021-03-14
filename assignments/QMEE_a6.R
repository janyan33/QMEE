## setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

library(tidyverse); theme_set(theme_classic())
library(asnipe)
library(igraph)
library(assortnet)
library(lme4)
library(emmeans)
library(dotwhisker)
library(car)

## Loading in association data
attr <- read.csv("data/bbsna_attributes.csv", stringsAsFactors = TRUE) %>% 
        filter(notes != "died") %>% 
        filter(replicate != "prelim")

## Linear models 
lm_sex <- lm(prox_strength ~ sex, data = attr)
lm_treatment <- lm(prox_strength ~ treatment, data = attr)
lm_both <- lm(data = attr, prox_strength ~ sex + treatment)
lm_int <- lm(data = attr, prox_strength ~ sex*treatment)
summary(lm_sex)
summary(lm_treatment)
summary(lm_both) # We expect an effect of treatment and sex so this model probably makes more sense than lm_sex
summary(lm_int)  # We also have predictions about the treatment (high vs. low sexual conflict) affecting the diff in strength betweeb males and females so I think it makes sense to include the interaction term
   # Interaction not significant but I should keep it cause I wanted to test it
plot(lm_int) 

## BMB: this is all good. You should look at the diagnostics **before** you
## look at the summaries (= p-values); that is, you should decide whether you
## think the model needs to be modified before you have a chance to decide
## that you do (or don't) like the results

## PLOTS DISCUSSION
## Residuals vs. fitted: 
   # The red line looks nearly flat so it looks like there's no bias or non-linear relationships that are not accounted for by the model

## Scale-location (heteroscedasticity):
   # The red line fluctuates a bit with fitted values which I believe indicates there's some heteroscedasticity (change in variance across the dataset) but I don't think the pattern is strong enough to be any cause for concern

## Residuals vs. leverage:
   # Since I can't even see the Cook's distance contour lines, there seems to be no particularly influential data point meaning I shouldn't treat any points as outliers

## Q-Q plot
   # The values mostly fall on the diagonal meaning the residuals are normally distributed so this is not cause for concern

## Plotting coefficients 
dwplot(lm_int)

## Comparing models
anova(lm_both, lm_sex, test = "F")
anova(lm_both, lm_treatment, test = "F")
drop1(lm_both, test = "F") # same results but less code
Anova(lm_both) # Identical as line above which was expected

Anova(lm_int)  # Different than summary(lm_int) 

## Pairwise comparisions
e1 <- emmeans(lm_int, c("sex", "treatment"))
summary(e1)
pairs(e1)
plot(e1) # Looking at the picture

## Visualizing strength means using a boxplot
strength_boxplot <- ggplot(data = attr, aes(y = prox_strength, x = treatment, fill = sex)) + geom_boxplot(notch=TRUE) 
strength_boxplot
## BMB: one issue is that the boxplot is **not** inferential, it's descriptive
## adding notches (as shown) gives approx CIs on the medians, but these are wonky
## here because the data set is too small (i.e. the width of the CI goes beyond
## the hinges (approx IQR)

## grade: 2.1/3
