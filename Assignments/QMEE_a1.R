setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

## Loading packages
library(tidyverse)
library(ggplot2)

## Loading and inspecting association matrix data
prox.mat <- read.csv("data/prox_network.csv")
head(prox.mat)
tail(prox.mat)

## Loading and inspecting node attribute data
attr <- read.csv("data/attributes.csv", stringsAsFactors = TRUE)
head(attr)
tail(attr)
attr <- attr[-(25:28) , -7] #Removing mysterious extra column and rows
head(attr)
tail(attr)

## t-test for male vs. female thorax width
bartlett.test(thorax.mm ~ sex, data = attr) # homogeneity of variance
t.test(thorax.mm ~ sex, data = attr, var.equal = FALSE)

## Visualizing m vs. f thorax width to make sure things look right
ggplot(data = attr, aes(x = sex, y = thorax.mm, fill = sex)) + 
       geom_boxplot() + 
       labs(x = "", y = "Thorax width (mm)") + 
       theme_classic() + 
       scale_fill_manual(values = c("red", "blue"))

