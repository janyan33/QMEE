## BMB: don't leave this uncommented!
## setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

## Loading packages
library(tidyverse)
# BMB: tidyverse includes ggplot2 already
# (see the message that prints when it loads)
# library(ggplot2)

## Loading and inspecting association matrix data
# BMB: "data/prox_network.csv" is definitely not reproducible
# because your directory is Data, not data
#
prox.mat <- read.csv("Data/prox_network.csv")
head(prox.mat)
tail(prox.mat)

## Loading and inspecting node attribute data
attr <- read.csv("Data/attributes.csv", stringsAsFactors = TRUE)
head(attr)
tail(attr)
# BMB: there is probably a better way to clean this, e.g.
attr <- (read.csv("Data/attributes.csv", stringsAsFactors=TRUE)
    %>% janitor::remove_empty(which=c("rows","cols"))
    ## doesn't quite work, we need a way to remove rows with
    ## blank OR NA
)

# BMB: avoid row/col reference by number
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

## BMB: looks good except for minor non-reproducibility issues. score: 1.9
