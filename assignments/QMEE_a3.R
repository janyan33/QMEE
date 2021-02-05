## setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

library(tidyverse)
library(asnipe)
library(igraph)
library(plot.matrix)
library(janitor)
library(assortnet)

## Loading in association data
groups <- read.csv("data/bbsna_aggregations.csv")
head(groups)
str(groups)

rep1 <- groups %>%
        filter(Replicate == 1)

rep2 <- groups %>% 
        filter(Replicate == 2)

#######################################################################################################################################
## REPLICATE 1 AGGREGATION
## Creating a list of groups where each individual is separated
group_list_1 <- strsplit(rep1$Members, " ")

## Creating a group x individual (k x n) matrix
gbi_matrix_1 <- get_group_by_individual(group_list_1, data_format = "groups")

## Converting k x n matrix into a network (n x n matrix) and an igraph object
ibi_matrix_1 <- get_network(gbi_matrix_1, data_format = "GBI")

## Re-arranging matrix into alphabetical order
ibi_matrix_1 <- ibi_matrix_1[order(rownames(ibi_matrix_1)) , order(colnames(ibi_matrix_1))]

## Creating igraph object
prox_1 <- graph_from_adjacency_matrix(ibi_matrix_1, diag = FALSE, weighted = TRUE, mode = "undirected")

## Calculating in-strength (# of mountings received)
agg_strength_1 <- strength(prox_1, v = V(prox_1), mode = c("all"), loops = F)

#######################################################################################################################################
## REPLICATE 2 AGGREGATION
## Creating a list of groups where each individual is separated
group_list_2 <- strsplit(rep2$Members, " ")

## Creating a group x individual (k x n) matrix
gbi_matrix_2 <- get_group_by_individual(group_list_2, data_format = "groups")

## Converting k x n matrix into a network (n x n matrix) and an igraph object
ibi_matrix_2 <- get_network(gbi_matrix_2, data_format = "GBI")

## Re-arranging matrix into alphabetical order
ibi_matrix_2 <- ibi_matrix_2[order(rownames(ibi_matrix_2)) , order(colnames(ibi_matrix_2))]

## Creating igraph object
prox_2 <- graph_from_adjacency_matrix(ibi_matrix_2, diag = FALSE, weighted = TRUE, mode = "undirected")

## Calculating in-strength (# of mountings received)
agg_strength_2 <- strength(prox_2, v = V(prox_2), mode = c("all"), loops = F)

#####################################################################################################################################
## REPLICATE 1 MOUNTING
## Loading replicate 1 mounting matrix in
mounting_1 <- read.csv("data/bbsna_mounting_matrix_rep1.csv", header=T, row.names=1)
mounting_1[is.na(mounting_1)] = 0
mounting_1 <- as.matrix(mounting_1)

## Creating igraph object
mounting_1 <- graph_from_adjacency_matrix(mounting_1, weighted = TRUE, mode = "directed")

## Calculating in-strength (# of mountings received)
in_strength_1 <- strength(mounting_1, v = V(mounting_1), mode = c("in"), loops = F)

## Adding newly calculated values to the attribute lists
attr_1 <- read.csv("data/bbsna_attributes_raw.csv", na.strings = c("","NA")) %>% 
          remove_empty(which = c("rows", "cols")) %>% 
          filter(replicate == 1) %>% 
          mutate(agg_strength = agg_strength_1) %>% 
          mutate(mount_in_strength = in_strength_1)

##################################################################################################################################
## REPLICATE 2 MOUNTING
## Loading replicate 1 mounting matrix in
mounting_2 <- read.csv("data/bbsna_mounting_matrix_rep2.csv", header=T, row.names=1)
mounting_2[is.na(mounting_2)] = 0
mounting_2 <- as.matrix(mounting_2)
mounting_2 <- mounting_2[-(17), -(17)] #removing Q because she died

## Creating igraph object
mounting_2 <- graph_from_adjacency_matrix(mounting_2, weighted = TRUE, mode = "directed")

## Calculating in-strength (# of mountings received)
in_strength_2 <- strength(mounting_2, v = V(mounting_2), mode = c("in"), loops = F)

attr_2 <- read.csv("data/bbsna_attributes_raw.csv", na.strings = c("","NA")) %>% 
          remove_empty(which = c("rows", "cols")) %>% 
          filter(replicate == 2) %>% 
          filter(ID != "Q") %>% #removing Q because she died
          mutate(agg_strength = agg_strength_2) %>% 
          mutate(mount_in_strength = in_strength_2)

################################################################################################################################
## ASSIGNMENT 3 PLOTS
# Combining data from above networks and calculations
attr <- rbind(attr_1, attr_2)
attr$replicate <- as.factor(attr$replicate)

## Computing sociability scores for each scan
soc_raw <- read.csv("data/bbsna_soc_raw.csv", stringsAsFactors = TRUE)
soc <- soc_raw %>%
       rowwise() %>%
       mutate(mean = mean(c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)), 
              var = var(c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)), 
              soc.index = var/mean)

soc_sexes <- soc %>% 
             filter(sex == "male" | sex == "female")
soc_sexes$scan <- as.factor(soc_sexes$scan)

## Plotting sociability index 
ggplot(data = soc_sexes, aes(y = soc.index, x = scan, fill = sex)) + geom_boxplot() + 
       theme_classic() + scale_color_manual("red", "blue") + 
        xlab("Hour") + ylab("Sociability index") + theme(legend.title=element_blank())
        # + geom_jitter() too crowded looking

## Plotting strength from aggregation network for males vs. females
# Edges from this network represent an association index calculated based on how often two individuals were aggregating
# Association index was calculated using SRI (simple ratio index) which is recommended when observations are rarely missing
# "Aggregating" was defined as two bugs physically touching one another

ggplot(data = attr, aes(y = agg_strength, x = sex)) + geom_boxplot() + theme_classic() +
       xlab("") + ylab("Aggregation network strength") + geom_jitter(color = "grey")

## Plotting correlation between aggregation strength and mounting in-strength 
# The edges in the mounting network represent total # of mountings received
ggplot(data = attr, aes(x = agg_strength, y = mount_in_strength, color = sex)) + geom_point() + theme_classic() + 
       geom_smooth(method = lm, se = FALSE) + xlab("Aggregation network strength") + ylab("Mounting network in-strength") + 
       theme(legend.title=element_blank())

## Plotting correlation between aggregation strength and mounting in-strength while separating the two replicates
ggplot(data = attr, aes(x = agg_strength, y = mount_in_strength, color = sex)) + geom_point() + theme_light() + 
       geom_smooth(method = lm, se = FALSE) +  xlab("Aggregation network strength") + ylab("Mounting network in-strength") +
       theme(legend.title=element_blank()) + facet_grid(rows = vars(replicate), labeller = label_both)
