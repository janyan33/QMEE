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

## BMB: splitting your data is usually unnecessary; it probably
## means you're going to do the same analysis on each group,
## which can often be done better by looping over groups
## "don't repeat yourself" is a programming maxim
## we can work on this in class (or sometime)

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

## BMB: for example, the code below replaces the two sections above
##  (half as much code to look at, no chance of cut-and-paste errors etc.)
get_agg_strength <- function(repdata) {
    group_list <- strsplit(repdata$Members, " ")
    ## Creating a group x individual (k x n) matrix
    gbi_matrix <- get_group_by_individual(group_list, data_format = "groups")
    ## Converting k x n matrix into a network (n x n matrix) and an igraph object
    ibi_matrix <- get_network(gbi_matrix, data_format = "GBI")
    ## Re-arranging matrix into alphabetical order
    ibi_matrix <- ibi_matrix[order(rownames(ibi_matrix)) ,
                             order(colnames(ibi_matrix))]
    ## Creating igraph object
    prox <- graph_from_adjacency_matrix(ibi_matrix,
                                        diag = FALSE, weighted = TRUE, mode = "undirected")
    ## Calculating in-strength (# of mountings received)
    agg_strength <- strength(prox, v = V(prox), mode = c("all"), loops = FALSE)
    return(agg_strength)
}

rep_list <- split(groups, groups$Replicate)
agg_strength_list <- lapply(rep_list,get_agg_strength)

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

## BMB: would be good to avoid enumerating s1, ..., s12. Don't know
## if s1:s12 would work in this context ... 'tidiest' option would
## be to pivot_longer(), e.g.

(soc_raw
    %>% pivot_longer(names_to="scan_num",values_to="soc_score",
                     -c(replicate:sex,open,Total))
    %>% group_by(replicate,day,time,scan,sex,Total,open)
    %>% summarise(soc.index=var(soc_score)/mean(soc_score),
                  .groups="drop")
)

soc_sexes <- soc %>% 
    filter(sex == "male" | sex == "female")
## BMB or sex %in% c("male","female") or sex !="both"
soc_sexes$scan <- as.factor(soc_sexes$scan)

## Plotting sociability index
colvec <- c("#f0553a", "#4A75D2") ## define colours once ...
ggplot(data = soc_sexes, aes(y = soc.index, x = scan, fill = sex)) + geom_boxplot(outlier.colour=NULL) + 
       theme_classic() + scale_fill_manual(values=colvec) + 
        xlab("Hour") + ylab("Sociability index") + theme(legend.title=element_blank())
        # + geom_jitter() too crowded looking

## BMB: nice.  Consider less saturated colours? (Or set alpha<1)
## Would be nice to colour outlier points the same as the boxes, but that turns out to be a pain

## Plotting strength from aggregation network for males vs. females
# Edges from this network represent an association index calculated based on how often two individuals were aggregating
# Association index was calculated using SRI (simple ratio index) which is recommended when observations are rarely missing
# "Aggregating" was defined as two bugs physically touching one another

ggplot(data = attr, aes(y = agg_strength, x = sex)) + geom_boxplot() + theme_classic() +
    xlab("") + ylab("Aggregation network strength") + geom_jitter(color = "grey")

## BMB alternative:
gg0 <- (ggplot(data = attr, aes(y = agg_strength, x = sex))
      + geom_boxplot()
      ## BMB: if you like theme_classic(), set it at the beginning of your
      ##  session: theme_set(theme_classic())
      + theme_classic()
    + xlab("") + ylab("Aggregation network strength")
)

## BMB: I would spread the points less 
gg0  + geom_jitter(color = "grey",width=0.04)

## BMB: or beeswarm, which spreads the points only as much as necessary
## (and non-randomly)
library(ggbeeswarm)
gg0  + geom_beeswarm(color = "grey",cex=1.5)


## Plotting correlation between aggregation strength and mounting in-strength 
# The edges in the mounting network represent total # of mountings received
ggplot(data = attr, aes(x = agg_strength, y = mount_in_strength, color = sex)) + geom_point() + theme_classic() + 
       geom_smooth(method = lm, se = FALSE) + xlab("Aggregation network strength") + ylab("Mounting network in-strength") + 
       theme(legend.title=element_blank()) + scale_color_manual(values=c("#f0553a", "#4A75D2"))

## Plotting correlation between aggregation strength and mounting in-strength while separating the two replicates
ggplot(data = attr, aes(x = agg_strength, y = mount_in_strength, color = sex)) + geom_point() + theme_light() + 
       geom_smooth(method = lm, se = FALSE) +  xlab("Aggregation network strength") + ylab("Mounting network in-strength") +
       theme(legend.title=element_blank()) + scale_color_manual(values=c("#f0553a", "#4A75D2")) +
       facet_grid(rows = vars(replicate), labeller = label_both) 


theme_set(theme_classic())
gg1 <- (ggplot(data = attr, aes(x = agg_strength, y = mount_in_strength, color = sex))
    + geom_point()
    + labs(x="Aggregation network strength",
           y="Mounting network in-strength")
    + scale_color_manual(values=c("#f0553a", "#4A75D2"))
    ## + theme(legend.title=element_blank())
)

gg1 + aes(shape=replicate,linetype=replicate) +
    scale_shape_manual(values=c(1,16)) +   ## open vs closed symbols
    geom_smooth(method = lm, se = FALSE)

## BMB: do you know what differences caused the big change between
## 'replicates' (we usually intend replicates to be similar!)

## grade: 2.2/3

