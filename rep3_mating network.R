library(tidyverse)
library(asnipe)
library(igraph)
library(assortnet)
source("igraphhack/igraphplot2.R")
environment(plot.igraph2) <- asNamespace('igraph')
environment(igraph.Arrows2) <- asNamespace('igraph')


## Loading matrix in and turning it into an igraph object
rep3_mating_mat <- read.csv("data/bbsna_mating_matrix_rep3.csv", header = TRUE, row.names = 1)
rep3_mating_mat[is.na(rep3_mating_mat)] = 0
rep3_mating_mat <- as.matrix(rep3_mating_mat)

mating3_igraph <- graph_from_adjacency_matrix(rep3_mating_mat, weighted = TRUE, mode = "directed")

## Assigning node attributes
attr_3 <- read.csv("data/bbsna_attributes.csv") %>% 
          filter(replicate == 3)

mating3_igraph <- set_vertex_attr(mating3_igraph, "sex", value = attr_3$sex)

V(mating3_igraph)$color <- ifelse(V(mating3_igraph)$sex == "Female", "red", "blue")
V(mating3_igraph)$label.color <- "white"
E(mating3_igraph)$width <- E(mating3_igraph)$weight*1.5

## Calculate strength then graph again
strength_mating3 <- strength(mating3_igraph, v = V(mating3_igraph), mode = c("all"), loops = F)
mating3_igraph <- set_vertex_attr(mating3_igraph, "strength", value = strength_mating3)
V(mating3_igraph)$size <- V(mating3_igraph)$strength*2.5

plot(mating3_igraph, edge.curved = 0, layout = layout_nicely(mating3_igraph), edge.color = "black", 
     edge.arrow.size = 0.1, main = "rep 3 (2 shelter) mating network")





