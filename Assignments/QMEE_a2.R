## setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

library(tidyverse)
library(asnipe)
library(igraph)
library(plot.matrix)
library(janitor)

## Import group membership SNA data
groups <- read.csv("data/bbsna_rep1_groups.csv")
head(groups)
str(groups)

## Creating a list of groups where each individual is separated
group.list <- strsplit(groups$Group.Members, " ")

## Creating a group x individual (k x n) matrix
gbi_matrix <- get_group_by_individual(group.list, data_format = "groups")
view(gbi_matrix)
view(groups$Group.Members)

## Converting k x n matrix into a network (n x n matrix) and an igraph object
ibi_matrix <- get_network(gbi_matrix, data_format = "GBI")
head(ibi_matrix)
str(ibi_matrix) # I think I need the matrix to be in alphabetical order to set node attributes later??

## Trying to re-arrange matrix into alphabetical order
ibi_test <- ibi_matrix[order(rownames(ibi_matrix)) , order(colnames(ibi_matrix))]
head(ibi_test) # Looks like it worked

## Comparing ibi_matrix and ibi_test to make sure I didn't jumble up the values
plot(ibi_matrix, col = topo.colors)
plot(ibi_test, col = topo.colors) # Looks ok to me

## Creating igraph object
prox.igraph <- graph_from_adjacency_matrix(ibi_test, diag = FALSE, weighted = TRUE, mode = "undirected")

## Importing and cleaning node attribute data
attr <- read.csv("data/attributes.csv", na.strings = c("","NA"))
head(attr) #extra column
tail(attr) #extra row
attr <- attr %>% 
        remove_empty(which = c("rows", "cols"))

## Assigning node attributes
prox.igraph <- set_vertex_attr(prox.igraph, "sex", value = attr$sex)
prox.igraph <- set_vertex_attr(prox.igraph, "thorax", value = attr$thorax.mm)
prox.igraph <- set_vertex_attr(prox.igraph, "matings", value = attr$matings)

## Plotting network
V(prox.igraph)$color <- ifelse(V(prox.igraph)$sex == "Female", "red", "blue")
V(prox.igraph)$label.color <- "white"
plot(prox.igraph, edge.curved = 0, edge.color = "black", weighted = TRUE, layout = layout_nicely(prox.igraph)) 
## Can't see differences in edge weights

## Making edge weight differences more distinct
E(prox.igraph)$width <- E(prox.igraph)$weight*15 # Is it ok to just multiply them like this?? 
plot(prox.igraph, edge.curved = 0, edge.color = "black", weighted = TRUE, layout = layout_nicely(prox.igraph))

