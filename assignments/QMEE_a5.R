## setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

library(asnipe)
library(igraph)
library(assortnet)
sessionInfo()
library(tidyverse)

## Loading in association data
groups <- read.csv("data/bbsna_aggregations.csv")
attr <- read.csv("data/bbsna_attributes.csv", stringsAsFactors = FALSE)
attr_prelim <- attr %>% 
  filter(replicate == "prelim")

<<<<<<< HEAD
attr_1 <- attr %>% 
  filter(replicate == 1) %>% 
  filter(ID != "Q") #Could probably avoid splitting this but will fix this in the future
=======
attr_2 <- attr %>% 
  filter(replicate == 2) %>% 
  filter(ID != "Q") #Could probably avoid splitting this but will fix this in the future ## BMB: OK
>>>>>>> 1ad287a39aa938b5828a3fb98b4408a8e2e42ef4

## Creating a generic function that creates an association matrix for each rep
func_make_matrix <- function(matrix) {
  group_list <- strsplit(matrix$Members, " ")
  ## Creating a group x individual (k x n) matrix
  gbi_matrix <- get_group_by_individual(group_list, data_format = "groups")
  ## Converting k x n matrix into a network (n x n matrix) and an igraph object
  ibi_matrix <- get_network(gbi_matrix, data_format = "GBI")
  ## Re-arranging matrix into alphabetical order
  ibi_matrix <- ibi_matrix[order(rownames(ibi_matrix)) ,
                           order(colnames(ibi_matrix))]
  return(ibi_matrix)
}

## Using my function to create an association matrix for each rep
prox_mat_prelim <- groups %>% 
  filter(Replicate == "prelim") %>% 
  func_make_matrix()

<<<<<<< HEAD
prox_mat_1 <- groups %>% 
  filter(Replicate == 1) %>% 
  func_make_matrix()
=======
stopifnot(nrow(prox_mat_1) == nrow(attr_1))

prox_mat_2 <- groups %>% 
  filter(Replicate == 2) %>% 
    func_make_matrix()

stopifnot(nrow(prox_mat_2) == nrow(attr_2))

>>>>>>> 1ad287a39aa938b5828a3fb98b4408a8e2e42ef4

## HYPOTHESIS 1: Are bedbug populations assorted by sex?
## BMB: "how are"? Can you make a more specific prediction?
## (assortative vs disassortative?)

## Creating a function that does the permutation test for assortativity index 
func_permute_assoc <- function(matrix, attributes, title){
  ## Calculating the observed assortativity index
  observed_index <- assortment.discrete(matrix, attributes$sex, weighted = TRUE)$r
  ## Setting up the for loop
  set.seed(33)
  nsim <- 999
  results <- numeric(nsim)
  ## Doing the permutations using a for loop
  for (i in 1:nsim) {
    ## Scramble order of nodes and put new ID order into old matrix
    matrix <- prox_mat_1
    names <- colnames(matrix)
    new_names <- sample(names)
    colnames(matrix) <- new_names
    rownames(matrix) <- new_names
    
    ## Re-order the sex attribute vector based on new ID order to be inputted into the assortment.discrete
    order_table <- as.data.frame(cbind(IDs = attributes$ID, 1:length(attributes$ID), sex = attributes$sex))
    order_table <- order_table %>% 
      slice(match(new_names, IDs))
    new.sexes <- order_table$sex
    
    ## Calculate new assortativity index
    assort_index <- assortment.discrete(matrix, new.sexes, weighted = TRUE)$r
    results[i] <- assort_index
  }
  
  ## Add observed value to the list of results and create histogram
  results <- c(results, observed_index)
  hist(results, main = title, xlab = "Assortativity index")
  abline(v = observed_index, col = "red")
  
  ## Calculating p-value for a two-tailed test 
  if (observed_index >= mean(results)) {
    p <- 2*mean(results >= observed_index) } else {
    p <- 2*mean(results <= observed_index)
  }
  return(p)
}

## HYPOTHESIS 2: Male strength vs. female strength 

## Function that runs permutations on strength
func_permute_strength <- function(matrix, attributes, title){
  
  prox_igraph <- graph_from_adjacency_matrix(matrix, diag = FALSE, weighted = TRUE, mode = "undirected")
  observed_strength <- as.data.frame(cbind(strength = strength(prox_igraph, v = V(prox_igraph), mode = c("all"), loops = FALSE), 
                                           sex = attributes$sex))
  
  strength_model <- lm(data = observed_strength, strength ~ sex)
  obs_coef <- coef(strength_model)[2]
  
  ## Simulating 1000 random networks based by shuffling the nodes of prox network #1 a thousand times
  set.seed(33)
  nsim <- 999
  strength_results <- numeric(nsim)
  
  for (i in 1:nsim) {
    ## Scramble order of nodes and create matrix with new order
    names <- colnames(matrix)
    new_names <- sample(names)
    colnames(matrix) <- new_names
    rownames(matrix) <- new_names
    
    ## Re-order the sex attribute vector based on new ID order to be inputted into the assortment.discrete
    order_table <- as.data.frame(cbind(IDs = attributes$ID, 1:length(attributes$ID), sex = attributes$sex))
    order_table <- order_table %>% 
      slice(match(new_names, IDs))
    new.sexes <- order_table$sex
    
    ## Calculate new strength
    prox_igraph <- graph_from_adjacency_matrix(matrix, diag = FALSE, weighted = TRUE, mode = "undirected")
    sim_strength <- as.data.frame(cbind(strength = strength(prox_igraph, v = V(prox_igraph), mode = c("all"), loops = FALSE), 
                                        sex = new.sexes))
    strength_model <- lm(data = sim_strength, strength ~ sex)
    sim_coef <- coef(strength_model)[2]
    
    strength_results[i] <- (sim_coef)
  }
  
  ## Add observed value to the list of results
  strength_results <- c(obs_coef, strength_results)
  
  hist(strength_results, main = title, xlab = "Coefficient value for sexMale")
  abline(v = obs_coef, col = "red")
  
  ## Compute p-value
  if (obs_coef >= mean(strength_results)) {
     p <- 2*mean(strength_results >= obs_coef) } else {
     p <- 2*mean(strength_results <= obs_coef)
     }
  return(p)
}

## Using my first function to do the assortativity permutations on each of my two replicates
func_permute_assoc(matrix = prox_mat_prelim, attributes = attr_prelim, title = "Assortativity prelim exp")
func_permute_assoc(matrix = prox_mat_1, attributes = attr_1, title = "Assortativity replicate 1")

## Using my second function to do the strength permutations on each of my two replicates
func_permute_strength(matrix = prox_mat_prelim, attributes = attr_prelim, title = "Male vs. female strength prelim exp")     
func_permute_strength(matrix = prox_mat_1, attributes = attr_1, title = "Male vs. female strength rep 1")
# positive values indcate average male strength > average female strength

## Comparing my permutation strength results with a classical test
func_lm_strength <- function(matrix, attributes){
  prox_igraph <- graph_from_adjacency_matrix(matrix, diag = FALSE, weighted = TRUE, mode = "undirected")
  strengths <- strength(prox_igraph, v = V(prox_igraph), mode = c("all"), loops = FALSE)
  attr <- cbind(attributes, strength = strengths)
  strength_model <- lm(data = attr, strength ~ sex)
  return(summary(strength_model))
}

func_lm_strength(matrix = prox_mat_prelim, attributes = attr_prelim)
func_lm_strength(matrix = prox_mat_1, attributes = attr_1)
