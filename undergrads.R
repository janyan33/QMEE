## setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

library(asnipe) 
library(igraph)
library(assortnet)
library(tidyverse)

## Loading in association data
groups <- read.csv("data/bbsna_aggregations.csv")
attr <- read.csv("data/bbsna_attributes.csv", stringsAsFactors = FALSE)

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
prox_mat_1 <- groups %>% 
  filter(Replicate == 1) %>% 
  func_make_matrix()

prox_mat_2 <- groups %>% 
  filter(Replicate == 2) %>% 
  func_make_matrix()

## HYPOTHESIS 1: Are bedbug populations assorted by sex?

## Creating a function that does the permutation test for assortativity index 
func_permute_assoc <- function(matrix, attributes){
  ## Calculating the observed assortativity index
  observed_index <- assortment.discrete(matrix, attributes$sex, weighted = TRUE)$r
  ## Setting up the for loop
  set.seed(33)
  nsim <- 999
  results <- numeric(nsim)
  ## Doing the permutations using a for loop
  for (i in 1:nsim) {
    ## Scramble order of nodes and put new ID order into old matrix
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
  
  ## Add observed value to the list of results
  results <- c(results, observed_index)
  
  ## Calculating p-value for a two-tailed test 
  if (observed_index >= mean(results)) {
    p <- 2*mean(results >= observed_index) } else {
      p <- 2*mean(results <= observed_index)
    }
  list <- list("random assortativity scores" = results, "observed assortativity score" = observed_index, "p-value" = p)
  return(list)
}

## Loading and cleaning attribute data
attr_1 <- attr %>% 
          filter(replicate == 1) %>% 
          filter(notes != "died")

attr_2 <- attr %>% 
          filter(replicate == 2) %>% 
          filter(notes != "died")

#################################### START CODING HERE ##########################################

## Try using the func_permute_assoc function to run the permutations and output assortativity scores and a p-value
results_1 <- func_permute_assoc(matrix = prox_mat_2, attributes = attr_2)


## OPTIONAL: At this point you can output the results to a csv file and create your histogram in Excel using write.csv
#write.csv(results_1, "permutation_results_1.csv")


## Create histogram using values in results_1 (try to make it look a little different than the default)
## I suggest changing the x-axis limits (would be nice if all the histograms had the same ones)



## Put a line that shows the observed assortativity score from the real network (make sure it stands out)


