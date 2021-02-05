# QMEE 
# Assignment 1: Choosing and describing my dataset 
My dataset is comprised of an association matrix obtained from observing a population of 24 bedbugs over the course of 6 days along with a spreadsheet describing key attributes of each individual bug such as sex and thorax width (a proxy for size). Scans were conducted between 10am - 6pm at hourly intervals. The values in the matrix represent how many times a given pair of bedbugs were observed to be within one body length of each other. With this data, I hope to explore how the presence of intense sexual conflict and sexual harassment influence female social behaviour. Because engaging in social interactions through aggregation participation is expected to be more costly for female bedbugs who require a very low mating rate to maintain maximum fecundity compared to males who benefit from multiple matings, I predict females will be less social than males. I also predict that my bedbug population will be phenotypically assorted by sex since females preferentially associating with other females may serve as a social strategy for mitigating the costs of mating while maintaining the benefits of group living. However, since I do not know how to statistically test these predictions yet, this week I decided to conduct a t-test comparing male vs. female thorax width to see if females are in fact larger than males. 

# Assignment 2: Data management
## What I did 
This week I worked on figuring out how to use R to convert my raw data which contained a list of group memberships into an association matrix. The generated matrix appeared to arrange the individuals by order of appearance so I re-arranged it into alphabetical order. I created matrix plots to visually validate that my re-arranging did not jumble up association values between individuals. Using this new matrix, I could create an igraph object which allowed me to assign corresponding attributes (sex etc.) to each individual and create a social network graph where node colour represents sex. This allowed me to double check that the sex attribute was correctly assigned to each node.

## What investigations I plan to do
With this script, I am now able to easily convert group membership data from subsequent replicates into igraph objects or association matrices. With an igraph object, I believe I can use a variety of functions within the igraph package to calculate both network-level and individual-level characteristics. I am interested in whether the bedbug population formed distinct sub-communities so I will need to test for modularity. I would like to test for assortativity based on sex to see if individuals have a tendency to affiliate with same-sex conspecifics. Lastly, I would like to investigate whether one sex is more social than the other. To do this, I could calculate degree for each individual and do a t-test between the sexes. Alternatively, I think the standard convention is to do something that involves using my observed data to run several random simulations but I'll have to read into this more. 

# Assignment 3: Visualization
## Background
This week I added data from my second replicate and used the igraph package to calculate some node-level social network metrics. Using these values, I used ggplot to explore my hypotheses. My first prediction was that females will be less social than males. My second prediction is that females who are less social will recieve less sexual harassment. The first four chunks of code are just for calculating the network metrics I used but I kept them all in the same script since that's most convenient for my project. 

## Plot 1: Both replicates males vs. females boxplot
This boxplot 

## Plot 2: 

## Plot 3: 

## Plot 4: 
