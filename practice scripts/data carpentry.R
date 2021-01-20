setwd("C:/Users/jy33/OneDrive/Desktop/R/QMEE")

## Removing missing data
heights <- c(2, 4, 4, NA, 6)
mean(heights)
mean(heights, na.rm = TRUE)
heights2 <- na.omit(heights)

## Downloading data file from the interent
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")

## Load the data into R
surveys <- read.csv("data/portal_data_joined.csv")

surveys["species_id"] # dataframe
surveys[,"species_id"] # vector
surveys[["species_id"]] # vector
surveys$species_id #vector

(surveys_22 <- surveys[200,])
nrow(surveys)
(surveys_last <- surveys[34786,])
tail(surveys)

34786/2
(surveys_middle <- surveys[17393,])


