## Importing sociability index raw data
soc_raw <- read.csv("data/soc_raw.csv")
summary(soc_raw)
str(soc_raw)

## Computing sociability scores for each scan
soc <- soc_raw %>%
  rowwise() %>%
  mutate(mean = mean(c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)), 
         var = var(c(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)), 
         soc.index = var/mean)

