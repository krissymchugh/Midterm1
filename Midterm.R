## Midterm 1 - Take Home
## Author: Krissy McHugh
##
##
## Read in the Ohio Population ASC Data
library(tidyverse)
OhioPopASC <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv")
view(OhioPopASC)
summary(OhioPopASC)
str(OhioPopASC)
##
## Calculate the percentage change in the population of each Ohio county from the year 2010 to 2019
## YEARS == 3 and 12 in the data, respectively) and output a data frame with this information
PercentChange <-OhioPopASC %>%
  group_by(YEAR) %>%
  filter(YEAR !=1) %>%
  filter(YEAR !=2) %>%
  select(YEAR, POPESTIMATE, COUNTY) %>%
  arrange(desc(YEAR), .by_group = TRUE) %>%
  mutate(pct_change = ((lead(POPESTIMATE))-(lag(POPESTIMATE))/(lead(POPESTIMATE))*100))
view(PercentChange)
##
## Calculate the 3 counties with the highest increase in the percentage of total population and the 
## 3 with the lowest percentage increase
PercentChange %>%
  arrange(desc(pct_change))







