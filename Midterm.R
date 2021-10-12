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
##
Year3 <- OhioPopASC %>%
  group_by(COUNTY) %>%
  filter(YEAR == 3) %>%
  select(COUNTY, POPESTIMATE) %>%
  mutate(initial = POPESTIMATE)
View(Year3)
##
## Created a column for the population estimates of each county in the year 2010.
##
Year12 <- OhioPopASC %>%
  group_by(COUNTY) %>%
  filter(YEAR == 12) %>%
  select(COUNTY, POPESTIMATE) %>%
  mutate(final = POPESTIMATE) 
View(Year12)
##
## Created a column for the population estimates of each county in the year 2019.
##
PercentChange <- rbind(Year3, Year12) %>%
  mutate(pct_change = (initial-final)/(final)*100)
View(PercentChange)
##
## Combined data sets for the initial and final values for calculating the percent change from the year
## 2010 to 2019.
##
##
## Calculate the 3 counties with the highest increase in the percentage of total population and the 
## 3 with the lowest percentage increase
PercentChange %>%
  group_by(COUNTY) %>%
  arrange(desc(pct_change))
## Here, we can see that the top 3 counties with the highest increase in the percentage of total 
## population are, respectively, county 47, 33, and 59.









