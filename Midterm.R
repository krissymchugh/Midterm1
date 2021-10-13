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
Year12 <- Year3 %>%
  group_by(COUNTY) %>%
  filter(YEAR == 12) %>%
  select(COUNTY, POPESTIMATE) %>%
  mutate(final = POPESTIMATE)
View(Year12)
##
## Created a column for the population estimates of each county in the year 2019.
##
PercentChange <- merge(Year3, Year12, by="COUNTY") %>%
  mutate(pct_change = (final-initial)/(initial)*100)
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

PercentChange %>%
  group_by(COUNTY) %>%
  arrange(pct_change)
## Here, we can see that the top 3 counties with the highest increase in the percentage of total 
## population are, respectively, counties 41 (19.5%), 49 (12.9%), and 159 (12.4%). The 3 counties with   
## the lowest increase in the percentage of total population are, respectively, counties 19 (-6.70%),
## 111 (-6.54%), and 81 (-6.24%).
## 
##
## Calculate the 3 counties with the highest increase in the percentage of population over age 65 and 
## the 3 with the lowest rate of increase in age over 65.
AGE.65.UP.INITIAL <- OhioPopASC %>%
  group_by(COUNTY) %>%
  filter(YEAR == 1) %>%
  select(COUNTY, AGE65PLUS_MALE, AGE65PLUS_FEM) %>%
  mutate(Age65upinitial = AGE65PLUS_MALE + AGE65PLUS_FEM)
View(AGE.65.UP.INITIAL)
## 
## Calculated the population for both the female and male populations in 2008.
##
AGE.65.UP.FINAL <- OhioPopASC %>%
  group_by(COUNTY) %>%
  filter(YEAR == 12) %>%
  select(COUNTY, AGE65PLUS_MALE, AGE65PLUS_FEM) %>%
  mutate(Age65upfinal = AGE65PLUS_MALE + AGE65PLUS_FEM)
View(AGE.65.UP.FINAL)
## 
## Calculated the population for both the female and male populations in 2008.
##
PercentChange65 <- merge(AGE.65.UP.INITIAL, AGE.65.UP.FINAL, by="COUNTY") %>%
  select(COUNTY, Age65upfinal, Age65upinitial) %>%
  mutate(pct_change_65 = (Age65upfinal-Age65upinitial)/(Age65upinitial)*100)
View(PercentChange65)
##
## Merged the initial 65+ population and final population 65+ data sets.
##
PercentChange65 %>%
  group_by(COUNTY) %>%
  arrange(desc(pct_change_65))

PercentChange65 %>%
  group_by(COUNTY) %>%
  arrange(pct_change_65)
## Here, we can see that the top 3 counties with the highest increase in the percentage of population
## over 65 are, respectively, counties 41 (79.1%), 165 (52.4%), and 159 (51.3%). We can also see that
## the 3 counties with the lowest rate of increase are, respectively, counties 81 (13.0%), 33 (13.8%),
## and 67 (13.9%).
##
##
## Calculate the 3 counties with the highest increase in the percentage of population under age 20 and 
## the 3 with the lowest rate of increase.
Age.20.Below.initial <- OhioPopASC %>%
  group_by(COUNTY) %>%
  filter(YEAR == 1) %>%
  select(COUNTY, AGE1519_TOT, AGE1014_TOT, AGE59_TOT, AGE04_TOT) %>%
  mutate(Under20initial = AGE1519_TOT + AGE1014_TOT + AGE04_TOT)
View(Age.20.Below.initial)
##
## Created a column for the population under 20 for each county in the year 2008.
##
Age.20.Below.final <- OhioPopASC %>%
  group_by(COUNTY) %>%
  filter(YEAR == 12) %>%
  select(COUNTY, AGE1519_TOT, AGE1014_TOT, AGE59_TOT, AGE04_TOT) %>%
  mutate(Under20final = AGE1519_TOT + AGE1014_TOT + AGE04_TOT)
View(Age.20.Below.final)
##
## Created a column for the population under 20 for each county in the year 2019.
##
Age.20.Below <- merge(Age.20.Below.initial, Age.20.Below.final, by="COUNTY") %>%
  select(COUNTY, Under20initial, Under20final) %>%
  mutate(pct_change_20 = (Under20final - Under20initial)/(Under20initial)*100)
View(Age.20.Below)
##
## Merged the two data sets and created a variable for the percent change of the populations under 20
## for each county between 2008 and 2019.
##
Age.20.Below %>%
  group_by(COUNTY) %>%
  arrange(desc(pct_change_20))

Age.20.Below %>%
  group_by(COUNTY) %>%
  arrange(pct_change_20)
## Here, we can see that the top 3 counties with the highest increase in the percentage of population
## under 20 are, respectively, counties 41 (14.6%), 49 (7.68%), and 159 (3.87%). The 3 counties with 
## the smallest increase in the percentage of population under 20 are, respectively, counties 111 (-16.0%),
## 115 (-15.8%), and 163 (-15.3%).
## 



