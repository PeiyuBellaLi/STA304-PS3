#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Nationscape
# Author: Peiyu Li
# Data: 30 October 2020
# Contact: bella.li@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("~/Desktop/PS3")
raw_data_census <- read_dta("inputs/usa_00003.dta.gz")


# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_census <- 
  raw_data_census %>% 
  select(sex, 
         age, 
         race, 
         educ,
         citizen)
         
#create a new variable 'race_new' that maps the format in the survey data set.
reduced_data_census <- 
  reduced_data_census%>%
  mutate(race_new=ifelse(race=='white', 'white',
                                           ifelse(race=='black/african american/negro', 'black',
                                                    ifelse(race=="american indian or alaska native","american indian or alaska native",
                                                           ifelse(race=='chinese','chinese',
                                                                  ifelse(race=='japanese','japanese',
                                                                         ifelse(race=='other asian or pacific islander','other asian or pacific islander',
                                                                                ifelse(race=='other race, nec','other race',
                                                                                       ifelse(race=='two major races','other race',
                                                                                              ifelse(race==
                                                                                                       'three or more major races', 'other race', NA))))))))))
#modify the race_new type
reduced_data_census$race_new <- as.factor(reduced_data_census$race_new)

#create a new variable 'edu' that maps the format in the  survey data set.
reduced_data_census <- 
  reduced_data_census%>%
  mutate(edu=ifelse(educ=="n/a or no schooling",'8th grade or less',
                    ifelse(educ=="nursery school to grade 4",'8th grade or less',
                           ifelse(educ=="grade 5, 6, 7, or 8",'8th grade or less',
                                  ifelse(educ=="grade 9",'high school',
                                         ifelse(educ=='grade 10','high school',
                                                ifelse(educ=='grade 11', 'high school',
                                                       ifelse(educ=='grade 12','high school',
                                                              ifelse(educ=='1 year of college','some college',
                                                                     ifelse(educ=='2 years of college','some college',
                                                                            ifelse(educ=="3 years of college",'some college',
                                                                                   ifelse(educ=="4 years of college",'some college',
                                                                                          ifelse(educ=="5+ years of college","5+ years of college",NA)))))))))))))

#filter observations who are eligible to vote.
reduced_data_census$age <- as.numeric(reduced_data_census$age)
reduced_data_census <-
  reduced_data_census %>%
  filter(age >= 18 &
           (citizen=='naturalized citizen' | citizen=='born abroad of american parents'))

reduced_data_census <- 
  reduced_data_census %>%
  count(sex,edu,race_new) %>%
  group_by(sex,edu,race_new) 



census_data <- reduced_data_census %>%
  select(sex,race_new,edu,n)

# Saving the census data as a csv file in my
# working directory
write_csv(census_data, "outputs/census_data.csv")



         
