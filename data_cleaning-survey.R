#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("~/Desktop/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("inputs/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
         gender,
         vote_intention,
         registration,
         race_ethnicity,
         education)



#create a new variable 'race_new' to combine some levels in race_ethnicity into some new levels
reduced_data <- 
  reduced_data %>%
  mutate(race_new=ifelse(race_ethnicity=='White','white',
                         ifelse(race_ethnicity=='Black, or African American','black',
                                ifelse(race_ethnicity=='American Indian or Alaska Native',"american indian or alaska native",
                                       ifelse(race_ethnicity=='Asian (Asian Indian)','other asian or pacific islander',
                                              ifelse(race_ethnicity=="Asian (Chinese)",'chinese',
                                                     ifelse(race_ethnicity=='Asian (Filipino)','other asian or pacific islander',
                                                            ifelse(race_ethnicity=="Asian (Japanese)",'japanese',
                                                                   ifelse(race_ethnicity=="Asian (Korean)",'other asian or pacific islander',
                                                                          ifelse(race_ethnicity=="Asian (Vietnamese)",'other asian or pacific islander',
                                                                                 ifelse(race_ethnicity=="Asian (Other)",'other asian or pacific islander',
                                                                                        ifelse(race_ethnicity=="Pacific Islander (Native Hawaiian)",'other asian or pacific islander',
                                                                                               ifelse(race_ethnicity=="Pacific Islander (Guamanian)",'other asian or pacific islander',
                                                                                                      ifelse(race_ethnicity=="Pacific Islander (Samoan)",'other asian or pacific islander',
                                                                                                             ifelse(race_ethnicity=="Pacific Islander (Other)",'other asian or pacific islander',
                                                                                                                    ifelse(race_ethnicity=="Some other race",'other race',NA))))))))))))))))
#modify the variable type for race_new
reduced_data$race_new <- as.factor(reduced_data$race_new)

#create a new variable 'sex' to take replace of 'gender'
reduced_data <- 
  reduced_data %>%
  mutate(sex=ifelse(gender=='Female','female','male'))

#create a new variable 'edu' to combine some levels in 'education' into new levels
reduced_data <- 
  reduced_data %>%
  mutate(edu=ifelse(education=="3rd Grade or less",'8th grade or less',
                    ifelse(education=="Middle School - Grades 4 - 8",'8th grade or less',
                           ifelse(education=="Completed some high school",'high school',
                                  ifelse(education=="High school graduate",'high school',
                                         ifelse(education=='Other post high school vocational training','high school',
                                                ifelse(education=='Completed some college, but no degree','some college',
                                                       ifelse(education=='Associate Degree','some college',
                                                              ifelse(education=='College Degree (such as B.A., B.S.)','some college',
                                                                     ifelse(education=='Completed some graduate, but no degree','5+ years of college',
                                                                            ifelse(education=='Masters degree','5+ years of college',
                                                                                   ifelse(education=='Doctorate degree','5+ years of college',NA))))))))))))

#select the final response and predictor variables that i need, and filter observations who were registered and clearly stated they would vote,
# and filter those who want to vote for Trump and Biden only.

new_survry_data<-
  reduced_data %>%
  select(vote_2020,sex,race_new,edu,registration,vote_intention) %>%
  filter((vote_2020=='Donald Trump'|vote_2020=='Joe Biden') &
           registration=='Registered' &
           vote_intention=='Yes, I will vote')%>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 'Donald Trump', "Joe Biden"))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(new_survry_data, "outputs/survey_data.csv")

