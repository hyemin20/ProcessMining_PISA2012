###' ###########################################################################'
###' 
###' Project(project name): Process Mining_PISA2012
###' 
###' Category(stage in the project): 2_ExploratoryAnalysis
###' 
###' Task(specific task in the category):descriptive statistics
###' 
###' data(data source): `PISA 2012 CBA 038q01`
###' 
###' date: 2022.04.05
###' 
###' Author: Hyemin Park(`hyemin.park@snu.ac.kr`)
###' 
###'

###' ###########################################################################'
###' 
###' Basic settings
###' 
###' 

### Start with clean state
gc(); rm(list=ls())


### Set working directory and data directory
work_dir <- c("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012")
data_dir <- file.path(work_dir, "datasets")
setwd(work_dir)


### Call libraries
library(haven); library(tidyverse); library(readr); library(writexl); library(dplyr)
library(rsconnect); library(bupaR); library(edeaR); library(processmapR); library(petrinetR); library(pm4py)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
df_all <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/2_ExploratoryAnalysis_0_start_with_all.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()
df_full <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/2_ExploratoryAnalysis_0_start_with_full_credit.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' df_all
###' - NAs of each variables
###' - summary of each variables(mean, sd, quartile)
###' 
###'

###
names(df_all)
df_all <- df_all[,-c(1,2)]

map(df_all, mean, na.rm = TRUE)
map(df_all, sd, na.rm = TRUE)
map(df_all, max, na.rm = TRUE)
map(df_all, min, na.rm = TRUE)
map(df_all, summary, na.rm = TRUE)

table(df_all$credit)
table(df_all$OECD)



###' ###########################################################################'
###' 
###' df_full
###' - NAs of each variables
###' - summary of each variables(mean, sd, quartile)
###' 
###'

### 
names(df_full)
df_full <- df_full[!(df_full$ID == 'ARE000006801770'), ]
df_full <- df_full[,-c(1,2)]


df_full %>%
  filter(length == 1) %>%
  select(ID, time_answer, sequence, length)

map(df_full, mean, na.rm = TRUE)
map(df_full, sd, na.rm = TRUE)
map(df_full, max, na.rm = TRUE)
map(df_full, min, na.rm = TRUE)
map(df_full, summary, na.rm = TRUE)

table(df_full$credit)
table(df_full$OECD)
table(df_full$length)

df_full %>%
  filter(length == 0) %>%
  select(ID, credit)


df_full %>%
  filter(time_b1 < 0) %>%
  select(ID, time_b1, time_answer, sequence, length)



###' ###########################################################################'
###' 
###' df_full
###' - NAs of each variables
###' - summary of each variables(mean, sd, quartile)
###' 
###'

names(df_full)
library(ggplot2)

ggplot(data = df_full) + 
  geom_density(mapping=aes(x=events_num))

ggplot(data = df_full) + 
  geom_density(mapping=aes(x=time_not_answer))

ggplot(data = df_full) + 
  geom_density(mapping=aes(x=time_b2))

ggplot(data = df_full) + 
  geom_density(mapping=aes(x=length))

table(df_full$length)
