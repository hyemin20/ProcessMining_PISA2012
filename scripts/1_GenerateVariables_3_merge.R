###' ###########################################################################'
###' 
###' Project(project name): Process Mining_PISA2012
###' 
###' Category(stage in the project): 1_GenerateVariables_3_merge
###' 
###' Task(specific task in the category): merge all variables
###' 
###' data(data source): `PISA 2012 CBA 038q01`
###' 
###' date: 2022.03.27
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
original <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/1_GenerateVariables_time_2_mean.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()
sequence <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/ab_sequence_final.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' data with score
###' 
###'

head(original)
head(sequence)
names(sequence) <- c("ID", "credit", "sequence", "length")
sequence <- sequence[, -2]
all <- left_join(original, sequence, by = 'ID')

all$length <- ifelse(is.na(all$length), 0, all$length)
all <- all %>% 
  filter(time_not_answer >= 0)

write.csv(all,file="1_GenerateVariables_3_merge.csv")
