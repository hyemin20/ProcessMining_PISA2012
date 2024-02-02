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
log_data <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/0_StartWith_4_final.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()
original <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/1_GenerateVariables_1_time_num.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()
sequence <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/1_GenerateVariables_2_sequence_abs_and_length.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' data merge
###' 
###'

head(log_data)
head(original)
head(sequence)
names(sequence) <- c("ID", "sequence", "length")


### merge
log_data_1 <- log_data %>%
  filter(credit == 1) %>%
  select(ID, credit, OECD) %>%
  unique()

nrow(sequence)
nrow(original)
nrow(log_data_1)

nrow(distinct(all_2, ID))

all_1  <- left_join(original, sequence, by = 'ID')
all_2  <- inner_join(log_data_1, all_1, by = 'ID')
all_2$length <- ifelse(is.na(all_2$length), 0, all_2$length)


nrow(all_2)


### check all_2
names(all_2)
summary(all_2)

# df_nomiss <- all_2 %>% filter(is.na(avg_time_btw_events))
# all_2 <- all_2[!(all_2$ID == "ARE000006801770" ), ]
# df_nomiss <- na.omit(all_2)

write.csv(all_2,file="1_GenerateVariables_3_merge.csv")
