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
###' date: 2022.04.09
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
library(haven); library(tidyverse); library(readr); library(writexl); library(dplyr); library(ggplot2)
library(rsconnect); library(bupaR); library(edeaR); library(processmapR); library(petrinetR); library(pm4py)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/5_Clustering_2_start,notanswer,Atotal_4C.csv",
               header = TRUE, stringsAsFactors = FALSE)



###' ###########################################################################'
###' 
###' df
###' 
###'

summary(df)
a <- df %>% 
  filter(cluster == 0) %>%
  summarise(mean_S = mean(time_start),
            mean_NA = mean(time_not_answer),
            mean_A = mean(time_A_total),
            sd_S = sd(time_start),
            sd_NA = sd(time_not_answer),
            sd_A = sd(time_A_total))
nrow(d)

b <- df %>% 
  filter(cluster == 1) %>%
  summarise(mean_S = mean(time_start),
            mean_NA = mean(time_not_answer),
            mean_A = mean(time_A_total),
            sd_S = sd(time_start),
            sd_NA = sd(time_not_answer),
            sd_A = sd(time_A_total))


c <- df %>% 
  filter(cluster == 2) %>%
  summarise(mean_S = mean(time_start),
            mean_NA = mean(time_not_answer),
            mean_A = mean(time_A_total),
            sd_S = sd(time_start),
            sd_NA = sd(time_not_answer),
            sd_A = sd(time_A_total))

d <- df %>% 
  filter(cluster == 3) %>%
  summarise(mean_S = mean(time_start),
            mean_NA = mean(time_not_answer),
            mean_A = mean(time_A_total),
            sd_S = sd(time_start),
            sd_NA = sd(time_not_answer),
            sd_A = sd(time_A_total))


names(df)
df %>% 
  filter(cluster == 0) %>%
  summarise(M_b1 = mean(time_b1),
            S_b1 = sd(time_b1),
            M_b2 = mean(time_b2),
            S_b2 = sd(time_b2),
            M_t = mean(time_total),
            S_t = sd(time_total),
            M_avg = mean(avg_time_btw_events),
            S_avg = sd(avg_time_btw_events),
            M_len = mean(length),
            S_len = sd(length),
            M_num = mean(events_num),
            S_num = sd(events_num))
