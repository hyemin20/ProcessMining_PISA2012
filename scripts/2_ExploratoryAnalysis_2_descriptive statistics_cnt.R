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
df <- readf <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_OutliersNormalization_3_FullC_7.0_with_score.csv", 
                        header = TRUE, stringsAsFactors = FALSE) %>% tibble()

###' ###########################################################################'
###' 
###' df
###' 
###'

###
names(df)

a <- df %>%
  group_by(cnt) %>%
  summarise(A_mean = mean(time_A_total),
            A_sd = sd(time_A_total),
            A_range = max(time_A_total) - min(time_A_total),
            b1_mean = mean(time_b1),
            b1_sd = sd(time_b1),
            b1_range = max(time_b1) - min(time_b1),
            b2_mean = mean(time_b2),
            b2_sd = sd(time_b2),
            b2_range = max(time_b2) - min(time_b2),
            B_mean = mean(time_B_total),
            B_sd = sd(time_B_total),
            B_range = max(time_B_total) - min(time_B_total))
a
a[11:21,]



b <- df %>%
  group_by(cnt) %>%
  summarise(start_mean = mean(time_start),
            start_sd = sd(time_start),
            start_range = max(time_start) - min(time_start),
            end_mean = mean(time_end),
            end_sd = sd(time_end),
            end_range = max(time_end) - min(time_end),
            answer_mean = mean(time_answer),
            answer_sd = sd(time_answer),
            answer_range = max(time_answer) - min(time_answer))

b
b[11:21,]


c <- df %>%
  group_by(cnt) %>%
  summarise(Nanswer_mean = mean(time_not_answer),
            Nanswer_sd = sd(time_not_answer),
            Nanswer_range = max(time_not_answer) - min(time_not_answer),
            sol_mean = mean(time_solving),
            sol_sd = sd(time_solving),
            sol_range = max(time_solving) - min(time_solving),
            total_mean = mean(time_total),
            total_sd = sd(time_total),
            total_range = max(time_total) - min(time_total))

c
c[11:21,]


d <- df %>%
  group_by(cnt) %>%
  summarise(num_mean = mean(events_num),
            num_sd = sd(events_num),
            num_range = max(events_num) - min(events_num),
            avg_mean = mean(avg_time_btw_events),
            avg_sd = sd(avg_time_btw_events),
            avg_range = max(avg_time_btw_events) - min(avg_time_btw_events),
            length_mean = mean(length),
            length_sd = sd(length),
            length_range = max(length) - min(length))

d
d[11:21,]


map(df, mean)
map(df, sd)
df %>%
  summarise(length_range = max(time_b2) - min(time_b2))


