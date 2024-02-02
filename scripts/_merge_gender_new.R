###' ###########################################################################'
###' 
###' Project(project name): Process Mining_PISA2012
###' 
###' Category(stage in the project): 4_Analysis
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



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
df_gender <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/0_StartWith_0_gender.csv", 
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()
df_noln <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_Outliers_3_FullC_without_ln.csv", 
                header = TRUE, stringsAsFactors = FALSE) %>% tibble()
df_ln <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_Outliers_3_FullC_with_ln.csv", 
                  header = TRUE, stringsAsFactors = FALSE) %>% tibble()


###' ###########################################################################'
###'
###'
###'

head(df_gender)

df_gender$CNT <- str_pad(df_gender$CNT, 3, pad = "0")
df_gender$SCHOOLID <- str_pad(df_gender$SCHOOLID, 7, pad = "0")
df_gender$StIDStd <- str_pad(df_gender$StIDStd, 5, pad = "0")
df_gender[,5] <- paste0(df_gender$CNT,df_gender$SCHOOLID,df_gender$StIDStd)
colnames(df_gender)[5] <- "ID"
colnames(df_gender)[4] <- "gender"
df_gender <- df_gender %>%
  relocate("ID", .before = CNT) %>%
  select(ID, gender)


a_noln <- left_join(df_noln, df_gender, by = 'ID')
a_ln <- left_join(df_ln, df_gender, by = 'ID')



write.csv(a_noln, "3_Outliers_3_FullC_without_ln_gender.csv")
write.csv(a_ln, "3_Outliers_3_FullC_with_ln_gender.csv")
