###' ###########################################################################'
###' 
###' Project(project 0me): EM-IP
###' 
###' Category(stage in the project): data Cleaning
###' 
###' Task(specific task in the category): import and management
###' 
###' data(data source): `PISA 2012 CBA 038q01`
###' 
###' date: 2022.02.18
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
scoring <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/newscoring.csv", header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' data with score
###' 
###' 

### scoring data ID padding & produce
colnames(scoring)[1] <- 'cnt'
scoring$cnt <- str_pad(scoring$cnt, 3, pad = "0")
scoring$SCHOOLID <- str_pad(scoring$SCHOOLID, 7, pad = "0")
scoring$StIDStd <- str_pad(scoring$StIDStd, 5, pad = "0")
scoring[,320] <- paste0(scoring$cnt,scoring$SCHOOLID,scoring$StIDStd)
colnames(scoring)[320] <- "ID"
scoring <- scoring %>%
  relocate("ID", .before = cnt)



### double check whether selected variables are enough
table(scoring$CP002Q06)

scoring_1 <- scoring %>%
  select(ID, cnt, starts_with("CP")) %>%
  mutate(CP002Q06 = factor(CP002Q06,
                      levels = c(0,1,2,7,8),
                      labels = c(0,1,2,NA,0))) %>%
  mutate(CP002Q07 = factor(CP002Q07,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP002Q08 = factor(CP002Q08,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP007Q01 = factor(CP007Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP007Q02 = factor(CP007Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP007Q03T = factor(CP007Q03T,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP010Q01 = factor(CP010Q01,
                            levels = c(0,1,2,7,8),
                            labels = c(0,1,2,0,0))) %>%
  mutate(CP010Q05 = factor(CP010Q05,
                            levels = c(0,1,2,7,8),
                            labels = c(0,1,2,NA,0))) %>%
  mutate(CP010Q06 = factor(CP010Q06,
                            levels = c(0,1,2,7,8),
                            labels = c(0,1,2,NA,0))) %>%
  mutate(CP014Q01 = factor(CP014Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP014Q02 = factor(CP014Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP014Q06 = factor(CP014Q06,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP015Q01 = factor(CP015Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP015Q02 = factor(CP015Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP015Q04 = factor(CP015Q04,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP018Q04T = factor(CP018Q04T,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP018Q05 = factor(CP018Q05,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP025Q01 = factor(CP025Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP025Q02 = factor(CP025Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP027Q01T = factor(CP027Q01T,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP027Q02 = factor(CP027Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP028Q01 = factor(CP028Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP028Q02 = factor(CP028Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP029Q01 = factor(CP029Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP029Q02 = factor(CP029Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP032Q01 = factor(CP032Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP032Q02T = factor(CP032Q02T,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP032Q04 = factor(CP032Q04,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP034Q01T = factor(CP034Q01T,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP034Q02 = factor(CP034Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP034Q05 = factor(CP034Q05,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP036Q01 = factor(CP036Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP036Q02 = factor(CP036Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP036Q03 = factor(CP036Q03,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP037Q01 = factor(CP037Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP037Q02 = factor(CP037Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP037Q03 = factor(CP037Q03,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP038Q01 = factor(CP038Q01,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP038Q02 = factor(CP038Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP038Q03 = factor(CP038Q03,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP041Q02 = factor(CP041Q02,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0))) %>%
  mutate(CP041Q03 = factor(CP041Q03,
                           levels = c(0,1,2,7,8),
                           labels = c(0,1,2,NA,0)))


write.csv(scoring_1, "0_StartWith_0_scoring_cleaning.csv")

### select CP038Q01 and change 0me to credit
### delete 7, 8 and recode 0,1,2 to 0,0,1
scoring_S <- scoring %>%
  relocate("ID", .before = cnt) %>%
  select(ID, OECD, CP038Q01) %>%
  filter(CP038Q01 %in% c(0,1,2)) %>%
  mutate(CP038Q01 = factor(CP038Q01,
                           levels = c(0,1,2),
                           labels = c(0,0,1)))

col0mes(scoring_S)[3] <- 'credit'
table(scoring_S$credit)


### join logdata and credit
origi0l_scored <- inner_join(origi0l, scoring_S, by = "ID")
write.csv(origi0l_scored, "origi0l_scored.csv")


