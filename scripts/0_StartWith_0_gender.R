###' ###########################################################################'
###' 
###' Project(project name): Process Mining_PISA2012
###' 
###' Category(stage in the project): 0_gender
###' 
###' Task(specific task in the category):gender coding
###' 
###' data(data source): `PISA 2012 CBA 038q01`
###' 
###' date: 2022.04.22
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
library(readr); library(tidyverse); library(dplyr)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
file_path_a <- file.path(data_dir, "0_StartWith_4_final.csv")
file_path_b <- file.path(data_dir, "3_OutliersNormalization_3_FullC_7.0_with_total_score.csv")
file_path_c <- file.path(data_dir, "0_StartWith_0_gender.xlsx")
df_log <- read_csv(file_path_a) %>% tibble()
df_ON_f <- read_csv(file_path_b) %>% tibble()
df_gender <- read_excel(file_path_c) %>% tibble()



###' ###########################################################################'
###' 
###' data with gender
###' 
###' 

### original data ID padding & produce

df_gender <- df_gender[,-1]
head(df_gender$cnt)
df_gender$cnt <- str_pad(df_gender$cnt, 3, pad = "0")
df_gender$SCHOOLID <- str_pad(df_gender$SCHOOLID, 7, pad = "0")
df_gender$StIDStd <- str_pad(df_gender$StIDStd, 5, pad = "0")
df_gender[,5] <- paste0(df_gender$cnt,df_gender$SCHOOLID,df_gender$StIDStd)
colnames(df_gender)[5] <- "ID"

table(df_gender$gender)

a <- df_gender %>%
  relocate("ID", .before = cnt) %>%
  filter(gender == "Yes, for one year or less")

  select(ID, gender) %>%
  mutate(gender = factor(gender,
                         labels = c(1,2),
                         levels = c("Male", "Female")))


### scoring data ID padding & produce
colnames(scoring)[1] <- 'cnt'
scoring$cnt <- str_pad(scoring$cnt, 3, pad = "0")
scoring$SCHOOLID <- str_pad(scoring$SCHOOLID, 7, pad = "0")
scoring$StIDStd <- str_pad(scoring$StIDStd, 5, pad = "0")
scoring[,320] <- paste0(scoring$cnt,scoring$SCHOOLID,scoring$StIDStd)
colnames(scoring)[320] <- "ID"


### double check whether selected variables are enough
scoring %>%
  select(starts_with("CP025"),starts_with("CP038"))
table(scoring$CP025Q01)
table(scoring$CP025Q02)
table(scoring$CP038Q01)
table(scoring$CP038Q02)
table(scoring$CP038Q03)


### select CP038Q01 and change name to credit
### delete 7, 8 and recode 0,1,2 to 0,0,1
scoring_S <- scoring %>%
  relocate("ID", .before = cnt) %>%
  select(ID, OECD, CP038Q01) %>%
  filter(CP038Q01 %in% c(0,1,2)) %>%
  mutate(CP038Q01 = factor(CP038Q01,
                           levels = c(0,1,2),
                           labels = c(0,0,1)))

colnames(scoring_S)[3] <- 'credit'
table(scoring_S$credit)


### join logdata and credit
original_scored <- inner_join(original, scoring_S, by = "ID")
write.csv(original_scored, "original_scored.csv")
### Set file path
df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_OutliersNormalization_3_FullC_7.0_with_total_score.csv", 
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()

df1 <- read_excel("C:/Users/OWNER/Downloads/ID_gender.xlsx") %>% tibble()
library(readxl)

df1 %>%
  select(ID, gender)

a <- left_join(df, df1, by='ID')

sum(is.na(a))




