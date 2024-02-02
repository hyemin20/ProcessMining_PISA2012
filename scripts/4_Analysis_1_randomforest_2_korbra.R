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
library(haven); library(tidyverse); library(readr); library(writexl); library(dplyr); library(ggplot2); library(randomForest)
library(rsconnect); library(bupaR); library(edeaR); library(processmapR); library(petrinetR); library(pm4py); library(tidymodels)
library(randomForestExplainer); library(yardstick); library(cli); library(tidymodels); library(ranger)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/korbra.csv", 
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' Random Forest
###' 
###'

### check NAs
summary(df)
df <- df %>%
  mutate(cnt = factor(cnt,
                      levels = c('KOR','BRA'),
                      labels = c(1,2)))




# random forest model
rf_imp <- ranger(cnt ~ time_A_total+time_b2+time_start+time_answer+time_not_answer+time_solving+time_total+
                   events_num+avg_time_btw_events+length, data = df, importance = 'impurity')
rf_perm <- ranger(cnt~ time_A_total+time_b2+time_start+time_answer+time_not_answer+time_solving+time_total+
                    events_num+avg_time_btw_events+length, data = df, importance = 'permutation')

time_A_total+time_b1+time_b2+time_B_total+time_start+time_answer+time_not_answer+time_solving+time_total+events_num+avg_time_btw_events+length
#time_end, 

# barplot
df1 <- data.frame(
  var = names(rf_imp$variable.importance), imp = c(rf_imp$variable.importance)
)
df1$data_type <- 'impurity'



df2 <- data.frame(
  var = names(rf_perm$variable.importance), imp = c(rf_perm$variable.importance)
)
df2$data_type <- 'permutation'
df <- rbind(df1, df2)
df$col <- df$var == 'noise'

ggplot(df) + 
  geom_bar(aes(x=reorder(var, imp), y=imp, fill=col), stat = 'identity') + 
  guides(fill=F) + xlab('variables') +
  scale_fill_manual(values = c('grey45', 'deeppink4')) +
  facet_wrap(~ data_type, scales = 'free')

