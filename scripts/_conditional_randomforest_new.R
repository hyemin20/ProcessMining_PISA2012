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
library(randomForestExplainer); library(yardstick); library(cli); library(tidymodels); library(party); library(permimp);library(Metrics)
library(caret)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_Outliers_3_FullC_without_ln_scoring_gender.csv", 
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' Random Forest
###' 
###'



# ### fit a random forest 
# ### ... using the party package
cfAirq5 <- cforest(ratio ~ time_a+time_b1+time_b2+time_b3+time_b4+
                     time_start+time_solving+time_total+time_answer+time_irrelevant+
                     time_explore_relevant+time_ticket_explore+time_explore_total+time_avg_explore+
                     time_avg_btw_events+events_num+length,
                   data = df, control = cforest_unbiased(mtry = 5, ntree = 1000,
                                              minbucket = 5, 
                                              minsplit = 10))

### compute the conditional permutation importance
permimp_cf <- permimp(cfAirq5, conditional = TRUE)
plot(permimp_cf, type = "box", interval = "quantile")



names(df)


### fit a random forest: cnt
### ... using the randomForest package
score_ratio_RF <- randomForest(ratio ~ time_a+time_b1+time_b2+time_b3+time_b4+
                                 time_start+time_solving+time_total+time_answer+time_irrelevant+
                                 time_explore_relevant+time_ticket_explore+time_explore_total+time_avg_explore+
                                 time_avg_btw_events+events_num+length,
                               data = df, importance = TRUE, keep.forest = TRUE, keep.inbag = TRUE)

### compute the conditional permutation importance                      
score_ratio_FI <- permimp(score_ratio_RF, conditional = TRUE)
#rfAirq5$confusion[, 'class.error']
plot(score_ratio_FI, horizontal = TRUE)
score_ratio_FI




varImpPlot(rfAirq5, sort = TRUE, n.var = min(30, nrow(rfAirq5$importance)),
           type = NULL, class = NULL, scale = TRUE,  
           main = deparse(substitute(rfAirq5)))




# ### test set, train set
set.seed(290875)
idx <- sample(1:nrow(df),nrow(df)*0.7, replace=TRUE)
df_train <- df[idx,]
df_test <- df[-idx,] 
rfAirq5 <- randomForest(weight ~ time_a+time_b1+time_b2+time_b3+time_b4+
                          time_start+time_solving+time_total+time_answer+time_irrelevant+
                          time_explore_relevant+time_ticket_explore+time_explore_total+time_avg_explore+
                          time_avg_btw_events+events_num+length,
                        data = df_train, importance = TRUE, keep.forest = TRUE, keep.inbag = TRUE)
## train data의 rmse
rfAirq5$mse[length(rfAirq5$mse)]
sqrt(rfAirq5$mse[length(rfAirq5$mse)])

## test data의 rmse_1
mt <- predict(rfAirq5, df_test)
sqrt(mean((df_test$weight-mt)^2))

## test data의 rmse_2
rmse(df_test$weight, mt)
sd(df_test$weight-mt)

