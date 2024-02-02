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



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_OutliersNormalization_3_FullC_7.0_with_total_score.csv", 
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' Random Forest
###' 
###'

### check NAs
summary(df)
summary(total_score)
total_score <- total_score %>%
  select(ID, TOTALSCORE)
df_n <- left_join(df, total_score, by = 'ID')
names(df_n)
df_n$cnt_code <- as.factor(df_n$cnt_code)
df_n$ranking <- as.factor(df_n$ranking)
df_n$rank_code <- as.factor(df_n$rank_code)
df_top <- df_n %>%
  filter(rank_code == '1')
df_low <- df_n %>%
  filter(rank_code == '4')
table(df_top$cnt)
table(df_low$cnt)

write.csv(df_n, "3_OutliersNormalization_3_FullC_7.0_with_total_score.csv")
 
# ### test set, train set
# idx <- sample(1:nrow(df),nrow(df)*0.7)
# df_train <- df[idx,]
# df_test <- df[-idx,] 
# 
# 
# ### for RandomForest-objects, by party::cforest()  
# set.seed(290875)
# df.cf <- party::cforest(cnt_code ~ time_A_total+time_b2+time_start+time_answer+time_not_answer+time_solving+time_total+
#                           events_num+avg_time_btw_events+length, data = party::df,
#                         control = party::cforest_unbiased(mtry = 2, ntree = 25))
# 
# ### conditional importance, may take a while...
# # party implementation:
# set.seed(290875)
# party::varimp(df.cf, conditional = TRUE)
# 
# # faster implementation but same results
# set.seed(290875)
# permimp(df.cf, conditional = TRUE, asParty = TRUE)
# 
# # different implementation with similar results
# set.seed(290875)
# permimp(df.cf, conditional = TRUE, asParty = FALSE)
# 
# ### standard (unconditional) importance is unchanged
# set.seed(290875)
# party::varimp(df.cf)
# set.seed(290875)
# permimp(df.cf)
# 
# 
# ###
# set.seed(290875)
# df.rf <- randomForest(cnt_code ~ time_A_total+time_b2+time_start+time_answer+time_not_answer+time_solving+time_total+
#                                       events_num+avg_time_btw_events+length, data = df, mtry = 2, ntree = 25, importance = TRUE,
#                                     keep.forest = TRUE, keep.inbag = TRUE)
# 
# 
# ### (unconditional) Permutation Importance
# set.seed(290875)
# permimp(df.rf, do_check = FALSE)
# 
# # very close to
# df.rf$importance[,1]
# 
# ### Conditional Permutation Importance
# set.seed(290875)
# permimp(df.rf, conditional = TRUE, threshold = .8, do_check = FALSE)
# 

ST04Q01


# ### fit a random forest 
# ### ... using the party package
 cfAirq5 <- cforest(TOTALSCORE ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_answer+time_not_answer+time_solving+time_total+avg_time_btw_events+length, data = df,
                    control = cforest_unbiased(mtry = 3, ntree = 1000,
                                               minbucket = 5, 
                                               minsplit = 10))
 
 ### compute the conditional permutation importance
 permimp_cf <- permimp(cfAirq5, conditional = TRUE)
 plot(permimp_cf, type = "box", interval = "quantile")
 

### fit a random forest: cnt
### ... using the randomForest package
rfAirq5 <- randomForest(TOTALSCORE ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_answer+time_not_answer+time_solving+time_total+avg_time_btw_events+length, data = df, 
                        importance = TRUE, ntree=2000,
                        keep.forest = TRUE, keep.inbag = TRUE)
### compute the conditional permutation importance                      
permimp_rf <- permimp(rfAirq5, conditional = TRUE)
#rfAirq5$confusion[, 'class.error']
plot(permimp_rf, horizontal = TRUE)
permimp_rf




varImpPlot(rfAirq5, sort = TRUE, n.var = min(30, nrow(rfAirq5$importance)),
           type = NULL, class = NULL, scale = TRUE,  
           main = deparse(substitute(rfAirq5)))




# ### test set, train set
set.seed(290875)
idx <- sample(1:nrow(df),nrow(df)*0.7, replace=TRUE)
df_train <- df[idx,]
df_test <- df[-idx,] 
rfAirq5 <- randomForest(TOTALSCORE ~ time_A_total+time_b1+time_b2+time_A_total+time_start+time_answer+time_not_answer+avg_time_btw_events+length+events_num, data = df_train, 
                        importance = TRUE, keep.forest = TRUE, keep.inbag = TRUE)
## train data의 rmse
rfAirq5$mse[length(rfAirq5$mse)]
sqrt(rfAirq5$mse[length(rfAirq5$mse)])

## test data의 rmse_1
mt <- predict(rfAirq5, df_test)
sqrt(mean((df_test$TOTALSCORE-mt)^2))

## test data의 rmse_2
rmse(df_test$TOTALSCORE, mt)
sd(df_test$TOTALSCORE-mt)


