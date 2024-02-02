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
library(randomForestExplainer); library(yardstick); library(cli); library(tidymodels)



###' ###########################################################################'
###' 
###' Import data
###' 
###' 

### Set file path
df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_OutliersNormalization_3_FullC_7.0_with_ranking.csv", 
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()



###' ###########################################################################'
###' 
###' Random Forest
###' 
###'

### check NAs
summary(df)



### test set, train set
df$rankings <- as.factor(df$rankings)
idx <- sample(1:nrow(df),nrow(df)*0.7)
df_train <- df[idx,]
df_test <- df[-idx,] # 해당 인덱스 제외하고 추출

df_train %>%
  recipe(rankings ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_end+time_answer+time_not_answer+time_solving+time_total+
           events_num+avg_time_btw_events+length)

df_recipe <- df_train %>%
  recipe(rankings ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_end+time_answer+time_not_answer+time_solving+time_total+
           events_num+avg_time_btw_events+length) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

df_testing <- df_recipe %>% bake(df_train)
df_training <- df_recipe %>% juice()

df_rf <- rand_forest(trees=100, mode='classification') %>%
  set_engine('randomForest') %>%
  fit(rankings ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_end+time_answer+time_not_answer+time_solving+time_total+
        events_num+avg_time_btw_events+length, data=df_training)

df_rg <- rand_forest(trees=100, mode='classification') %>%
  set_engine('ranger') %>%
  fit(rankings ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_end+time_answer+time_not_answer+time_solving+time_total+
        events_num+avg_time_btw_events+length, data=df_training)

df_rf %>%
  predict(df_testing) %>%
  bind_cols(df_testing)

df_rf %>%
  predict(df_testing) %>%
  bind_cols(df_testing) %>%
  metrics(truth=rankings, estimate=.pred_class)

## importance
a <- measure_importance(df_rf$fit, localImp = TRUE)

measure_importance(df_rf$fit) %>%
  as_tibble() %>%
  mutate(imp=p_value*100/max(p_value)) %>%
  arrange(-imp) %>%
  select(variable, imp)

measure_importance(df_rf$fit) %>%
  as_tibble() %>%
  mutate(imp=gini_decrease*100/max(gini_decrease)) %>%
  arrange(-imp) %>%
  select(variable, imp)


measure_no_of_nodes(a)


summary(df)


set.seed(2017)
forest <- randomForest(rankings ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_end+time_answer+time_not_answer+time_solving+time_total+
                         events_num+avg_time_btw_events+length, data = df,localImp = TRUE)
min_depth_frame <- min_depth_distribution(forest)
# save(min_depth_frame, file = "min_depth_frame.rda")
#load("min_depth_frame.rda")
head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame)

plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

importance_frame <- measure_importance(forest)
#save(importance_frame, file = "importance_frame.rda")
#load("importance_frame.rda")
importance_frame  %>%
  as_tibble() %>%
  mutate(imp=accuracy_decrease *100/max(accuracy_decrease )) %>%
  arrange(-imp) %>%
  select(variable, imp)
  





rf_fit_perm <- randomForest(rankings ~ time_A_total+time_b1+time_b2+time_B_total+time_start+time_end+time_answer+time_not_answer+time_solving+time_total+
                              events_num+avg_time_btw_events+length, data = df, importance = T)

df <- data.frame(
  var = rownames(rf_fit_perm$importance), 
  imp = c(rf_fit_perm$importance[,'MeanDecreaseGini'])
)
df$col <- df$var == 'noise'

rf_fit_perm$importance %>%
  as_tibble() %>%
  arrange(-MeanDecreaseGini)


ggplot(df) + 
  geom_bar(aes(x=reorder(var, imp), y=imp, fill=col), stat = 'identity') + 
  guides(fill=F) + 
  scale_fill_manual(values = c('grey45', 'deeppink4')) +
  xlab('variables')






library(ranger)

# random forest model
rf_imp <- ranger(rankings ~ time_A_total+time_b2+time_start+time_answer+time_not_answer+time_solving+time_total+
                   events_num+avg_time_btw_events+length, data = df, importance = 'impurity')
rf_perm <- ranger(rankings ~ time_A_total+time_b2+time_start+time_answer+time_not_answer+time_solving+time_total+
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

