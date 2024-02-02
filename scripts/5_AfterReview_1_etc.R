
### Start with clean state
gc(); rm(list=ls())


library(readr); library(tidyverse)


### Set file path
df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_1_GenerateVariables_1_time_abs.csv") %>% tibble()
abs <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_2_ab_score_merge.csv") %>% tibble()
score <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_3_scoring_final.csv") %>% tibble()


abs_need <- abs %>%
  select(ID, sequence, length)
score_need <- score %>%
  select(ID, ratio)

a <- left_join(df, abs_need, by='ID')
b <- left_join(a, score_need, by='ID')

write.csv(b, "D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_4_merge.csv")

nrow(df)
nrow(etc)



df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_5_GenerateVariables_1_time_mean.csv") %>% tibble()
ticket <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_6_ticket_explore.csv") %>% tibble()
a <- left_join(df, ticket, by='ID')

write.csv(a, "D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_7_merge.csv")






df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_7_merge.csv") %>% tibble()
credit <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_8_credit.csv") %>% tibble()

credit_a <- credit %>%
  select(ID, credit)
credit_b <- unique(credit_a)

a <- left_join(df, credit_b, by='ID')
credit_need <- a %>%
  filter(credit == 0)


write.csv(credit_need, "D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_10_credit_filter.csv")






df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_0_logdata_after.csv") %>% tibble()
cluster <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_12_Clustering_1se.csv") %>% tibble()

cluster_a <- cluster %>%
  select(ID, cluster)

a <- left_join(df, cluster_a, by='ID')

write.csv(a, "D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_13_logdata_cluster_1se.csv")






df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_5_GenerateVariables_1_time_mean.csv") %>% tibble()
credit <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_8_credit.csv") %>% tibble()

credit_a <- credit %>%   select(ID, credit)
credit_b <- unique(credit_a)

a <- left_join(df,credit_b, by = 'ID')
nrow(a)

table(a$credit)

30453 - 7191






time_end <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1030_14_time_end.csv") %>% tibble()
df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_12_clustering_1se.csv") %>% tibble()

nrow(time_end)
nrow(df)

time_end_need <- time_end %>%
  select(ID, time_end)

a <- left_join(df, time_end_need, by ='ID')
table(a$credit)
write.csv(a, "D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1030_15_merge.csv")





cluster2 <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1030_16_cluster.csv") %>% tibble()
df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_11_logdata_cluster_mean.csv") %>% tibble()

nrow(cluster2)
nrow(df)

cluster2_need <- cluster2 %>%
  select(ID, cluster)

a <- left_join(df, cluster2_need, by ='ID')
table(a$credit)
write.csv(a, "D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1030_17_logdata_cluster2.csv")






cluster3 <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1030_18_cluster3.csv") %>% tibble()
df <- read_csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1022_11_logdata_cluster_mean.csv") %>% tibble()

nrow(cluster3)
nrow(df)

cluster3_need <- cluster3 %>%
  select(ID, cluster)
df_need <- df %>%
  select(!cluster)
names(df_need)

a <- left_join(df_need, cluster3_need, by ='ID')

write.csv(a, "D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets_after/1030_19_logdata_cluster3.csv")
