



df <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/5_NewNewClustering_1_start,b1,b3,irre,avgevents_4C.csv",
               header = TRUE, stringsAsFactors = FALSE) %>% tibble()
names(df)
df_a <- df[,-c(1:6)]
names(df_a)

summary(df_a)
sum(is.na(df_a))



#################################################################################'
#################################################################################'

df_all <- read.csv("D:/HYEM'S/GraduatedSchool/PROJECTS/MyProjects/ProcessMining_PISA2012/datasets/3_Outliers_3_FullC_with_ln_scoring_gender.csv", 
                   header = TRUE, stringsAsFactors = FALSE) %>% tibble()
df_ID <- read.csv("C:/Users/OWNER/Downloads/ID.csv", 
                  header = TRUE, stringsAsFactors = FALSE) %>% tibble()
names(df_ID) <- c("num","ID")
df_a <- anti_join(df_all, df_ID, by = "ID")

nrow(df_all)
nrow(df_ID)
nrow(df_a)


write.csv(df_a, "3_Outliers_3_FullC_with_ln_scoring_gender_time_cleaning.csv")


#################################################################################'
#################################################################################'

library(quadrupen); library(enetLTS); library(glmnet); library(caret)

set.seed(123)
train <- createDataPartition(y=df$ratio, p=0.7, list=FALSE)
train1 <- df[train,]
train2 <- df[-train,]

x <- model.matrix(ratio ~ time_a+time_b1+time_b2+time_b3+time_b4+time_start+
                    time_solving+time_total+time_answer+time_irrelevant+time_explore_relevant+time_explore_total+
events_num+length+time_avg_explore+time_avg_btw_events+time_ticket_explore, train1)[,-1]
y <- train1$ratio


set.seed(123)
elastic1 <- train(form=ratio ~ time_a+time_b1+time_b2+time_b3+time_b4+time_start+
                    time_solving+time_total+time_answer+time_irrelevant+time_explore_relevant+time_explore_total+
                    events_num+length+time_avg_explore+time_avg_btw_events+time_ticket_explore, data=df, method='glmnet',
                  trainControl=trainControl(method='cv',number=10), tuneLength=10)
elastic2 <- elastic1$bestTune
elastic2

elastic3 <- glmnet(x,y,family = 'gaussian', alpha=elastic2$alpha, lambda = elastic2$lambda)
coef(elastic3)


z <- model.matrix(ratio ~ time_a+time_b1+time_b2+time_b3+time_b4+time_start+
                    time_solving+time_total+time_answer+time_irrelevant+time_explore_relevant+time_explore_total+
                    events_num+length+time_avg_explore+time_avg_btw_events+time_ticket_explore, train2)[,-1]
elastic4 <- predict(elastic3, newx = z)
postResample(pred = elastic4, obs=train2$ratio)


#################################################################################'
#################################################################################'

train = matrix(NA, round(7191*0.7), 100)
for(r in 1:100){
  set.seed(r)
  train[,r] <- sort(sample(1:7191, round(7191*0.7), replace = F))
}

df_a <- apply(df_a, 2, as.numeric)
str(df_a)

names(df_a)

# ridge
CV.ridge <- list()
for(r in 1:100){
  start_time <- Sys.time()
  set.seed(r)
  CV.ridge[[r]] <- cv.glmnet(x = df_a[train[,r], 1:17], y = df_a[train[,r],18],
                             nfold = 10, alpha = 0)
  end_time <- Sys.time()
  
  print(r)
  print(end_time - start_time)
}


# Enet
CV.Enet <- list()

for(r in 1:100){
  start_time <- Sys.time()
  set.seed(r)
  CV.Enet[[r]] <- cv.glmnet(x = df_a[train[,r], 1:17], y = df_a[train[,r], 18],
                            nfolds = 10, alpha = 0.5)
  end_time <- Sys.time()
  
  print(r)
  print(end_time - start_time)
}


# ridge
MOD.ridge <- list()

for(r in 1:100){
  start_time <- Sys.time()
  MOD.ridge[[r]] <- glmnet(x = df[train[,r], 1:17], y = df[train[,r], 18],
                           alpha = 0, lambda = CV.ridge[[r]]$lambda.min)
  end_time <- Sys.time()
  
  print(r)
  print(end_time - start_time)
}



# Enet
MOD.Enet <- list()

for(r in 1:100){
  start_time <- Sys.time()
  MOD.Enet[[r]] <- glmnet(x = df_a[train[,r], 1:17], y = df_a[train[,r], 18],
                          alpha = 0.5, lambda = CV.Enet[[r]]$lambda.min)
  end_time <- Sys.time()
  
  print(r)
  print(end_time - start_time)
}



# ridge
Beta.ridge <- matrix(NA, 100, ncol(df_a[, 1:17]))

for(r in 1:100){
  Beta.ridge[r,] <- MOD.ridge[[r]]$beta[,1]
}

colnames(Beta.ridge) <- colnames(df[, 1:17])
rownames(Beta.ridge) <- 1:100


#Enet
Beta.Enet <- matrix(NA, 100, ncol(df[,1:17]))

for(r in 1:100){
  Beta.Enet[r,] <- MOD.Enet[[r]]$beta[,1]
}

colnames(Beta.Enet) <- colnames(df[, 1:17])
rownames(Beta.Enet) <- 1:100




# ridge
Selection.ridge = matrix(NA, ncol(df[,1:17]), 8)
rownames(Selection.ridge) <- colnames(df[,1:17])
colnames(Selection.ridge) <- c("#", "Min", "Q1", "Med", "Q3", "Max", "Mean", "SD")

# Selection Counts
Selection.ridge[,1] <- colSums(abs(sign(Beta.ridge)))

# Quantile
for(p in 1:ncol(df[,1:17])){
  Selection.ridge[p, 2:6] <- quantile(Beta.ridge[Beta.ridge[,p] != 0, p])
}

# Mean
for(p in 1:ncol(df[,1:17])){
  Selection.ridge[p,7] <- mean(Beta.ridge[Beta.ridge[,p] !=0, p])
}

# SD
for(p in 1:ncol(df[,1:17])){
  Selection.ridge[p,8] <- sd(Beta.ridge[Beta.ridge[,p] != 0, p])
}

Selection.ridge[is.na(Selection.ridge)] <- 0
Selection.ridge[is.nan(Selection.ridge)] <- 0
round(Selection.ridge, 3)



# Enet
Selection.Enet = matrix(NA, ncol(df_a[,1:17]), 8)
rownames(Selection.Enet) <- colnames(df_a[,1:17])
colnames(Selection.Enet) <- c("#", "Min", "Q1", "Med", "Q3", "Max", "Mean", "SD")

# Selection Counts
Selection.Enet[,1] <- colSums(abs(sign(Beta.Enet)))

#Quantile
for(p in 1:ncol(df_a[, 1:17])){
  Selection.Enet[p, 2:6] <- quantile(Beta.Enet[Beta.Enet[,p] !=0,p])
}

# Mean
for(p in 1:ncol(df_a[,1:17])){
  Selection.Enet[p,7] <- mean(Beta.Enet[Beta.Enet[,p] !=0, p])
}

# SD
for(p in 1:ncol(df_a[, 1:17])){
  Selection.Enet[p, 8] <- sd(Beta.Enet[Beta.Enet[,p] !=0, p])
}

Selection.Enet[is.na(Selection.Enet)] <- 0
Selection.Enet[is.nan(Selection.Enet)] <- 0
round(Selection.Enet, 3)


# RMSE
library(caret)
class(df_a)


Pred.Enet = list()
for(r in 1:100){
  Pred.Enet[[r]] <- predict(MOD.Enet[[r]], newx = df_a[-train[,r], 1:15], 
                            type = "response")
}

RMSE.Enet = c()
for(r in 1:100){
  RMSE.Enet[r] <- RMSE(Pred.Enet[[r]], df_a[-train[,r], 16])
}

summary(RMSE.Enet)
sd(RMSE.Enet)


