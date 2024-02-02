df <- read.csv(file.choose())

train = matrix(NA, round(7191*0.7), 100)
for(r in 1:100){
  set.seed(r)
  train[,r] <- sort(sample(1:7191, round(7191*0.7), replace = F))
}

df <- apply(df, 2, as.numeric)
str(df)



# ridge
CV.ridge <- list()
for(r in 1:100){
  start_time <- Sys.time()
  set.seed(r)
  CV.ridge[[r]] <- cv.glmnet(x = df[train[,r], 1:17], y = df[train[,r],20],
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
  CV.Enet[[r]] <- cv.glmnet(x = df[train[,r], 1:17], y = df[train[,r], 20],
                            nfolds = 10, alpha = 0.5)
  end_time <- Sys.time()
  
  print(r)
  print(end_time - start_time)
}


# ridge
MOD.ridge <- list()

for(r in 1:100){
  start_time <- Sys.time()
  MOD.ridge[[r]] <- glmnet(x = df[train[,r], 1:17], y = df[train[,r], 20],
                           alpha = 0, lambda = CV.ridge[[r]]$lambda.min)
  end_time <- Sys.time()
  
  print(r)
  print(end_time - start_time)
}



# Enet
MOD.Enet <- list()

for(r in 1:100){
  start_time <- Sys.time()
  MOD.Enet[[r]] <- glmnet(x = df[train[,r], 1:17], y = df[train[,r], 20],
                          alpha = 0.5, lambda = CV.Enet[[r]]$lambda.min)
  end_time <- Sys.time()
  
  print(r)
  print(end_time - start_time)
}



# ridge
Beta.ridge <- matrix(NA, 100, ncol(df[, 1:17]))

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
Selection.Enet = matrix(NA, ncol(df[,1:17]), 8)
rownames(Selection.Enet) <- colnames(df[,1:17])
colnames(Selection.Enet) <- c("#", "Min", "Q1", "Med", "Q3", "Max", "Mean", "SD")

# Selection Counts
Selection.Enet[,1] <- colSums(abs(sign(Beta.Enet)))

#Quantile
for(p in 1:ncol(df[, 1:17])){
  Selection.Enet[p, 2:6] <- quantile(Beta.Enet[Beta.Enet[,p] !=0,p])
}

# Mean
for(p in 1:ncol(df[,1:17])){
  Selection.Enet[p,7] <- mean(Beta.Enet[Beta.Enet[,p] !=0, p])
}

# SD
for(p in 1:ncol(df[, 1:17])){
  Selection.Enet[p, 8] <- sd(Beta.Enet[Beta.Enet[,p] !=0, p])
}

Selection.Enet[is.na(Selection.Enet)] <- 0
Selection.Enet[is.nan(Selection.Enet)] <- 0
round(Selection.Enet, 3)


# RMSE
library(caret)
class(df)


Pred.Enet = list()
for(r in 1:100){
  Pred.Enet[[r]] <- predict(MOD.Enet[[r]], newx = df[-train[,r], 1:17], 
                            type = "response")
}

RMSE.Enet = c()
for(r in 1:100){
  RMSE.Enet[r] <- RMSE(Pred.Enet[[r]], df[-train[,r], 20])
}

summary(RMSE.Enet)
sd(RMSE.Enet)


