library(caret)
library(MLmetrics)
library(rpart)
library(readr)
library(e1071) 
library(doParallel) 





#svm C36.70 60.92%

numCores <- detectCores()
cl <- makePSOCKcluster(numCores)
registerDoParallel(cl)



df <- read_csv('new_dataset-4.csv')
df$class <- factor(c(df$class))
parts = createDataPartition(df$class, p = 0.8, list = F)
train = df[parts, ]
test = df[-parts, ]

fitControl <- trainControl(method = "cv",
                           number = 10,
                           search = 'random')


x = train[,1:10]
y= train$class

model_lr = train(class ~ ., data=df, method='svmLinear',  trControl = fitControl,metric = "accuracy")
stopCluster(cl)

saveRDS(model_lr, file = "model_lr.rda")


#decision tree 0.00 66.66%

numCores <- detectCores()
cl <- makePSOCKcluster(numCores)
registerDoParallel(cl)



df <- read_csv('new_dataset-4.csv')
df$class <- factor(c(df$class))
parts = createDataPartition(df$class, p = 0.8, list = F)
train = df[parts, ]
test = df[-parts, ]

fitControl <- trainControl(method = "cv",
                           number = 10,
                           search = 'random')


x = train[,1:10]
y= train$class

model_dt = train(class ~ ., data=df, method='rpart',  trControl = fitControl,metric = "accuracy")
stopCluster(cl)
saveRDS(model_dt, file = "model_dt1.rds")

model_dt



#XGboost
numCores <- detectCores()
cl <- makePSOCKcluster(numCores)
registerDoParallel(cl)



df <- read_csv('new_dataset-4.csv')
df$class <- factor(c(df$class))
parts = createDataPartition(df$class, p = 0.8, list = F)
train = df[parts, ]
test = df[-parts, ]

fitControl <- trainControl(method = "cv",
                           number = 50,
                           search = 'random')


x = train[,1:10]
y= train$class

model_xgb_1 = train(class ~ ., data=df, method='xgbTree',  trControl = fitControl,metric = "accuracy")
stopCluster(cl)

model_xgb_1

saveRDS(model_xgb_1, file = "model_xgb_1.rds")




# KNN













