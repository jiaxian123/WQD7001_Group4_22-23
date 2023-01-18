library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(ggrepel)
library(knitr)

df <- read.csv(file = '/Users/yapjiaxian/Downloads/bodyPerformance.csv')
#Remane Column
names(df)[5] = 'bodyfat_per'
names(df)[9] = 'sitbend'
names(df)[10] = 'situp'
names(df)[11] = 'boardjump'

df$age[is.na(df$age)]<-mean(df$age,na.rm=TRUE)
df <- na.omit(df)


df <- df %>% 
  mutate(gender = case_when(
    as.character(gender) %in% c("Male", "Man","MALE") ~ "M",
    as.character(gender) %in% c("Women", "Female", "FEMALE") ~ "F",
    TRUE ~ as.character(gender)
  )
  )

df$gender[df$gender == 'M'] <- 1
df$gender[df$gender == 'F'] <- 2

df$class[df$class == 'A']  <- 1
df$class[df$class == 'B']  <- 2
df$class[df$class == 'C']  <- 3
df$class[df$class == 'D']  <- 4

df <- subset(df,gender=='M' | gender=='F')
df = subset(df, select = -c(class,gender) )
df <- df %>% distinct()


clearOutlier <- function(x){
  quartiles <- quantile(x, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(x)
  
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR 
  
  df <- subset(df, x > Lower & x < Upper)
  
}

df <- clearOutlier(df$height_cm)
df <- clearOutlier(df$bodyfat_per)
df <- clearOutlier(df$weight_kg)
df <- clearOutlier(df$systolic)
df <- clearOutlier(df$gripForce)
df <- clearOutlier(df$sitbend)
df <- clearOutlier(df$situp)
df <- clearOutlier(df$boardjump)
df <- clearOutlier(df$diastolic)


df <- read.csv(file = '/Users/yapjiaxian/Downloads/new_data.csv')
df = subset(df, select = -c(X) )
corr <- round(cor(df), 3)

ggcorrplot(corr)

df$gender[df$gender == 1] <- "M"
df$gender[df$gender == 2] <- "F"
df$class[df$class == 1]  <- "A"
df$class[df$class == 2]  <- "B"
df$class[df$class == 3]  <- "C"
df$class[df$class == 4]  <- "D"

p <- ggplot(data = df, aes(x = height_cm,fill=gender,color='black' )) + geom_histogram(bins = 15,alpha=0.6,position='identity')

p + facet_wrap(~class)

p1 <- ggplot(data = df, aes(x = age,fill=gender ,color='black')) + geom_histogram(bins = 15,alpha=0.6,position='identity')

p1 + facet_wrap(~class)

p2 <- ggplot(data = df, aes(x = weight_kg,fill=gender ,color='black')) + geom_histogram(bins = 15,alpha=0.6,position='identity')

p2 + facet_wrap(~class)

ggplot(df, aes(age, situp, fill= class)) + 
  geom_point(shape=23)+ggtitle("Sit-ups density on age" )+theme(plot.title = element_text(hjust = 0.5))+ theme_classic()

ggplot(df, aes(age, boardjump, fill= class)) + 
  geom_point(shape=23)+ggtitle("Board Jump density on age" )+theme(plot.title = element_text(hjust = 0.5))+theme_classic()

ggplot(df, aes(age, sitbend, fill= class)) + 
  geom_point(shape=23)+ggtitle("Sit Bend density on age" )+theme(plot.title = element_text(hjust = 0.5))+theme_classic()

df1<-aggregate(df[,9], list(df$class), mean)

ggplot(data=df1, aes(x=Group.1, y=x, group=1,label = x)) +
  geom_line()+
  geom_point(color='blue',size=5) + 
  labs(title = 'Relationship between Sit&Bend and Class', x = 'Class',y = 'average sit and bend forward_cm') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')

df2<-aggregate(df[,10], list(df$class), mean)

ggplot(data=df2, aes(x=Group.1, y=x, group=1,label = x)) +
  geom_line()+
  geom_point(color='red',shape=22,size=5,fill='red') + 
  labs(title = 'Relationship between Sit-ups and Class', x = 'Class',y = 'average sit-ups count') + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')

df3<-aggregate(df[,11], list(df$class), mean)

ggplot(data=df3, aes(x=Group.1, y=x, group=1,label = x)) +
  geom_line()+
  geom_point(color='green',shape=24,size=5,fill='green') + 
  labs(title = 'Relationship between Board Jump and Class', x = 'Class',y = 'average Board jump cm') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')

p4 <- ggplot(data = df, aes(x = age,y = after_stat(density),fill=class)) + geom_histogram(bins = 15,color='black',alpha = 0.5,position='identity') + geom_density(alpha = 0.5, fill="yellow")

p4 + facet_wrap(~class)

df4<- aggregate(df[,1], list(df$class), mean)
colnames(df4)[2] = "value"

ggplot(data=df4, aes(x=Group.1, y=value, group=1,label = value)) +
  geom_line(aes(color='blah'))+
  geom_point(aes(color='blah')) + 
  geom_errorbar(aes(ymin=value-sd(value), ymax=value+sd(value),color='blah'), width=.2,position=position_dodge(0.05))+
  labs(title = 'Age and Class distribution', x = 'Class',y = 'Age') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(value,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_manual(values = c('blah' = '#E69F00'))

df$age_cat[df$age <=40] <- 'Younger'
df$age_cat[df$age >40] <- 'Older'

df5<- aggregate(df[,6], list(df$class,df$age_cat), mean)



ggplot(data=df5, aes(x=Group.1, y=x, group=Group.2,col=Group.2)) +
  geom_line(aes(color=Group.2))+
  geom_point(aes(color=Group.2)) + 
  geom_errorbar(aes(ymin=x-sd(x), ymax=x+sd(x),color=Group.2), width=.2,position=position_dodge(0.05))+
  labs(title = 'Diastolic blood pressure to Class affections', x = 'Class',y = 'Average Diastolic',fill="Age Group") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_discrete(name="Age Group")

df6<- aggregate(df[,7], list(df$class,df$age_cat), mean)



ggplot(data=df6, aes(x=Group.1, y=x, group=Group.2,col=Group.2)) +
  geom_line(aes(color=Group.2))+
  geom_point(aes(color=Group.2)) + 
  geom_errorbar(aes(ymin=x-sd(x), ymax=x+sd(x),color=Group.2), width=.2,position=position_dodge(0.05))+
  labs(title = 'Systolic blood pressure to Class affections', x = 'Class',y = 'Average Systolic',fill="Age Group") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_discrete(name="Age Group")

df$sys_cat[df$systolic >0 & df$systolic <=120] <- 'Category 1'
df$sys_cat[df$systolic >120 & df$systolic <=129] <- 'Category 2'
df$sys_cat[df$systolic >129 & df$systolic <=139] <- 'Category 3'
df$sys_cat[df$systolic >130 & df$systolic <=180] <- 'Category 4'

df$dis_cat[df$diastolic >40 & df$diastolic <=60] <- 'Category 1'
df$dis_cat[df$diastolic >60 & df$diastolic <=80] <- 'Category 2'
df$dis_cat[df$diastolic >80] <- 'Category 3'

df7<-aggregate(df[,1], list(df$dis_cat), mean)
ggplot(data=df7, aes(x=Group.1, y=x, group=1 ,label=x)) +
  geom_line()+
  geom_point(color='#00AFBB',shape=15,size=5,fill='#00AFBB') + 
  labs(title = 'Diastolic blood pressure and Age differences', x = 'Systolic blood pressure Category',y = 'Average Age') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,0)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  annotate("text", x=1.5, y=38, label= "category 1 = more than 40, less then 60 \n category 2 = more than 60, less then 80 \n category 3 = more than 80")

df8<-aggregate(df[,1], list(df$sys_cat), mean)
ggplot(data=df8, aes(x=Group.1, y=x, group=1 ,label=x)) +
  geom_line()+
  geom_point(color='#CC79A7',shape=15,size=5,fill='#CC79A7') + 
  labs(title = 'Systolic blood pressure and Age differences', x = 'Systolic blood pressure Category',y = 'Average Age') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,0)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')+
  annotate("text", x=1.5, y=38, label= "category 1 = more than 0, less then 120 \n category 2 = more than 120, less then 129 \n category 3 = more than 129, less than 139 \n category 4 = more than 139")


df9<-aggregate(df[,5], list(df$sys_cat), mean)
ggplot(data=df9, aes(x=Group.1, y=x, group=1 ,label=x)) +
  geom_line()+
  geom_point(color='#CC79A7',shape=15,size=5,fill='#CC79A7') + 
  labs(title = 'Systolic blood pressure and Body Fat% differences', x = 'Systolic blood pressure Category',y = 'Average Body Fat%') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')

df10<-aggregate(df[,5], list(df$dis_cat), mean)
ggplot(data=df10, aes(x=Group.1, y=x, group=1 ,label=x)) +
  geom_line()+
  geom_point(color='#CC79A7',shape=15,size=5,fill='#CC79A7') + 
  labs(title = 'Diastolic blood pressure and Body Fat% differences', x = 'Diastolic blood pressure Category',y = 'Average Body Fat%') +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_label_repel(aes(label = round(x,2)),box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50')

ggplot(df, aes(x=age_cat, y=gripForce,fill=age_cat)) + 
  geom_boxplot()


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

#NB True 54.79%%

numCores <- detectCores()
cl <- makePSOCKcluster(numCores)
registerDoParallel(cl)



df <- read_csv('new_dataset-4.csv')
df$class <- factor(c(df$class))
parts = createDataPartition(df$class, p = 0.8, list = F)
train = df[parts, ]
test = df[-parts, ]

fitControl <- trainControl(method = "repeatedcv",
                           number = 100,
                           repeats=10,
                           search = 'random')


x = train[,1:10]
y= train$class

model_dt = train(class ~ ., data=df, method='nb',  trControl = fitControl,metric = "accuracy")
stopCluster(cl)

saveRDS(model_dt, file = "model_nb.rda")

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
# Loading package
library(e1071)
library(caTools)

df <- read_csv('new_dataset-4.csv')
df$class <- factor(c(df$class))
parts = createDataPartition(df$class, p = 0.8, list = F)
train_cl = df[parts, ]
test_cl = df[-parts, ]




classifier_knn <- knn(train = train_cl,
                      test = test_cl,
                      cl = train_cl$class,
                      k = 113)

classifier_knn
cm <- table(test_cl$class, classifier_knn)
cm
misClassError <- mean(classifier_knn != test_cl$class)
print(paste('Accuracy =', 1-misClassError))













