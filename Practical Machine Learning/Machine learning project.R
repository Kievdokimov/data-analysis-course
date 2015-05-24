
fileUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileUrl, destfile="./train.csv")
data_tr<-read.csv("train.csv")

fileUrl<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileUrl, destfile="./test.csv")
data_t<-read.csv("test.csv")

data_tr$classe <- as.factor(data_tr$classe)
data_short <- data_tr[,(colSums(is.na(data_tr)) < 1000)&(colSums(data_tr=="")<1000)]
data_t_short<-data_t[,(colSums(is.na(data_t)) < 1000)&(colSums(data_t=="")<1000)]

library(caret)
library(randomForest)
set.seed(2233)

folds<-createResample(y=data_short$classe,times=10,list=F)

for(i in 1:10){
partition <- createDataPartition(y=folds[,i], p=0.7, list=FALSE)
train <- data_short[partition,8:60]
test <- data_short[-partition,8:60]

model1 <- randomForest(classe~., data=train)
prediction<-predict(model1,newdata=test)
accuracy[i]<-sum(prediction==test$classe)/nrow(test)

}

mean(accuracy)

prediction2<-predict(model1,newdata=data_t)

