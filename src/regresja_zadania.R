library(ggplot2)

set.seed(23)

inTraining<-createDataPartition(y=diamonds$price,p = .7,list = FALSE)

training<-diamonds[inTraining,]
testing<-diamonds[~inTraining,]

ctrl<-trainControl()

fit_reg<-train(price ~ .,data=training,method="pls",trControl=ctrl,ntree=10)

