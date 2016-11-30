library(caret)
library(C50)
library(data.table)
data(churn)
set.seed(23)
ctrl<-trainControl()
fit<-train(churn ~ .,data=churnTrain,method="rf",trControl=ctrl,tree=10)

