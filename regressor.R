setwd("D:\\ML\\Decision Tree\\data")

#dm Import
dm=read.csv("dm.csv")
dim(dm)
head(dm)

#Character variables as factors
str(dm)

#Removing 'Cust_Id" column
dm=dm[,-11]

#Split the dm into train and test
index<- sort(sample(nrow(dm),nrow(dm)*0.8))
train<-dm[index,]
test<-dm[-index,]

which(names(train)=="AmountSpent")
X_train=train[,-10]
y_train=train$AmountSpent

y_test=test$AmountSpent
X_test=test[,-10]
class(X_test)

library(caret)
library(rpart)
set.seed(1234)
control=trainControl(method='cv',number=10)
grid=data.frame(maxdepth=c(3:15))
cv_rt=train(X_train,y_train,method="rpart2",trControl=control,tuneGrid=grid)
cv_rt

control=trainControl(method='cv',number=10)
grid=data.frame(cp=c(0.02:0.1))
cv_rt=train(X_train,y_train,method="rpart",trControl=control,tuneGrid=grid,maxdepth=10)
cv_rt

cv_rtm=train(X_train,y_train,method="rpart",control=rpart.control(cp=0.02,maxdepth=10))

#Prediction and Accuracy
predicted=predict(cv_rtm,X_test)

#RMSE
RMSE=sqrt(mean((y_test-predicted)^2))
RMSE

library(DMwR)
regr.eval(y_test,predicted)

#Variable Importance
importance=varImp(cv_rtm,scale=FALSE)
importance

library(rattle)
fancyRpartPlot(cv_rtm$finalModel)
