setwd("D:\\ML\\Decision Tree\\Data")

data=read.csv("credit_history.csv")
head(data)
colSums(is.na(data))
summary(data$years)

data$years[is.na(data$years)]=median(data$years,na.rm=TRUE)
str(data)

#Split the data into train and test
index<- sort(sample(nrow(data),nrow(data)*0.8))
train<-data[index,]
test<-data[-index,]

which(names(train)=="default")
X_train=train[,-1]
y_train=train$default

y_test=test$default
X_test=test[,-1]
class(X_test)

library(caret)
library(rattle)
set.seed(1234)
control <- trainControl(method = 'cv', number=6)
grid<- data.frame(cp=seq(0, 0.06, 0.001))
c_dt=train(X_train,as.factor(y_train),method="rpart",parms=list(split="gini"),tuneGrid=grid,maxdepth=3)

#Prediction and accuracy
prob=predict(c_dt,newdata=X_test,type="prob")

library(ROCR)
pred=prediction(prob[,2],y_test)
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))
auc

#Plotting
class(c_dt$finalModel)
fancyRpartPlot(c_dt$finalModel,sub="Decision Tree")

#Cross Validation
control=trainControl(method='cv',number=5)
grid=data.frame(maxdepth=seq(5,25,5))
cv_dt=train(X_train,as.factor(y_train),method="rpart2",trControl=control,tuneGrid=grid, control=rpart.control(minsplit = 10,cp=0.06,minbucket = 5))

#Prediction and accuracy
prob=predict(cv_dt,newdata=X_test,type="prob")

library(ROCR)
pred=prediction(prob[,2],y_test)
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))
auc
