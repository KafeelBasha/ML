#Import Data
setwd("D:\\ML\\Linear Regression\\R codes")

data=read.csv("D:\\ML\\Linear Regression\\Data\\OnlineNewsPopularity.csv")
str(data)
data=data[,-1]
dim(data)

summary(data)

#Predict Number of shares for each article
#Split the data into train and test
library(caret)
index<- sort(sample(nrow(data),nrow(data)*0.8))
train<-data[index,]
test<-data[-index,]

X_train=train[,-60]
y_train=train$shares

shares=test[,"shares"]
X_test=test[,-60]
dim(X_train)
dim(X_test)

library(glmnet)
#Build a Linear Regression model with Regularization
#??=1 the lasso and ??=0 the ridge.
#Lasso Model
control=trainControl(method="cv",number=5)
grid_lasso=data.frame(alpha=rep(1,121),lambda=seq(0,60,by=0.5))
lasso=train(X_train,y_train,method="glmnet",preProcess=c("center","scale"),trControl=control,tuneGrid=grid_lasso)
lasso$finalModel$lambdaOpt

#gsl_lambda=data.frame(alpha=1,lambda=24.5)


#Ridge Model
grid_ridge=data.frame(alpha=rep(0,121),lambda=seq(60,120,by=0.5))
ridge=train(X_train,y_train,method="glmnet",preProcess=c("center","scale"),trControl=control,tuneGrid=grid_ridge)
ridge$finalModel$lambdaOpt

#Normalizing X_test
X_test<- as.data.frame(scale(X_test))
class(X_test)
test1=cbind(X_test,shares)

#Prediction on Test data
plasso=predict(lasso,test1)
pridge=predict(ridge,test1)

#Mean squared error
mselasso=mean((plasso-test1$shares)^2)
mseridge=mean((pridge-test1$shares)^2)
mselasso
mseridge

