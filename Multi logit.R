setwd("D:\\ML\\Logistic Regression\\Data")

pvalues=read.csv("mnist_x.csv",header=TRUE)
ilabels=read.csv("mnist_y.csv",header=FALSE)
head(pvalues)

#Normalize the pixel values
pvalues=pvalues/255

x=pvalues
names(ilabels)="labels"
y=ilabels
head(y)

data=cbind(x,y)
dim(data)
#Split the data into train and test
index<- sort(sample(nrow(data),nrow(data)*0.8))
train<-data[index,]
test<-data[-index,]

which(names(train)=="labels")
X_train=as.matrix(train[,-65])
y_train=train$labels

y_test=test$labels
X_test=as.matrix(test[,-65])
class(X_test)

set.seed(350)
library(glmnet)
cv.mlr=cv.glmnet(X_train,as.factor(y_train),family="multinomial",alpha=0,nfolds=5)

lambda=cv.mlr$lambda.1se
lambda

#Predict class
prob=predict(cv.mlr,newx=X_test,s=lambda,type="class")
head(prob)

#Confusion Matrix and Missclassification
table(prob,y_test)

mean(as.character(prob) != as.character(y_test))

y_data=as.factor(data$Species)
#Accuracy
sum(diag(table(prob,y_test)))/nrow(X_test)


