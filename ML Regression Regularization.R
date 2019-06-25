setwd("D:\\ML\\Linear Regression\\Data")

adv=read.csv("Advertising.csv")

head(adv)

#Dropping row numbers
adv=adv[,-1]
head(adv)

library(caret)

Model1=train(Sales~TV,data=adv,method="lm")
Model2=train(Sales~Radio,data=adv,method="lm")
Model3=train(Sales~Newspaper,data=adv,method="lm")
Model4=train(Sales~TV+Radio,data=adv,method="lm")
Model5=train(Sales~TV+Newspaper,data=adv,method="lm")
Model6=train(Sales~Radio+Newspaper,data=adv,method="lm")
Model7=train(Sales~TV+Radio+Newspaper,data=adv,method="lm")

#RSS
One.variable=sum(resid(Model2)^2)
Two.variable=sum(resid(Model5)^2)
Three.variable=sum(resid(Model7)^2)

RSS=data.frame(With=c("One variable","Two variable","Three variable"),RSS=c(One.variable,Two.variable,Three.variable))

library(ggplot2)

RSS.bar=ggplot(data=RSS,aes(x=With,y=RSS,fill=With))
RSS.bar+geom_bar(stat='identity',position='dodge')+xlab("Number of Predictors")


#Regularization
library(glmnet)
library(caret)

setwd("D:\\ML\\Linear Regression\\Data")

adv=read.csv("Advertising.csv")
head(adv)

#Dropping row numbers
adv=adv[,-1]
head(adv)
x=as.matrix(adv[,-4])
y=adv[,"Sales"]


gs_lambda=data.frame(alpha=0,lambda=0)
Ridge=train(x,y,method="glmnet",tuneGrid=gs_lambda)

#??=1 the lasso and ??=0 the ridge.
lasso_lambda=data.frame(alpha=1,lambda=0.23)
lasso=train(x,y,method="glmnet",preProc=c('center','scale'),tuneGrid=lasso_lambda)

ridge_lambda=data.frame(alpha=0,lambda=0.23)
ridge=train(x,y,method="glmnet",preProc=c('center','scale'),tuneGrid=ridge_lambda)
ridge

enet_grid <-data.frame(alpha=0.4,lambda=0.23)
enet=train(x,y,method="glmnet",preProc=c('center','scale'),tuneGrid=enet_grid)
enet
