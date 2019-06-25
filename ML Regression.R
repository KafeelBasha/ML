#Data Import
setwd("D:\\ML\\Linear Regression\\Data")

Reg<-read.csv("regression.csv")
head(Reg)

Y=Reg$y

Reg$x0<- 1

head(Reg)

X=as.matrix(Reg[,c("x0","x1")])

#OLS Solution with first principal Method
H=solve(t(X)%*%X)
x=t(X)%*%Y

#lets compute beta
beta=H%*%x
beta

#Predicted values can be calculated using computed betas and 'X'
Yp=X%*%beta

#RSS
RSS=sum((Yp-Y)^2)
RSS

#Instead of using first principle methods we can work with standard R API provided in 'caret' package

#contains functions to design the model training process for complex regression and classification problems. 

library(caret)

mod=train(X,Y,method="lm",tuneGrid = data.frame(intercept=F))
mod$finalModel$coefficients

#Working directly with our data file
mod1=train(y~x1,data=Reg,method="lm")
mod1$finalModel$coefficients
mod1

#RSS
residuals=mod1$finalModel$residuals
RSS=sum(residuals^2)
RSS