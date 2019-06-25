#Gradient Descent
#Initialize beta vector with some random values
#Compute the gradient at a given value of beta
#Update the beta vector 

#Do steps 2 and 3 till a feasible solution is obtained.

gradient_descent<-function(X,y,n,eta)
{
  beta=rep(1,ncol(X))
  cost_history=1:n
  converged=FALSE
  for(i in 1:n)
  {
    if(converged)
    {
      break
    }
    y_hat=X%*%beta
    Error=y-y_hat
    J=sum(Error^2)
    Gradient=-2*(t(X)%*%Error)
    beta=beta-eta*Gradient
    cost_history[i]=J
    if(i>1)
    {
      if(abs(cost_history[i]-cost_history[i-1])<=0.0003)
      {
        converged=TRUE
      }
    }
  }
  return(list(Converged=converged,Coefficients=beta,Final_Cost=J,Initial_Cost=cost_history[1],rss=cost_history))
}

#Data Import
setwd("D:\\ML\\Linear Regression\\Data")

Reg<-read.csv("regression.csv")
head(Reg)

Y=Reg$y

Reg$x0<- 1

head(Reg)

X=as.matrix(Reg[,c("x0","x1")])

gd=gradient_descent(X=X,y=Y,n=100000,eta=0.000001)

p=ggplot(data=cost_hist,aes(x=1:100000,y=rss))+geom_line()+ylim(min=0.1*1e7,max=1e7)+scale_x_continuous(limits = c(0,15000))

#Plotting 

#Stochastic Gradient descent

stochastic_gradient_descent<-function(data,x_cols,y_col,n,eta,p)
{
  beta=rep(0,ncol(X))
  cost_history=1:n
  converged=FALSE
  for(i in 1:n)
  {
    if(converged)
    {
      break
    }
    index<-sample(1:nrow(data),p*nrow(data),replace = F)
    X=data[index,x_cols]
    X=as.matrix(X)
    y=data[index,y_col]
    y_hat=X%*%beta
    Error=y-y_hat
    J=sum(Error^2)
    Gradient=-2*(t(X)%*%Error)
    beta=beta-eta*Gradient
    cost_history[i]=J
    if(i>1)
    {
      if(abs(cost_history[i]-cost_history[i-1])<=0.0003)
      {
        converged=TRUE
      }
    }
  }
  return(list(Converged=converged,Coefficients=beta,Final_Cost=J,Initial_Cost=cost_history[1]))
}

stochastic_gradient_descent(data = Reg,x_cols = c("x0","x1"),y_col = "y",n = 100000,eta = 0.000001,p = 0.6)