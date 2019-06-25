setwd("D:\\ML\\Clustering\\Data")
hdata=read.csv("kc_housingsample.csv")
str(hdata)

#Choose columns that are numeric and have numeric interpretation
data=hdata[,c("price","bedrooms","bathrooms","sqft_living")]
str(data)

#Scaling using base R
scaled=data.frame(scale(data)) 
head(scaled)
dim(scaled)

#Kmeans algorithm
#R uses iter.max=10 as default,it may take more than 10 iterations to converge.
#Convergence: clusters formed in the current iteration are the same as those obtained
#in the previous iteration.
km.fit=kmeans(scaled,10,iter.max=20,nstart=25)
#summary(km.fit)

km.fit$size #No of observations in each cluster
km.fit$withinss #Within sum of squares metric for each cluster
km.fit$totss #The total sum of squares
#km.fit$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
#km.fit$betweenss

#Cluster Centers and labels
km.fit$centers
km.fit$cluster

#Compute optimal Number of clusters
#Elbow Method
set.seed(200)
wss<-1:15
number<-1:15

for (i in 1:15)
{
  wss[i]<-kmeans(scaled,i,iter.max=20,nstart=25)$tot.withinss
}

plot(number,wss,main="Scree Plot",xlab="Number of clusters",ylab="Total Within sum of squares",type="b")

#Average Silhouette width for 2<=nclust<=n-1
library(cluster)
set.seed(200)
sil<-list()
for(j in 2:10){
  km.fit <- kmeans(scaled, j,iter.max=20,nstart=25)
  sw <- silhouette(km.fit$cluster, dist(scaled))#dist():"euclidean"
  sil[j] <- mean(sw[, 3])
}
class(sil)
Avg_width=unlist(sil)

#Optimal number of clusters
plot(x=2:10,Avg_width,type="b",pch=20,xlab="Number of Clusters",ylab="Average Silhouette Width")

#Compute kmeans with k=5
set.seed(200)
km.final=kmeans(scaled,5,iter.max=20,nstart=25)

#Cluster Profiling
pop_mean=sapply(data,mean) #Population mean
pop_sd=sapply(data,sd) #Population std
group_mean=aggregate(data,by=list(km.final$cluster),mean) #sample mean
group_mean
class(pop_mean)
class(pop_sd)
class(group_mean)
#z score
score=apply(group_mean[,-1],1,function(x){(x-t(pop_mean))/t(pop_sd)})
score
rownames(score)<-colnames(group_mean[-1])
colnames(score)<-row.names(group_mean)
z_score=data.frame(t(score))
z_score$size<-km.final$size
z_score
