setwd("D:\\ML\\Clustering\\Data")
data=read.csv("pollution_india_2010.csv",na.strings=c("Null"))
head(data)
colSums(is.na(data))
data=na.omit(data)
dim(data)

#Aggregate
head(data)
data=aggregate(data[,-c(1,5)],by=list(data$State),sum)
head(data)
data_num=data[,-1]
#names(data_num)

#Standardize the data
scaled=scale(data_num)
rownames(scaled)<-data$Group.1
head(scaled)

#Similarity Measures: Dissimilarity Matrix
dis.mat=dist(scaled) #default Euclidean
class(dis.mat)

#Linkage: Groups pairs of objects into clusters based on their similarity
set.seed(1234)
h_clust=hclust(dis.mat,method="ward.D2") #Minimum variance
plot(h_clust,main="Cluster Dendogram",xlab="Heirarchical clustering with Ward's criterion")

#Cluster profiling
pop_mean=sapply(data_num,mean) #Population mean
pop_sd=sapply(data_num,sd) #Population std
data_num$label=cutree(h_clust,4)
group_mean=aggregate(data_num,by=list(data_num$label),mean) #sample mean

#z score
score=apply(group_mean[,-c(1,5)],1,function(x){(x-t(pop_mean))/t(pop_sd)})
score
rownames(score)<-colnames(group_mean[-c(1,5)])
colnames(score)<-row.names(group_mean)
z_score=data.frame(t(score))
z_score$size<-as.numeric(table(cutree(h_clust,4)))
z_score
