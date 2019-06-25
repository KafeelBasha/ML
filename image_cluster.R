library(keras)
path="D:\\ML\\Clustering\\Data\\py_images"
#Produce vector of file names or directories
#full.names: If TRUE, the directory path is prepended to the file names
#Recursive: Look into sub folders
images=list.files(path,recursive=TRUE,pattern=".png",full.names=TRUE)
head(images)
length(images)

readimg<-function(files)
{
  pixels<-image_to_array(image_load(files,grayscale=TRUE)) #default RGB 
}
data=as.data.frame(sapply(images,readimg))
class(data)
#sapply() arranges values in column wise
dim(data) #90x100

#Transform the pixels into a range between [0,1]
#Min-Max Scaler
minmax<-function(x)
{
  (x-min(x))/(max(x)-min(x))
}
data=as.data.frame(lapply(data,minmax))
data=t(data) #Columns to rows
dim(data)
View(head(data)[,1:5])

#Kmeans
set.seed(1234)
km.fit=kmeans(data,4,nstart=25,iter.max=25)

#Image clusters
library(dplyr)
image_clusters=data.frame(cluster=km.fit$cluster,img_id=gsub("D..ML.Clustering.Data.py_images.","",names(km.fit$cluster)),row.names=NULL)
image_clusters%>%filter(cluster==1)->class1
class1
image_clusters%>%filter(cluster==2)->class2
class2
image_clusters%>%filter(cluster==3)->class3
class3
image_clusters%>%filter(cluster==4)->class4
class4