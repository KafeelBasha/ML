#Working directory
setwd("D:\\ML\\Logistic Regression\\Data")

library(glmnet)

#Data Import
reviews=read.table("movie_reviews.tsv",sep="\t",header=TRUE)
dim(reviews)
names(reviews)

colSums(is.na(reviews))
unique(reviews$sentiment)

library(tm)
demo_text=c("This is sentence one.", "This is sentence two.", "This is a very very long sentence three.")


#Converting text to corpus
corp=Corpus(VectorSource(demo_text))

#Checking Head
inspect(corp)
corp=tm_map(corp,content_transformer(tolower))

#Stem document(offer/offered/offering)
corp=tm_map(corp,stemDocument)
corp
#terms are rows and documents columns
tdm<- TermDocumentMatrix(corp)
inspect(tdm)

#Transpose
tdm=t(tdm)
inspect(tdm)

#Build a Text Corpus of Jokes
reviews$review[[1]]
moviereview<-Corpus(VectorSource(reviews$review))
inspect(moviereview[1]) #Inspecting element in Corpus

#Data Transformations -Cleaning
moviereview<-tm_map(moviereview,tolower) #Converting to lower case
moviereview<-tm_map(moviereview,stripWhitespace) #Removing extra white space
moviereview<-tm_map(moviereview,removePunctuation) #Removing punctuations
moviereview<-tm_map(moviereview,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'available') #Can add more words apart from standard list(a/an/the/and/yet)
moviereview<-tm_map(moviereview,removeWords,my_stopwords)

#Stem document(Reducing words to their common root)
corp=tm_map(moviereview,stemDocument)
writeLines(as.character(moviereview[[33]]))

dtm <- TermDocumentMatrix(moviereview)
dim(dtm)
#Removing Sparse terms(large number of its entries are 0)
dtm=removeSparseTerms(dtm,0.95)
class(dtm)
dim(dtm)

#Transpose
dtm=t(dtm)
dim(dtm)

data=as.data.frame(as.matrix(dtm))
data$target=reviews$sentiment

#Split the data into train and test
index<- sort(sample(nrow(data),nrow(data)*0.8))
train<-data[index,]
test<-data[-index,]

which(names(data)=="target")
X_train=as.matrix(train[,-760])
y_train=train$target

y_test=test$target
X_test=as.matrix(test[,-760])
class(X_test)

#set.seed(350)
library(ROCR)

cv.lr=cv.glmnet(X_train,as.factor(y_train),alpha=1,family="binomial",nfolds=5)

lambda=cv.lr$lambda.1se

#Area under the curve
prob=predict(cv.lr,newx=X_test,s=lambda,type="response")

pred=prediction(prob,y_test)
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))
auc
