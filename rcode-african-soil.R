
library(e1071)
install.packages("caret")
library(caret)


train <- read.csv("F:/Kaggle/African Soil/training.csv",header=TRUE,stringsAsFactors=FALSE)
test <- read.csv("F:/Kaggle/African Soil/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)
#train <- read.csv("/media/jobil/OS/Flutura/Kaggle/Afsis_soil/training.csv",header=TRUE,stringsAsFactors=FALSE)
#test <- read.csv("/media/jobil/OS/Flutura/Kaggle/Afsis_soil/sorted_test.csv",header=TRUE,stringsAsFactors=FALSE)

submission <- test[,1]
labels <- train[,c("Ca","P","pH","SOC","Sand")]

#train <- train[,2:3579]
#test <- test[,2:3579]
train<-train[,2:3595]
test<-test[,2:3595]
a<-var(train)
head(a)
#removing highly correlated columns
tooHigh <- findCorrelation(cor(rbind(train[,2:3593],test[,2:3593])), .9999)
train<-train[,-tooHigh]
test<-test[,-tooHigh]

#Features variance
vardata<-""
r1<-""
for(i in 1:3594)
{
  r1<-cbind(names(train[i]),var(train[i]))
  #r1<-var(train[i])
  vardata<-rbind(vardata,r1)
}
write.csv(vardata,"F:/Kaggle/African Soil/variance.csv",row.names=FALSE,quote=FALSE)
var<-read.csv("F:/Kaggle/African Soil/variance.csv")
colnames(var)<-c("variable", "value")
head(var)
cutoff<-quantile(var$value, c(.75))
newdata <- var[ which(var$value>cutoff),]
nrow(newdata)
head(newdata)

##Building using SVM
svms <- lapply(1:ncol(labels),
               function(i)
               {
                 svm(train,labels[,i],cost=10000,scale=FALSE)
               })
predictions <- sapply(svms,predict,newdata=test)
colnames(predictions) <- c("Ca","P","pH","SOC","Sand")
submission <- cbind(PIDN=submission,predictions)



write.csv(submission,"F:/Kaggle/African Soil/beating_benchmark1.csv",row.names=FALSE,quote=FALSE)

