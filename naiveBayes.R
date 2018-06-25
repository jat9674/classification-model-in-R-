dataset=read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]
#data encoding
dataset$Purchased=factor(dataset$Purchased,levels=c(1,0))
#data splitting
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio =0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
#feature scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])
#fitting classifier naive bayes
library(e1071)
classifier=naiveBayes(x=training_set[-3],y=training_set$Purchased)
#preictor
y_pred=predict(classifier,newdata=test_set[-3])
#confusion matrix
cm=table(y_pred,test_set[,3])
#visualising training set
library(ElemStatLearn)
set=training_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c('Age','Salary')
y_grid= predict(classifier,newdata=grid_set)
plot(set[,-3],main='naiveBayes training set',xlab='Age',ylab='Salary',xlim=range(X1),ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'green','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'black','white'))
#visualising test set
library(ElemStatLearn)
set=test_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c('Age','Salary')
y_grid= predict(classifier,newdata=grid_set)
plot(set[,-3],main='naiveBayes test set',xlab='Age',ylab='Salary',xlim=range(X1),ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'green','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'black','white'))

