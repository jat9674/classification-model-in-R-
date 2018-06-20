dataset=read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]
#encoding target feature as factor
datset$Purchased=factor(dataset$Purchased,levels = c(0,1))
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
#feature scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])
#predictor and classifier together in knn
library(class)
y_pred=knn(train=training_set[,1:2],test=test_set[,1:2],cl=training_set[,3],k=5)
#confusion matrix
cm=table(y_pred,test_set[,3])
#visualising training set
library(ElemStatLearn)
set=training_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,1])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c('Age','EstimatedSalary')
 
y_grid=knn(train=training_set[,1:2],test=grid_set,cl=training_set[,3],k=5)
plot(set[,-3],main='KNN training set',xlab='Age',ylab='Salary',xlim=range(X1),ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'green','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'black','white'))
#test set graph
library(ElemStatLearn)
set=test_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,1])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c('Age','EstimatedSalary')

y_grid=knn(train=training_set[,1:2],test=grid_set,cl=training_set[,3],k=5)
plot(set[,-3],main='KNN test set',xlab='Age',ylab='Salary',xlim=range(X1),ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'green','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'black','white'))