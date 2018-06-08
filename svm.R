#SVM
dataset=read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio=0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
#feature scaling
test_set[,1:2]=scale(test_set[,1:2])
training_set[,1:2]=scale(training_set[,1:2])


library(e1071)
#fitting SVM model AND PREDICTING RESULT SIMULTANEOUSLY
classifier=svm(formula=Purchased~.,data=training_set,type='C-classification',kernel='linear')
y_pred=predict(classifier,newdata=test_set[-3])

#making confusion matrix
cm=table(test_set[,3],y_pred)
# install.packages('ElemStatLearn')
library(ElemStatLearn)
set=training_set
x1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
x2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
gridset=expand.grid(x1,x2)
#till this point grid will be created
colnames(gridset)=c('Age','EstimatedSalary')

y_grid= predict(classifier,gridset)
plot(set[,-3],main='SVM(TRAINING SET)',xlab='age',ylab='estimated salary',
     xlim=range(x1),ylim=range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add=TRUE)
points(gridset,pch='.',col=ifelse(y_grid==1,'springgreen','tomato'))
points(set,pch=21,bg=ifelse(set[,3],'white','black'))
#visualising test set result
library(ElemStatLearn)
set=test_set
x1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
x2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
gridset=expand.grid(x1,x2)
#till this point grid will be created
colnames(gridset)=c('Age','EstimatedSalary')

y_grid= knn(train=training_set[-3],test=gridset,cl=training_set[,3],k=5)
plot(set[,-3],main='SVM(Test SET)',xlab='age',ylab='estimated salary',
     xlim=range(x1),ylim=range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add=TRUE)
points(gridset,pch='.',col=ifelse(y_grid==1,'springgreen','tomato'))
points(set,pch=21,bg=ifelse(set[,3],'white','black'))