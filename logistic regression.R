dataset=read.csv('Social_Network_Ads.csv')
dataset=dataset[,3:5]
#data splittibg
library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio =0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
#feature scaling
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])
#fitting logistic regression
classifier=glm(formula = Purchased ~., family = binomial, data=training_set)
#predicting test set result
prob_pred=predict(classifier,type='response',newdata=test_set[-3])
y_pred=ifelse(prob_pred>0.5,1,0)
#confusion matrix
cm=table(test_set[,3],y_pred)
library(ElemStatLearn)
set=training_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,1])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c('Age','EstimatedSalary')
prob_set=predict(classifier,type='response',newdata=grid_set)
y_grid=ifelse(prob_set>0.5,1,0)
plot(set[,-3],main='logistic regression',xlab='Age',ylab='Salary',xlim=range(X1),ylim=range(X2))
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
prob_set=predict(classifier,type='response',newdata=grid_set)
y_grid=ifelse(prob_set>0.5,1,0)
plot(set[,-3],main='logistic regression',xlab='Age',ylab='Salary',xlim=range(X1),ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'green','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'black','white'))
