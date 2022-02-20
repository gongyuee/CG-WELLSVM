library(dplyr,warn.conflicts = FALSE)
library(ggplot2,warn.conflicts = FALSE)
library(RSSL)
library(pROC)
library(e1071)
library(Metrics)
#Import label data file
unlabel<-read.table(labelfile,header=T,sep=",",na.strings = c("NA"))
#Import unlabel data file
label<-read.table(unlabelfile,header=T,sep=",",na.strings = c("NA"))

#Randomly select training data and test data
set.seed(20200711)
train_sub <- sample(nrow(label),0.7*nrow(label))
train_set <- label[train_sub,]
test_set <- label[-train_sub,]


# Get  the characteristic data  of the training data and testing data
x <- as.matrix(train_set[,-c(1,14,15)])
x_u<-as.matrix(unlabel[,-c(13,14)])
x2<-as.matrix(test_set[,-c(1,14,15)])

#Data standardization
x<-scale(x)
x_u<-scale(x_u)
x2<-scale(x2)

#Get the label category of the training data
y_train<- factor(train_set$Degree)

#Construct WELLSVM model using training data
#param C1 double; A regularization parameter for labeled data, default 1;
#param C2 double; A regularization parameter for unlabeled data, default 0.1;
#param gamma double; Gaussian kernel parameter, i.e., k(x,y) = exp(-gamma^2||x-y||^2/avg) where avg is the average distance among instances; when gamma = 0, linear kernel is used. default gamma = 1;
#inheritParams BaseClassifier
wellsvm<-WellSVM(x,factor(train_set$Degree),x_u,C1 =1, C2=0.01,gamma =1.7,x_center=FALSE,scale=FALSE)


#Get the label category of the training data and testing data
train_set$Degree<-as.factor(train_set$Degree)
test_set$Degree<-as.factor(test_set$Degree)


#Predict the possibility to waterlogging for testing sites and Get testing evaluation results
wellsvm.test.pre <-as.numeric(predict(wellsvm,x2))
table(wellsvm.test.pre,test_set$Degree)
classAgreement(table(wellsvm.test.pre,test_set$Degree))
roc(test_set$Degree,wellsvm.test.pre)

rmse(x2,wellsvm.test.pre)

#Predict the possibility to waterlogging for training sites and Get test evaluation results
wellsvm.train.pre <-as.numeric(predict(wellsvm,x))
table(wellsvm.train.pre,train_set$Degree)
classAgreement(table(wellsvm.train.pre,train_set$Degree))
roc(train_set$Degree,wellsvm.train.pre)
rmse(x,wellsvm.train.pre)

