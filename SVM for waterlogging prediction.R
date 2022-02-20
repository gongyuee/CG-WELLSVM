library(dplyr,warn.conflicts = FALSE)
library(ggplot2,warn.conflicts = FALSE)
library(RSSL)
library(pROC)
library(e1071)

#Import label data file
label<-read.table(labelfile,header=T,sep=",",na.strings = c("NA"))

#Randomly select training data and test data
set.seed(20180808)
train_sub <- sample(nrow(label),0.7*nrow(label))
train_set <- label[train_sub,]
test_set <- label[-train_sub,]

# Get  the label category of the training data and test data
factor1 <- train_set[,-c(1)]
factor2<-test_set[,-c(1)]

#Data standardization
x<-transform(as.data.frame(scale(factor1)),Degree=train_set$Degree)
x2<-transform(as.data.frame(scale(factor2)),Degree=test_set$Degree)

#Construct SVM model using training data
x$Degree<-as.factor(train_set$Degree)
svm<-svm(Degree ~  RiverDistance+RoadDistance+twi+spi+slope+aspect+Curvate+LAI+Imper+x2016day+x2016all+elevation,
         data = x,
         kernel="radial",
         C=1,
         gamma=0.01,
         probability=TRUE
         )

#Get test evaluation results
x2$Degree<-as.factor(test_set$Degree)
svm.test.pre =as.numeric(predict(svm,x2))
table(svm.test.pre,x2$Degree)
classAgreement(table(svm.test.pre,x2$Degree))
roc(test_set[,-c(1,5)]$Degree,svm.test.pre)

#Get test evaluation results
svm.train.pre =as.numeric(predict(svm,x))
table(svm.train.pre,x$Degree)
classAgreement(table(svm.train.pre,x$Degree))
roc(train_set[,-c(1,5)]$Degree,svm.train.pre)


