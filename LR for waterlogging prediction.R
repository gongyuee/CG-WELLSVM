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

#Construct LR model using training data
x$Degree<-as.factor(train_set$Degree)
LR<-glm(Degree ~  RiverDistance+RoadDistance+twi+spi+slope+aspect+Curvate+LAI+Imper+x2016day+x2016all+elevation,
        data =x ,family=binomial)
#Predict the possibility to waterlogging for testing sites and Get testing evaluation results
LR.test.pre =as.numeric(predict(LR,type="response",newdata=x2)>0.5)
table(LR.test.pre,x2$Degree)
classAgreement(table(LR.test.pre,x2$Degree))
roc(test_set$Degree,LR.test.pre)

#Predict the possibility to waterlogging for training sites and Get test evaluation results
LR.train.pre =as.numeric(predict(LR,type="response",newdata=x)>0.5)
table(LR.train.pre,x$Degree)
classAgreement(table(LR.train.pre,x$Degree))
roc(train_set$Degree,LR.train.pre)


