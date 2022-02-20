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

#Create the objective function for optimization calls
#The parameter gamma and the penalty parameter C1, C2 are encoded in a vector as the position of the particle of the PSO
wellsvm_obj <- function(param) {
  # construct wellsvm model
  wellsvm<-WellSVM(x,factor(train_set$Degree),x_u,C1 =param[0], C2=param[1],gamma =[2],x_center=FALSE,scale=FALSE) 
  # Get the classification accuracy of thr model
  socre<-measure_accuracy(wellsvm, x,factor(train_set$Degree),x_u,x2,factor(test_set$Degree))
  return socre
}

# Get the optimal parameter including gamma, C1, C2 
library(pso)
set.seed(45642)
pso_res <- psoptim(par = c(0, 0,0), fn = wellsvm_obj,
                   lower = c(0,0,0), upper = c(4, 4,10),
                   control = list(maxit = ceiling(num_mods/12)))
paores



