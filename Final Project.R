library(caret)
library(party)
library(tree)

training<-read.csv("pml-training.csv")
testing<-read.csv("pml-testing.csv")


#cleaning the data
## Droppng the summary statistics variables taht include 
##"kurtosis_", "skewness_", "max_", "min_", "amplitude_", "var_", "avg_", and "stddev_" 

training<-training[,c(8:11,37:49,60:68,84:86,116:124,151:160)]
testing<-testing[,c(8:11,37:49,60:68,84:86,116:124,151:160)]


#Tree
mod.tree <- train(classe ~ .,method="rpart",data=training)

# Train set prediction
pred.train<-predict(mod.tree, training)
train.table<-table(predict=pred.train,truth = training$classe )
train.table
(train.table[1,1]+train.table[2,2]+train.table[3,3]
        +train.table[4,4]+train.table[5,5])/sum(train.table)

# Test set prediction
pred.test<-predict(mod.tree, testing)
pred.test

# Tree with pruning and cross validation
mod.tree2 <- tree(classe ~ .,data=training)
mod.tree.cv <- cv.tree(mod.tree2 ,FUN=prune.misclass )
mod.tree.cv$size
par(mfrow =c(1,2))
plot(mod.tree.cv$size ,mod.tree.cv$dev ,type="b")
plot(mod.tree.cv$k ,mod.tree.cv$dev ,type="b")

mod.tree.prune =prune.misclass (mod.tree2, best =18)

#Train set prediction
pred.train<-predict(mod.tree.prune, training,type="class")
train.table<-table(predict=pred.train,truth = training$classe )
train.table
(train.table[1,1]+train.table[2,2]+train.table[3,3]+train.table[4,4]+train.table[5,5])/sum(train.table)

# Test set prediction
pred.test<-predict(mod.tree.prune, testing, type="class")
pred.test

#bagging
predictors = training[-c(dim(training)[2])]
classe = training$classe
treebag <- bag(predictors, classe, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,predict 
                                       = ctreeBag$pred, aggregate 
                                       = ctreeBag$aggregate))

# Train set prediction
pred.train<-predict(treebag,predictors)
train.table<-table(predict=pred.train,truth = training$classe )
train.table
(train.table[1,1]+train.table[2,2]+train.table[3,3]
        +train.table[4,4]+train.table[5,5])/sum(train.table)

# Test set prediction
predictors.test<-testing[-c(dim(testing)[2])]
predictors.test[,35]<-as.numeric(predictors.test[,35])
predictors.test[,46]<-as.numeric(predictors.test[,46])
predictors.test[,47]<-as.numeric(predictors.test[,47])
pred.test<-predict(treebag,predictors.test)
pred.test


#Random forests
#mod.rf <- train(classe ~ .,method="rf",data=training)
library (randomForest)
mod.rf =randomForest(classe~.,data=training , importance =TRUE)


#Train set prediction
pred.train<-predict(mod.rf, training)
train.table<-table(predict=pred.train,truth = training$classe )
train.table
(train.table[1,1]+train.table[2,2]+train.table[3,3]+train.table[4,4]+train.table[5,5])/sum(train.table)

# Test set prediction
pred.test<-predict(mod.rf, testing)
pred.test


### Random forests with parallel method
#library(parallel)
#library(doParallel)
#cluster <- makeCluster(detectCores() - 1)
#registerDoParallel(cluster)
#fitControl <- trainControl(method = "cv",number = 5, allowParallel = TRUE)

#mod.rf2 <- train(classe ~ .,method="rf",data=training,trControl = fitControl)
#stopCluster(cluster)
#registerDoSEQ()

#Train set prediction
#pred.train<-predict(mod.rf2, training)
#train.table<-table(predict=pred.train,truth = training$classe )
#train.table
#(train.table[1,1]+train.table[2,2]+train.table[3,3]+train.table[4,4]+train.table[5,5])/sum(train.table)

# Test set prediction
#pred.test<-predict(mod.rf2, testing)
#pred.test

###
# Boosting
library (gbm)
#mod.boost <- train(classe ~ .,method="gbm",data=training)
mod.boost <- gbm(classe ~ .,data=training, distribution=
                         "gaussian",n.trees =5000 , interaction.depth =4)
summary (mod.boost)

# LDA
library (MASS)
mod.lda=lda(classe~. ,data=training)
pred.train<-predict(mod.lda , training)
train.table<-table(predict=pred.train$class,truth = training$classe )
train.table
(train.table[1,1]+train.table[2,2]+train.table[3,3]+train.table[4,4]+train.table[5,5])/sum(train.table)

# Test set prediction
pred.test<-predict(mod.lda , testing)
pred.test$class

# QDA
mod.qda=qda(classe~. ,data=training)
pred.train<-predict(mod.qda , training)
train.table<-table(predict=pred.train$class,truth = training$classe )
train.table
(train.table[1,1]+train.table[2,2]+train.table[3,3]+train.table[4,4]+train.table[5,5])/sum(train.table)

# Test set prediction
pred.test<-predict(mod.qda , testing)
pred.test$class
