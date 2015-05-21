library("caret")

## Data import and clean-up
# import the raw training set
training_raw <- read.csv2(file = "pml-training.csv",header = TRUE,sep=",")

# remove non-informative variables (NAs/empty)
training_raw <- training_raw[,apply(training_raw,2,function(x) {sum(is.na(x))})/nrow(training_raw)<0.95]
training_raw <- training_raw[,apply(training_raw,2,function(x) { sum(x == "")})/nrow(training_raw) < 0.95]

# remove columns associated with user name, recording time, index etc.
training_raw <- subset(training_raw, select=-c(1,2,3,4,5,6,7))

# convert columns to appropriate types
training_raw[,c(1,2,3,5,6,7,14,15,16,18,19,20,27,28,29,31,32,33,39,40,41,42,44,45,46,51,52)] <- lapply(training_raw[,c(1,2,3,5,6,7,14,15,16,18,19,20,27,28,29,31,32,33,39,40,41,42,44,45,46,51,52)],function(x) { as.numeric(as.character(x))})
training_raw$classe <- as.factor(training_raw$classe)

# split data into training and validation set
# set.seed(11235)
# inTrain <- createDataPartition(training_raw$classe,p=0.7,list=FALSE)
# training <- training_raw[inTrain,]
# validation <- training_raw[-inTrain,]

training <- training_raw

# use Principal Component Analysis to reduce the number of variables
pp <- preProcess(training[,-53],method="pca",thresh=0.95)
training_pc <- predict(pp,training[,-53])
training_pc$classe <- training_raw$classe
#validation_pc <- predict(pp,validation[,-53])

# use cross valdiation for the rpart algorithm to estimate out of sample error
set.seed(11235)
k <- 10
oosestim <- NULL
folds <- createFolds(training$classe,k)
for (i in 1:k) {
    train_rpart <- training_pc[folds[[i]],]
    test_rpart <- training_pc[-folds[[i]],]
    modelFit.rpart <- train(train_rpart$classe ~ ., method="rpart",data=train_rpart)
    oosestim <- c(oosestim,sum(predict(modelFit.rpart,test_rpart) == test_rpart$classe)/nrow(test_rpart))
    confusionMatrix(test_rpart$classe,predict(modelFit.rpart,test_rpart))
    
}
1- mean(oosestim)
print(modelFit.rpart$finalModel)

# modelFit.rpart <- train(x=training[,-53],y=training$classe,method="rpart",
#                         preProc=c("center","scale"),trControl=trainControl(
#                             method="repeatedcv",classProbs=TRUE,
#                             summaryFunction=twoClassSummary))

# maybe improve by using cross validation and removing bad predictors?!

modelFit.rf <- train(x=training_pc, y=training$classe ,method="rf",data=training_pc)
confusionMatrix(validation$classe,predict(modelFit.rf,validation_pc))



modelFit.gbm <- train(training$classe ~.,method="gbm",data=training_pc)
confusionMatrix(training$classe,predict(modelFit.gbm,training_pc))

modelFit.svm <- train(training$classe ~.,method="svmLinear",data=training_pc)
confusionMatrix(training$classe,predict(modelFit.gbm,training_pc))

modelFit.svm <- train(training$classe ~.,method="lssvmPoly",data=training_pc)
confusionMatrix(training$classe,predict(modelFit.gbm,training_pc))


modelFit.glm <- train(training$classe ~ .,method="glmnet",data=training_pc)

testing_raw <- read.csv2(file = "pml-testing.csv",header = TRUE,sep=",")
testing_raw <- testing_raw[,apply(testing_raw,2,function(x) {sum(is.na(x))})/nrow(testing_raw)<0.95]
testing_raw <- testing_raw[,apply(testing_raw,2,function(x) { sum(x == "")})/nrow(testing_raw) < 0.95]
testing_raw <- subset(testing_raw, select=-c(1,2,3,4,5,6,7))
testing_raw[,c(1,2,3,5,6,7,14,15,16,18,19,20,27,28,29,31,32,33,39,40,41,42,44,45,46,51,52)] <- 
    lapply(testing_raw[,c(1,2,3,5,6,7,14,15,16,18,19,20,27,28,29,31,32,33,39,40,41,42,44,45,46,51,52)],
           function(x) { as.numeric(as.character(x))})
testing <- predict(pp,testing_raw[,-53])
testing$problem_id <- testing_raw$problem_id
predict(modelFit.rf_cv,testing)

modelFit.rf_cv_raw <- train(classe ~.,method="rf",data=training_raw,trControl =
trainControl(method="cv",
number=5,
allowParallel=TRUE,
verboseIter = TRUE))
answers_full <- predict(modelFit.rf_cv_raw,testing)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

# #testing <- read.csv2(file = "pml-testing.csv",header = TRUE,sep=",")
# training_orig <- read.csv2(file = "pml-training.csv",header = TRUE,sep=",")
# # remove columns that have too many missing observations
# inTrain <- createDataPartition(training_orig$classe,
#                                p=0.7,list=FALSE)
# 
# training <- training_orig[inTrain,]
# #nearZeroVar(training,saveMetrics=TRUE)
# 
# testing <- training_orig[-inTrain,]
# 
# training <- training[,apply(training,2,function(x) { sum(is.na(x))})/nrow(training) < 0.95]
# training <- training[,apply(training,2,function(x) { sum(x == "")})/nrow(training) < 0.95]
# training[,7:59] <- lapply(training[,7:59],function(x) { as.numeric(as.character(x))})
# training <- training[,8:60]
# 
# validation <- validation[,apply(validation,2,function(x) { sum(is.na(x))})/nrow(validation) < 0.95]
# validation <- validation[,apply(validation,2,function(x) { sum(x == "")})/nrow(validation) < 0.95]
# validation[,7:59] <- lapply(validation[,7:59],function(x) { as.numeric(as.character(x))})
# validation <- validation[,8:60]
