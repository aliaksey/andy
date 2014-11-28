rm(list=ls())
load("DATA/indam.RData")
load("DATA//andyshi.RData")
#creating data for hits
top<-as.data.frame(cbind(FeatureIdx=topanfeat,Class=rep("top")))
bot<-as.data.frame(cbind(FeatureIdx=botanfeat,Class=rep("bottom")))
middle<-as.data.frame(cbind(FeatureIdx=indam[!indam$FeatureIdx%in%c(topanfeat,
     botanfeat),"FeatureIdx"],Class=rep("middle")))
for_ips_model<-as.data.frame(rbind(top,bot,middle))
#merging with feature discriptors
featmodel<-read.csv(file ="DATA/indexes_and_features.csv")
ipshitsmodel_temp<-merge(for_ips_model,featmodel, 
                         by.x="FeatureIdx",by.y="feature.idx", all=F)
ipshitsmodel<-unique(ipshitsmodel_temp[,-c(1,3:14)])

ipshitsmodel<-ipshitsmodel[ipshitsmodel$Class!="middle",]
ipshitsmodel$Class<-factor(ipshitsmodel$Class)
##creating model
library(caret)
library(corrplot)
library(randomForest)
library(rpart)
library(partykit)
set.seed(28072013)
##selecting samples for training and testing
class_data<-ipshitsmodel[,1]
inTrain <- createDataPartition(class_data, p = 3/4, list = FALSE)
forTraining <- ipshitsmodel[inTrain,]
forTrainingX <- forTraining[, names(forTraining) != "Class"]
#reate testing set for features selection
forTesting <- ipshitsmodel[-inTrain,]
#############rpart analysis##########
rpart_training <- rpart(Class~.,  method="class", data=forTraining)
##polt as party object
rpart1a <- as.party(rpart_training)
plot(rpart1a, main="Classyfication tree for OCT4 positive colonies")
rpartPred <- predict(rpart_training, forTesting, type = "class")
confusionMatrix(rpartPred, forTesting$Class)
##tunning the model
cvCtrl <- trainControl(method = "repeatedcv", repeats = 3,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)
rpartTune <- train(Class ~ ., data = forTraining, method = "rpart",
                   tuneLength = 30,
                   metric = "ROC",
                   trControl = cvCtrl)
plot(rpartTune)
predictors(rpartTune)
plot(varImp(rpartTune),top = 20)
plot.train(rpartTune)
#print.train(rpartTune)
plot(rpartTune, scales = list(x = list(log = 10)))

rpartPred2 <- predict(rpartTune, forTesting)
confusionMatrix(rpartPred2, forTesting$Class)

rpartProbs <- predict(rpartTune, forTesting, type = "prob")
head(rpartProbs)
library(pROC)
rpartROC <- roc(forTesting$Class, rpartProbs[, "top"], 
                levels = rev(forTesting$Class))
plot(rpartROC, type = "S", print.thres = .5)
rpartROC
## cLCULTING MODEL BASED BY boosting TREE
grid <- expand.grid(.model = "tree",
                    .trials = c(1:100),
                    .winnow = FALSE)
c5Tune <- train(forTrainingX, forTraining$Class,
                method = "C5.0",
                metric = "ROC",
                tuneGrid = grid,
                trControl = cvCtrl)
c5Tune
plot(c5Tune)
c5Pred <- predict(c5Tune, forTesting)
confusionMatrix(c5Pred, forTesting$Class)
c5Probs <- predict(c5Tune, forTesting, type = "prob")
head(c5Probs)
c5ROC <- roc(predictor = c5Probs$top,
               response = forTesting$Class,
               levels = rev(levels(forTesting$Class)))
c5ROC
plot(rpartROC, type = "S")
plot(c5ROC, add = TRUE, col = "#9E0142")

histogram(~c5Probs$top|forTesting$Class, 
          xlab = "Probability of being bottom")
##svm
svmTune <- train(x = forTrainingX,
                 y = forTraining$Class,
                 method = "svmRadial",
                 tuneLength = 20,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = cvCtrl)
svmTune

predictors(svmTune)
plot(varImp(svmTune),top = 30)
svmTune$finalModel
plot(svmTune, metric = "ROC", scales = list(x = list(log =2)))
svmPred <- predict(svmTune, forTesting[, names(forTesting) != "Class"])
confusionMatrix(svmPred, forTesting$Class)

svmProbs <- predict(svmTune, forTesting[, names(forTesting) != "Class"],
                    type = "prob")
head(svmProbs)
svmROC <- roc(predictor = svmProbs$top,
             response = forTesting$Class,
             levels = rev(levels(forTesting$Class)))
svmROC

plot(rpartROC, type = "S")
plot(c5ROC, add = TRUE, col = "red")
plot(svmROC, add = TRUE, col = "blue")
#comparing models with resampling
cvValues <- resamples(list(CART = rpartTune, SVM = svmTune, C5.0 = c5Tune))
summary(cvValues)
splom(cvValues, metric = "ROC")
xyplot(cvValues, metric = "ROC")
parallelplot(cvValues, metric = "ROC")
dotplot(cvValues, metric = "ROC")
rocDiffs <- diff(cvValues, metric = "ROC")
summary(rocDiffs)
dotplot(rocDiffs, metric = "ROC")
