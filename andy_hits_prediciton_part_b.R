rm(list=ls())
library(caret)
library(lattice)
library(ggplot2)
#prediction of hits from B side
##loading model
##loading features
##features that were not used in screening

##getting only indexes of another half of the chip
load("DATA/indam.RData")
featmodel<-read.csv(file ="DATA/indexes_and_features.csv")
# indam_B_t<-indam_e[!indam_e$FeatureIdx%in%indam$FeatureIdx,]
# indam_B<-indam_B_t[,-]
FeatPartB<-c(1:2177)[!c(1:2177)%in%indam$FeatureIdx]
PartB_t<-featmodel[featmodel$feature.idx%in%FeatPartB,]
PartB<-PartB_t[,-c(1:13)]
rownames(PartB)<-PartB_t$feature.idx
#shuffeling a bit matrix
set.seed(23456)
PartB <- PartB[sample(nrow(PartB)),]
load("model_for_andy_data.RData")

#prediction 
#based on rpart
predictedfeat<-predict(rpartTune, PartB, type = "prob")
 head(predictedfeat)
##select top hits
top_pred_rpart<-rownames(predictedfeat[order(-predictedfeat$top),])[1:30]
bottom_pred_rpart<-rownames(predictedfeat[order(-predictedfeat$bottom),])[1:30]

#based on logit
predictedfeat<-predict(logitTune, PartB[,predictors(logitTune)], type = "prob")
head(predictedfeat)
##select top hits
top_pred_logit<-rownames(predictedfeat[order(-predictedfeat$Positive),])[1:30]
bottom_pred_logit<-rownames(predictedfeat[order(-predictedfeat$Negative),])[1:30]

#based on svm
predictedfeat<-predict(svmTune, PartB[,predictors(svmTune)], type = "prob")
head(predictedfeat)
##select top hits
top_pred_svm<-rownames(predictedfeat[order(-predictedfeat$top),])[1:30]
bottom_pred_svm<-rownames(predictedfeat[order(-predictedfeat$bottom),])[1:30]

##getting indexes for predicted hits

get_andy_indexes<-function(top, bottom,name){
  ##getting indexes
  load("DATA/indam_e.RData")
  
  top_indam<-indam_e[indam_e$FeatureIdx%in%top,c("FeatureIdx","ACol_e","ARow_e")]
  colnames(top_indam)<-c("FeatureIdx","Col","Row")
  top_indam$Cat<-"Top"
  top_indam<-top_indam[order(top_indam$FeatureIdx),]
  bot_indam<-indam_e[indam_e$FeatureIdx%in%bottom,c("FeatureIdx","ACol_e","ARow_e")]
  colnames(bot_indam)<-c("FeatureIdx","Col","Row")
  bot_indam$Cat<-"Bottom"
  bot_indam<-bot_indam[order(bot_indam$FeatureIdx),]
  resta_t<-rbind(top_indam,bot_indam)
  resta<-do.call(data.frame, aggregate(. ~ FeatureIdx, resta_t, as.vector))
  resta<-resta[,c("FeatureIdx","Col.1",  "Row.1", "Col.2","Row.2",  "Cat.1")]
  resta<-resta[order(resta$Cat.1),]
    print(resta)
  write.table(resta, file=paste("DATA/",name,".csv",sep = ""), sep = ";",row.names = F)
}



get_andy_indexes(top_pred_svm,bottom_pred_svm,"30 surfaces predicted by SVM")
get_andy_indexes(top_pred_logit,bottom_pred_logit,"30 surfaces predicted by logit")
get_andy_indexes(top_pred_rpart,bottom_pred_rpart,"30 surfaces predicted by CART")

# library(caret)
# ##create model based on 3 classes
# 
# library(caret)
# library(corrplot)
# library(randomForest)
# library(rpart)
# library(partykit)
# library(gplots)
# set.seed(2)
# load("DATA/indam.RData")
# load("DATA/andyshi.RData")
# #creating data for hits
# top<-as.data.frame(cbind(FeatureIdx=topanfeat,Class=rep("top")))
# bot<-as.data.frame(cbind(FeatureIdx=botanfeat,Class=rep("all")))
# middle<-as.data.frame(cbind(FeatureIdx=indam[!indam$FeatureIdx%in%c(topanfeat,
#                                                                     botanfeat),"FeatureIdx"],Class=rep("all")))
# for_ips_model<-as.data.frame(rbind(top,bot,middle))
# #merging with feature discriptors
# featmodel<-read.csv(file ="DATA/indexes_and_features.csv")
# ipshitsmodel_temp<-merge(for_ips_model,featmodel, 
#                          by.x="FeatureIdx",by.y="feature.idx", all=F)
# ipshitsmodel_temp<-unique(ipshitsmodel_temp[,-c(3:14,40)])
# 
# row.names(ipshitsmodel_temp)<-ipshitsmodel_temp$FeatureIdx
# 
# ipshitsmodel<-ipshitsmodel_temp[,-1]
# 
# ipshitsmodel$Class<-factor(ipshitsmodel$Class)
# 
# ###get read of highly correlated features
# corMatips <- cor(featmodel[,-c(1:13)], method="spearman")
# corrplot(corMatips)
# # corMatips <- cor(ipshitsmodel[,-1], method="spearman")
# # corrplot(corMatips)
# # find higly correlated features
# hCorr <- findCorrelation(corMatips, 0.75)
# colnames(corMatips[,hCorr])
# corMatips.f <- cor(ipshitsmodel[,-c(1,(hCorr+1))],method="spearman")
# corrplot(corMatips.f)
# ipshitsmodel_f<-ipshitsmodel[,-c((hCorr+1))]
# ##creating model
# 
# ##selecting samples for training and testing
# data_for_model<-ipshitsmodel_f
# class_data<-data_for_model[,1]
# inTrain <- createDataPartition(class_data, p = 3/4, list = FALSE)
# forTraining <- data_for_model[inTrain,]
# #forTraining <- ipshitsmodel
# forTrainingX <- forTraining[, names(forTraining) != "Class"]
# #reate testing set for features selection
# forTesting <- data_for_model[-inTrain,]
# #forTesting <- ipshitsmodel
# #############rpart analysis##########
# rpart_training <- rpart(Class~.,  method="class", data=data_for_model)
# #plot(rpart_training)
# #text(rpart_training)
# ##polt as party object
# rpart1a <- as.party(rpart_training)
# plot(rpart1a, main="Pruned CART classyfication tree for OCT4 hits top vs all")
# 
# rpart_training2 <- rpart(Class~.,  method="class", data=ipshitsmodel)
# #plot(rpart_training)
# #text(rpart_training)
# ##polt as party object
# rpart1a2 <- as.party(rpart_training2)
# plot(rpart1a2, main="Pruned CART classyfication tree for OCT4 hits top vs all")
# 
# 
# rpartPred <- predict(rpart_training, forTesting, type = "class")
# confusionMatrix(rpartPred, forTesting$Class)
# ##tunning the model
# cvCtrl <- trainControl(method = "repeatedcv", repeats = 10,
#                        summaryFunction = twoClassSummary,
#                        classProbs = TRUE,savePred=T)
# #rpart
# rpartTune <- train(Class ~ ., data = forTraining, method = "rpart",
#                    tuneLength = 10,
#                    metric = "ROC",
#                    trControl = cvCtrl)
# plot(rpartTune)
# predictors(rpartTune)
# plot(varImp(rpartTune),top = 5,cex=4,pch=16,
#      main="Importance of Features do descriminate high and low OCT4 sutfaces")
# varImp_re<-varImp(rpartTune)
# row.names(varImp_re$importance)[varImp_re$importance>0]
# plot.train(rpartTune)
# #print.train(rpartTune)
# plot(rpartTune, scales = list(x = list(log = 10)))
# 
# rpartPred2 <- predict(rpartTune, forTesting)
# confusionMatrix(rpartPred2, forTesting$Class)
# 
# rpartProbs <- predict(rpartTune, forTesting, type = "prob")
# head(rpartProbs)
# 
# ##svm
# svmTune <- train(x = forTrainingX,
#                  y = forTraining$Class,
#                  method = "svmRadial",
#                  tuneLength = 10,
#                  preProc = c("center", "scale"),
#                  metric = "ROC",
#                  trControl = cvCtrl)
# svmTune
# 
# #predictors(svmTune)
# plot(varImp(svmTune),top = 30)
# svmTune$finalModel
# plot(svmTune, metric = "ROC", scales = list(x = list(log =2)))
# svmPred <- predict(svmTune, forTesting[, names(forTesting) != "Class"])
# confusionMatrix(svmPred, forTesting$Class)
# 
# svmProbs <- predict(svmTune, forTesting[, names(forTesting) != "Class"],
#                     type = "prob")
# head(svmProbs)
# 
# ##logit analysis
# 
# logitTune <- train(x = forTrainingX,
#                    y = forTraining$Class,
#                    method = "glm",
#                    tuneLength = 10,
#                    family = binomial(link = "logit"),
#                    metric = "ROC",
#                    trControl = cvCtrl)
# summary(logitTune)
# 
# 
# #predictors(logitTune)
# plot(varImp(logitTune))
# logitTune$finalModel
# logitPred <- predict(logitTune, forTesting[, names(forTesting) != "Class"])
# confusionMatrix(logitPred, forTesting$Class)
# 
# logitProbs <- predict(logitTune, forTesting[, names(forTesting) != "Class"],
#                       type = "prob")
# head(logitProbs)
# 
# #rpartTune, svmTune
