rm(list=ls())
load("DATA/indam.RData")
load("DATA//andyshi.RData")
#creating data for hits
top<-as.data.frame(cbind(FeatureIdx=topanfeat,Class=rep("top")))
bot<-as.data.frame(cbind(FeatureIdx=botanfeat,Class=rep("bottom")))
middle<-as.data.frame(cbind(FeatureIdx=indam[!indam$FeatureIdx%in%c(topanfeat,
                                                        botanfeat),"FeatureIdx"],Class=rep("bottom")))
for_ips_model<-as.data.frame(rbind(top,bot,middle))
#merging with feature discriptors
featmodel<-read.csv(file ="DATA/indexes_and_features.csv")
ipshitsmodel_temp<-merge(for_ips_model,featmodel, 
                         by.x="FeatureIdx",by.y="feature.idx", all=F)
ipshitsmodel<-unique(ipshitsmodel_temp[,-c(1,3:14)])

ipshitsmodel<-ipshitsmodel[ipshitsmodel$Class!="middle",]
class(ipshitsmodel$Class)
##creating model
library(caret)
library(corrplot)
library(randomForest)
library(rpart)
###########################3calculating higly correlated features
# calculate correlation matrix
corMatips <- cor(ipshitsmodel[,-1], method="pearson")
corrplot(corMatips)
# find higly correlated features
hCorr <- findCorrelation(corMatips, 0.75)
colnames(corMatips[,hCorr])
corMatips.f <- cor(ipshitsmodel[,-c(1,(hCorr+1))])
corrplot(corMatips.f)

################Rank features by importance
# training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Class~., data=ipshitsmodel, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
###########################Performimg feature selection by models RF
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(ipshitsmodel[,-1], ipshitsmodel[,1], sizes=c(1:10), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

###########################Performimg feature selection by models SVM
features_data<-scale(ipshitsmodel[,-1])
class_data<-ipshitsmodel[,1]
#create training and test data sets
for_train <- createDataPartition(class_data, p = 3/4, list = FALSE)
##crete training set for features selection
for_train_feat <- features_data[for_train,]
#reate testing set for features selection
for_test_feat <- features_data[-for_train,]
##repeat for class data
for_train_class <- class_data[for_train]
for_test_class <- class_data[-for_train]
# ####find and remove higly corelated features
# corrfeat <- cor(for_train_feat)
# hicorr <- findCorrelation(corrfeat, 0.75)
# for_train_feat <- for_train_feat[, -hicorr]
# for_test_feat <- for_test_feat[, -hicorr]
##running model predicition
svmProfile <- rfe(x=for_train_feat, y = for_train_class, sizes = c(1:10), 
rfeControl= rfeControl(functions = caretFuncs,number = 10),
method = "svmRadial",fit = FALSE)
svmProfile
predictors(svmProfile)
#############rpart analysis##########
fitah <- rpart(Class~.,  method="class", data=ipshitsmodel)
# plot tree
plot(fitah, uniform=TRUE,
     main="Regression tree for OCT4 positive colonies")
text(fitah, use.n=TRUE, all=TRUE, cex=.8)
# create postcript plot of tree
post(fitah, file = "rpart positive negative.ps",
     title ="Classyfication tree positive versus negative")

