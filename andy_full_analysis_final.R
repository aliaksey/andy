    # fiterout highly correlated features
    # Cart tree as  party
    # cart with CV
    # importance of parameters
    # roc
    # heatmap with selected parameters for 2 hits
    
    
    ##make another model for comparison
    ##make regression analysis
    
    
    ##########regression part
    ##add random forest
    ##add statistical filtering
    ##add make seperate script on feature selection
    ##separate script on rgression
    rm(list=ls())
    library(caret)
    library(corrplot)
    library(randomForest)
    library(rpart)
    library(partykit)
    library(gplots)
    set.seed(2)
    load("DATA/indam.RData")
    load("DATA/andyshi.RData")
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
    ipshitsmodel_temp<-unique(ipshitsmodel_temp[,-c(3:14,40)])
    
    row.names(ipshitsmodel_temp)<-ipshitsmodel_temp$FeatureIdx
    
    ipshitsmodel<-ipshitsmodel_temp[,-1]
    
    ipshitsmodel<-ipshitsmodel[ipshitsmodel$Class!="middle",]
    ipshitsmodel$Class<-factor(ipshitsmodel$Class)
    
    plot(featmodel$FeatSize,featmodel$WN4)
    plot(featmodel$WN0.1,featmodel$WN4)
    
    ###get read of highly correlated features
    corMatips <- cor(featmodel[,-c(1:13)], method="spearman")
    corrplot(corMatips)
    # corMatips <- cor(ipshitsmodel[,-1], method="spearman")
    # corrplot(corMatips)
    # find higly correlated features
    hCorr <- findCorrelation(corMatips, 0.75)
    colnames(corMatips[,hCorr])
    corMatips.f <- cor(ipshitsmodel[,-c(1,(hCorr+1))],method="spearman")
    corrplot(corMatips.f)
    ipshitsmodel_f<-ipshitsmodel[,-c((hCorr+1))]
    ##creating model
    
    ##selecting samples for training and testing
    data_for_model<-ipshitsmodel_f
    class_data<-data_for_model[,1]
    inTrain <- createDataPartition(class_data, p =3/4, list = FALSE)
    forTraining <- data_for_model[inTrain,]
    #forTraining <- ipshitsmodel
    forTrainingX <- forTraining[, names(forTraining) != "Class"]
    #reate testing set for features selection
    forTesting <- data_for_model[-inTrain,]
    #forTesting <- ipshitsmodel
    
    #############rpart analysis##########
#     rpart_training <- rpart(Class~.,  method="class", data=data_for_model)
#     #plot(rpart_training)
#     #text(rpart_training)
#     ##polt as party object
#     rpart1a <- as.party(rpart_training)
#     plot(rpart1a, main="Pruned CART classyfication tree for OCT4 hits")
#     
    ##rpart on unchanged data
    
    rpart_training2 <- rpart(Class~.,  method="class", data=ipshitsmodel)
    #plot(rpart_training)
    #text(rpart_training)
    ##polt as party object
    rpart1a2 <- as.party(rpart_training2)
    plot(rpart1a2, main="Pruned CART classyfication tree for OCT4 hits")
    
    
    rpartPred <- predict(rpart_training, forTesting, type = "class")
    confusionMatrix(rpartPred, forTesting$Class)
   ______________________________________________________________________________________ 
    ##tunning the model
    cvCtrl <- trainControl(method = "repeatedcv", repeats = 10,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,savePred=T,returnResamp="final")
    #rpart
    rpartTune <- train(Class ~ ., data = forTraining, method = "rpart",
                       tuneLength = 10,
                       metric = "ROC",
                       trControl = cvCtrl)
    plot(rpartTune)
    predictors(rpartTune)
     plot(varImp(rpartTune),top=5,cex=4,pch=16,
         main="Feature importance for CART method")
    varImp_re<-varImp(rpartTune)
    row.names(varImp_re$importance)[varImp_re$importance>0]
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
    ##svm
    svmTune <- train(Class ~ ., data = forTraining,
                     method = "svmRadial",
                     tuneLength = 10,
                     preProc = c("center", "scale"),
                     metric = "ROC",
                     trControl = cvCtrl)#,
                     #classProbs =  TRUE)
    plot( svmTune)
    svmTune
    
    #predictors(svmTune)
    plot(varImp(svmTune),top=17,cex=4,pch=16,cex.axis=30,
         main="Feature importance for SVM method")
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
    ##logit
    logitTune <- train(x = forTrainingX,
                       y = forTraining$Class,
                       method = "glm",
                       tuneLength = 10,
                       family = binomial(link = "logit"),
                       metric = "ROC",
                       trControl = cvCtrl)
    summary(logitTune)
    plot(logitTune)
    
    library(ggplot2)
      ggplot(forTrainingX, aes(y=DC, x=WN0.4)) + geom_point() + 
        stat_smooth(method="glm", family="binomial", se=FALSE)
      
    #predictors(logitTune)
    plot(varImp(logitTune),top=17,cex=4,pch=16,cex.axis=30,
         main="Feature importance for Logit method")
    logitTune$finalModel
    logitPred <- predict(logitTune, forTesting[, names(forTesting) != "Class"])
    confusionMatrix(logitPred, forTesting$Class)
    
    library(pROC)
    logitROC <- roc(predictor = logitProbs$top,
                    response = forTesting$Class,
                    levels = rev(levels(forTesting$Class)))
    plot(logitROC, type = "S", print.thres = 0.47,col = "red",lwd=2.5,
         ylab="True Positive Rate(Sensitivity)",
         xlab="False Positive rate (1-Specificity)")
    legend(x=0.6,y=0.3,
           legend=c(paste(" AUC=","0.77"),
                    paste("Accuracy=","0.72" )),box.lwd = 0,box.col = "white",bg = "white")
    
    library(ROCR)
    predLogit <- prediction(logitProbs$top, forTesting$Class)
    perfLogit <- performance( predLogit, "tpr", "fpr")
    plot( perfLogit, col="red",lty=1, lwd=3)
    abline(a=0,b=1,col="grey",lwd=1,lty=3)  
    auc<-performance(predLogit,"auc")
    auc <- unlist(slot(auc, "y.values"))
    auct <- paste(c("AUC  = "),auc,sep="")
    legend(x=0.6,y=0.3,
           legend=c(paste(" AUC=","0.77"),
                    paste("Accuracy=","0.72" )),box.lwd = 0,box.col = "white",bg = "white")
    
    
    logitProbs <- predict(logitTune, forTesting[, names(forTesting) != "Class"],
                          type = "prob")
    head(logitProbs)
    logitROC <- roc(predictor = logitProbs$top,
                  response = forTesting$Class,
                  levels = rev(levels(forTesting$Class)))
    plot(logitROC, type = "S", print.thres = 0.47,col = "red",lwd=2.5,
         ylab="True Positive Rate(Sensitivity)",
         xlab="False Positive rate (1-Specificity)")
    
    
    library(ROCR)
    predLogit <- prediction(logitProbs$Positive, forTesting$Class)
    perfLogit <- performance( predLogit, "tpr", "fpr")
    plot( perfLogit, col="red",lty=1, lwd=3)
    abline(a=0,b=1,col="grey",lwd=1,lty=3)  
    auc<-performance(predLogit,"auc")
    auc <- unlist(slot(auc, "y.values"))
    auct <- paste(c("AUC  = "),auc,sep="")
    legend(x=0.6,y=0.3,
           legend=c(paste(" AUC=","0.76"),
                    paste("Accuracy=","0.72" )),box.lwd = 0,box.col = "white",bg = "white")
    
    
    
    
    ggplot(logitTune) +theme(legend.position="top")
    
    ##compare 
    plot(rpartROC, type = "S",col = "red", main="comparison 3 fitted models")#,print.thres = .5
    plot(svmROC, add = TRUE, col = "blue")#,print.thres = .5
    plot(logitROC, add = TRUE, col = "yellow")
    legend(x=0.6,y=0.2, legend=c("CART: AUC=0,8","SVM: AUC=0,85","Logit: AUC=0,77"), 
           lty=1, col=c("red","blue","yellow"), bty='o', cex=1)
    #resampling models
    resamps <- resamples(list(CART = rpartTune,
                              SVM = svmTune,
                              Logit = logitTune))
    summary(resamps)
    trellis.par.set(theme1)
    bwplot(resamps, layout = c(3, 1))
    
    trellis.par.set(caretTheme())
    dotplot(resamps, metric = "ROC")
    splom(resamps)
    difValues <- diff(resamps)
    difValues
    summary(difValues)
    bwplot(difValues, layout = c(3, 1))
    trellis.par.set(caretTheme())
    dotplot(difValues)
    
    #saving model
    save(svmTune,rpartTune,logitTune, file="model_for_andy_data4.RData")
    
    
    ###########################################################################################################################
    
    
    ##creating heatmap
    #get the data
    ##ordring
    data_for_heatm<-data_for_model[order(ipshitsmodel$Class),]
    row.names(varImp_re$importance)[varImp_re$importance>0]
    data_to_heatm<-data_for_heatm[,predictors(rpartTune)]
    #data_to_heatm<-data_for_heatm[,row.names(varImp_re$importance)[varImp_re$importance>0]]
    ##naming raw names
    row.names(data_to_heatm)<-paste(data_for_heatm$Class,
                                         row.names(data_for_heatm))
    boxplot(data_to_heatm$FeatSize)
    ##sclaing data
    scl<-apply(data_to_heatm,2,sd)
    cnt<-apply(data_to_heatm,2,mean)
    data_to_heatm_scale<-scale(data_to_heatm,center=cnt, scale=scl)
    boxplot(data_to_heatm_scale)
    ##########ordering data for hetmap
    #top
    top_scale<-as.matrix(data_to_heatm_scale[grepl("top",row.names(data_to_heatm_scale)),])
    top_scale<-top_scale[do.call(order, lapply(1:NCOL(top_scale), function(i) top_scale[, i])), ]
    #bottom
    bottom_scale<-as.matrix(data_to_heatm_scale[grepl("bottom",row.names(data_to_heatm_scale)),])
    bottom_scale<-bottom_scale[do.call(order, lapply(1:NCOL(bottom_scale), function(i) bottom_scale[, i])), ]
    #merging
    data_to_heatm_scale<-rbind(top_scale[length(top_scale[,1]):1,],bottom_scale[length(bottom_scale[,1]):1,])
    ##plotting heatmap
    colors = c(seq(-7,-0.2,length=100),seq(-0.2,0.2,length=100),seq(0.2,10,length=100))
    
    my_palette <- colorRampPalette(c("red", "black", "green"))(n = 299)
    
    heatmap.2(data_to_heatm_scale,col=my_palette, Rowv = F,cexCol=.8,
              offsetCol = 0.1,breaks=colors, density.info="none", trace="none", 
              dendrogram="none", symm=F,symkey=F,symbreaks=T, scale="none", 
              main=("Features selected by CART"))
    ##selecting represenatative surfaces from each of 4 groups.
    #Creating four groups
    
    
    #top subgroups
    top_largefeture.temp<-strsplit(rownames(top_scale[top_scale[,1]>0,])," ")
    top_largefeture<-as.numeric(do.call(rbind, top_largefeture.temp)[,2])
    top_smallfeture.temp<-strsplit(rownames(top_scale[top_scale[,1]<0,])," ")
    top_smallfeture<-as.numeric(do.call(rbind, top_smallfeture.temp)[,2])
    #bottom subgroups
    bottom_largefeture.temp<-strsplit(rownames(bottom_scale[bottom_scale[,1]>0,])," ")
    bottom_largefeture<-as.numeric(do.call(rbind, bottom_largefeture.temp)[,2])
    bottom_smallfeture.temp<-strsplit(rownames(bottom_scale[bottom_scale[,1]<0,])," ")
    bottom_smallfeture<-as.numeric(do.call(rbind, bottom_smallfeture.temp)[,2])
    
    # ##find fetures that might be different between two minor subgroups.
    # ####################################################################################
    # 
    # topsm<-as.data.frame(cbind(FeatureIdx=top_smallfeture,Class=rep("top_small")))
    # toplg<-as.data.frame(cbind(FeatureIdx=top_largefeture,Class=rep("top_large")))
    # 
    # bottomsm<-as.data.frame(cbind(FeatureIdx=bottom_smallfeture,Class=rep("bottom_small")))
    # bottomlg<-as.data.frame(cbind(FeatureIdx=bottom_largefeture,Class=rep("bottom_large")))
    # 
    # for_small_comp<-as.data.frame(rbind(topsm,bottomsm))
    # for_large_comp<-as.data.frame(rbind(toplg,bottomlg))
    # 
    # 
    # #merging with feature discriptors
    # featmodel<-read.csv(file ="DATA/indexes_and_features.csv")
    # 
    # largemodel_temp<-merge(for_large_comp,featmodel, 
    #                          by.x="FeatureIdx",by.y="feature.idx", all=F)
    # largemodel<-unique(largemodel_temp[,-c(1,3:14)])
    # 
    # smallmodel_temp<-merge(for_small_comp,featmodel, 
    #                        by.x="FeatureIdx",by.y="feature.idx", all=F)
    # smallmodel<-unique(smallmodel_temp[,-c(1,3:14)])
    # 
    # smallmodel$Class<-factor(smallmodel$Class)
    # largemodel$Class<-factor(largemodel$Class)
    # 
    # ##selecting samples for training and testing
    # data_for_model<-largemodel
    # class_data<-smallmodel[,1]
    # inTrain <- createDataPartition(class_data, p =3/4, list = FALSE)
    # forTraining <- data_for_model[inTrain,]
    # #forTraining <- ipshitsmodel
    # forTrainingX <- forTraining[, names(forTraining) != "Class"]
    # #reate testing set for features selection
    # forTesting <- data_for_model[-inTrain,]
    # #forTesting <- ipshitsmodel
    # 
    # #############rpart analysis##########
    # rpart_training <- rpart(Class~.,  method="class", data=data_for_model)
    # #plot(rpart_training)
    # #text(rpart_training)
    # ##polt as party object
    # rpart1a <- as.party(rpart_training)
    # plot(rpart1a, main="Pruned CART classyfication tree for OCT4 hits")
    # 
    # ##rpart on unchanged data
    # 
    # rpart_training2 <- rpart(Class~.,  method="class", data=largemodel)
    # #plot(rpart_training)
    # #text(rpart_training)
    # ##polt as party object
    # rpart1a2 <- as.party(rpart_training2)
    # plot(rpart1a2, main="Differences for Top and Bottom with large features")
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
    # library(pROC)
    # rpartROC <- roc(forTesting$Class, rpartProbs[, "top"], 
    #                 levels = rev(forTesting$Class))
    # plot(rpartROC, type = "S", print.thres = .5)
    # rpartROC
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
    # svmROC <- roc(predictor = svmProbs$top,
    #               response = forTesting$Class,
    #               levels = rev(levels(forTesting$Class)))
    # svmROC
    # 
    # plot(rpartROC, type = "S",col = "red", main="comparison CART model vs SVM")#,print.thres = .5
    # plot(svmROC, add = TRUE, col = "blue")#,print.thres = .5
    # legend(x=1,y=1, legend=c("CART: AUC=0.8","SVM: AUC=0.86"), 
    #        lty=1, col=c("red","blue"), bty='o', cex=1)
    # 
    # 
    # # 
    # # ##show correlation between edge parameters and cell number
    # # top100_feat<-read.csv2(file="DATA/Top_100_further_analysis_Alex.csv")
    # # 
    # 
