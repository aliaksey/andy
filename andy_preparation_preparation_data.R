ipsresult<-read.csv(file="DATA/iPS screen results andi.csv")
ipsresult$AWell<-ipsresult$Row.1*100+ipsresult$Column.1
load("indam.RData")
indam$AWell<-indam$ARow*100+indam$ACol

#selecting columns that should be merged
ipsresults<-ipsresult[,c("AWell",
                         "Oct4.pos.colonies...Number.of.Objects.1")]
#merging data
#merging with feature idx
ipsresfeat<-merge(indam[,c("AWell","FeatureIdx")],
                  ipsresults, by="AWell")[,c("FeatureIdx","Oct4.pos.colonies...Number.of.Objects.1")]
#merging with feature discriptors
featmodel<-read.csv(file ="indexes_and_features.csv")

ipshitsmodel_temp<-merge(ipsresfeat,featmodel, 
                         by.x="FeatureIdx",by.y="feature.idx", all=F)
ipshitsmodel<-unique(ipshitsmodel_temp[,-c(1,3:14)])
names(ipshitsmodel)[names(ipshitsmodel) == 'Oct4.pos.colonies...Number.of.Objects.1']<-'Oct4.pos'