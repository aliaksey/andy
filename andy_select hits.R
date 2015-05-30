
# make original indexing
indam<-read.table("DATA/indexes for andy.csv", header=T, sep=",",na.strings = "NaN")
indam$AWell<-indam$ARow*100+indam$ACol
hitsan<-read.table("DATA/andy's hits.csv", header=T, sep=",",na.strings = "NaN")
hitsan$TWell<-hitsan$TRow*100+hitsan$TCol
hitsan$BWell<-hitsan$BRow*100+hitsan$BCol
# finding features by indexes
topanfeat<-indam[indam$AWell%in%hitsan$TWell,"FeatureIdx"]
botanfeat<-indam[indam$AWell%in%hitsan$BWell,"FeatureIdx"]



x<-indam[indam$AWell%in%hitsan$TWell,]


save(topanfeat,botanfeat, file="andyshi2.RData")

hitsan[hitsan$TWell==1717,]

indam[indam$FeatureIdx==33,]

indam[indam$ARow==27&indam$ACol==9,"FeatureIdx"]

x[87,]

intersect(topanfeat, botanfeat)

hitttt<-cbind(Top=topanfeat,Bottom=botanfeat)

write.table(hitttt, "clipboard-128", sep="\t", row.names=F)
