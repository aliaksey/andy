rm(list=ls())
load("DATA/indam.RData")
load("DATA/andyshi.RData")
## selectin top 30
hitsan<-read.table("DATA/andy's hits.csv", header=T, sep=",",na.strings = "NaN")
#selecting only top 30
hit30<-hitsan[1:30,]
hit30$TWell<-hit30$TRow*100+hit30$TCol
hit30$BWell<-hit30$BRow*100+hit30$BCol
indam$AWell<-indam$ARow*100+indam$ACol
#getting featureidx 
topanfeat30<-indam[indam$AWell%in%hit30$TWell,"FeatureIdx"]
botanfeat30<-indam[indam$AWell%in%hit30$BWell,"FeatureIdx"]

##getting only indexes of another half of the chip
load("DATA/indam_e.RData")

indam_ah<-indam_e[!indam_e$UnitIdx%in%indam$UnitIdx,]
##getting indexes
##PArt 1
top_x<-indam[indam$FeatureIdx%in%topanfeat30,c("FeatureIdx","ACol","ARow")]
colnames(top_x)<-c("FeatureIdx","Col_part1","Row__part1")
bot_x<-indam[indam$FeatureIdx%in%botanfeat30,c("FeatureIdx","ACol","ARow")]
colnames(bot_x)<-c("FeatureIdx","Col_part1","Row__part1")
part1_merg<-rbind(top_x,bot_x)
##part 2
top_indam<-indam_ah[indam_ah$FeatureIdx%in%topanfeat30,c("FeatureIdx","ACol_e","ARow_e")]
colnames(top_indam)<-c("FeatureIdx","Col_part2","Row__part2")
top_indam$Cat<-"Top"
bot_indam<-indam_ah[indam_ah$FeatureIdx%in%botanfeat30,c("FeatureIdx","ACol_e","ARow_e")]
colnames(bot_indam)<-c("FeatureIdx","Col_part2","Row__part2")
bot_indam$Cat<-"Bottom"
part2_merg<-rbind(top_indam,bot_indam)

resta<-merge(part1_merg,part2_merg, by="FeatureIdx")
resta<-resta[order(resta$Cat,resta$FeatureIdx),]

write.table(resta, file="DATA/top30_both_repl.csv", sep = ",",row.names = F)


