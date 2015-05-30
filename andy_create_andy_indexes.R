rm(list=ls())
# make original indexing
featnuma<-read.table("DATA/feature numbering.csv", header=T, sep=",",na.strings = "NaN")
#fntopa<-featnuma[,c("internal.idx","unit.idx.top","col.idx.top","row.idx.top","feature.idx")]
fnbottoma<-featnuma[,c("internal.idx","unit.idx.bottom","col.idx.bottom","row.idx.bottom","feature.idx")]
colnames(fnbottoma)<-c("InternalIdx","UnitIdx","Col", "Row", "FeatureIdx")
fnbottomac<-as.data.frame(fnbottoma[fnbottoma$Row>33&fnbottoma$Col>34,])
#fnbottomac[fnbottomac$Row==66&fnbottomac$Col==66, "FeatureIdx"]=21777
# load andys data
#anind<-read.table("F:/Alex/andy analysis/andy indexes.csv", header=T, sep=",",na.strings = "NaN")
#anind[anind$FeatureIdx==0,"FeatureIdx"]=2177
#anind[anind$ARow==1&anind$ACol==1, "FeatureIdx"]=21777
#merge all indexes together
#indam<-merge(anind,fnbottomac, by="FeatureIdx",all=F)
#indam[indam$Row==66&indam$Col==66, "FeatureIdx"]=2177
# creating initial matrix ARow
aa=33
bb=c()
for (i in 1:33){
  bb=cbind(bb,rep(aa, times=32))
  aa=aa-1}
ACol=c(bb)
#ARow=rep(c(32:1),times=33)

indam<-cbind(fnbottomac[order(fnbottomac$Row, fnbottomac$Col),], ACol,
             ARow=rep(c(32:1),times=33))
head(indam)
# script converting andy's indexes
andyi<-function(ar, ac){
  resulta<-indam[indam$ARow==ar&indam$ACol==ac,c("Row", "Col")]
  #resulta<-indam[indam$FeatureIdx==bbccx,c("Row", "Col")]
  print( str(resulta)) }

write.csv(indam, file="indexes for andy.csv", quote = F,row.names = F)
andyi(1,8)

save(indam,file="DATA\indam.RData")
##########expanding andy indexes to the whole topochip
#rm(list=ls())
featnuma<-read.table("DATA/feature numbering.csv", header=T, sep=",",na.strings = "NaN")
fntopa<-featnuma[,c("internal.idx","unit.idx.top","col.idx.top","row.idx.top","feature.idx")]
colnames(fntopa)<-c("InternalIdx","UnitIdx","Col", "Row", "FeatureIdx")
fnbottoma<-featnuma[,c("internal.idx","unit.idx.bottom","col.idx.bottom","row.idx.bottom","feature.idx")]
colnames(fnbottoma)<-c("InternalIdx","UnitIdx","Col", "Row", "FeatureIdx")
fntall<-rbind(fntopa,fnbottoma)
colnames(fnbottoma)<-c("InternalIdx","UnitIdx","Col", "Row", "FeatureIdx")


aa_e=66
bb_e=c()
for (i in 1:66){
  bb_e=cbind(bb_e,rep(aa_e, times=66))
  aa_e=aa_e-1}
ACol_e=c(bb_e)
#ARow=rep(c(32:1),times=33)
indam_e<-cbind(fntall[order(fntall$Row, fntall$Col),], ACol_e,
             ARow_e=rep(c(66:1),times=66))
head(indam_e)
save(indam_e,file="DATA/indam_e.RData")
# 
# 
# indam[indam$FeatureIdx==2177,]
# 
# indam[indam$ARow==1&indam$ACol==3,c("Row","Col")]
# indam[indam$ARow==8&indam$ACol==1,]
# 

#cc<-intersect(anind$FeatureIdx,fnm$FeatureIdx)
