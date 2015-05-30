rm(list=ls())
##create Marcs internal idx
nucFF<-as.data.frame(cbind(c(55, 1034, 793, 1775, 85, 884),rep("Nucleus Form Factor")))
nucMI<-as.data.frame(cbind(c(235,545, 1089, 1627, 1597, 221),rep("Nucleus Maximum Intensity")))
nucCA<-as.data.frame(cbind(c(2032, 1833,  2000, 233, 1689, 696),rep("Cell Alignement")))
marcHpi<-rbind(nucFF,nucMI,nucCA)
colnames(marcHpi)<-c("InternalIdx","Signature")
##loading data with internal indexes
featmodel<-read.csv(file ="DATA/indexes_and_features.csv")
marcHpi_meged_temp2<-merge(marcHpi,featmodel, 
                         by.x="InternalIdx",by.y="internal.idx", all=F)
#loading data with andy's indexes
load("DATA/indam.RData")
marcHpi_meged_temp<-merge(marcHpi,indam, by.x="InternalIdx", all=F)
#loading data with andy's indexes
marcHpi_meged_d<-marcHpi_meged_temp[,c("Signature", "FeatureIdx","ACol","ARow") ]
##join with hits data if any
load("DATA/andyshi.RData")
##selecting only features which are in heats
marcHpi_meged_d$FeatureIdx%in%topanfeat
marcHpi_meged_d[marcHpi_meged_d$FeatureIdx%in%botanfeat,"Hits"]<-"Bottom Hit"
marcHpi_meged_d[marcHpi_meged_d$FeatureIdx%in%topanfeat,"Hits"]<-"Top Hit"
marcHpi_meged_d[is.na(marcHpi_meged_d$Hits),"Hits"]<-"Not a hit surfase"

write.table(marcHpi_meged_d, file="DATA/Surfaces_rom_Marcs_paper.csv", sep = ",",row.names = F)
###make expanded hits
load("DATA/indam_e.RData")
marcHpi_meged_temp_e<-merge(marcHpi,indam_e, by.x="InternalIdx", all=F)
#loading data with andy's indexes
marcHpi_meged_d_e<-marcHpi_meged_temp_e[,c("Signature", "FeatureIdx","ACol_e","ARow_e") ]
##join with hits data if any
load("DATA/andyshi.RData")
##selecting only features which are in heats
marcHpi_meged_d_e[marcHpi_meged_d_e$FeatureIdx%in%botanfeat,"Hits"]<-"Bottom Hit"
marcHpi_meged_d_e[marcHpi_meged_d_e$FeatureIdx%in%topanfeat,"Hits"]<-"Top Hit"
marcHpi_meged_d_e[is.na(marcHpi_meged_d_e$Hits),"Hits"]<-"Not a hit surfase"

write.table(marcHpi_meged_d_e, file="DATA/Surfaces_rom_Marcs_paper_expanded.csv", sep = ",",row.names = F)

##selected 2 surfaces are 1522 and 237

marcHpi_meged_d_e.sel<-marcHpi_meged_d_e[marcHpi_meged_d_e$FeatureIdx%in%c(1522,237),]
write.table(marcHpi_meged_d_e.sel, file="DATA/selected surfaces.csv", sep = ",",row.names = F)

