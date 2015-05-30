
featureimg2webpage("large bottom",bottom_largefeture)

featureimg2webpage("small bottom",bottom_smallfeture)

featureimg2webpage("large top",top_largefeture)

featureimg2webpage("small top",top_smallfeture)

featureimg2webpage("blank2",2177)
featureimg2webpage("Frits hit",229)
featureimg2webpage("1",1)
image_validation<-c(297,68,73,504,345,518)

featureimg2webpage("image_validation_02_06",image_validation)
load("DATA/indam_e.R")

indam_e[indam_e$ARow==54&indam_e$ACol==19,"FeatureIdx"]
featureimg2webpage("test 3",1702)

indam_e[indam_e$Row==59&indam_e$Col==60,"FeatureIdx"]

featureimg2webpage("2135",2135)

indam_e[indam_e$FeatureIdx==2135,]


bach_surface<-c(11,229,494,1018,1050,1147,1673)

featureimg2webpage("Marc hulsman surfaces",marcHpi_meged_d_e$FeatureIdx)

featureimg2webpage("Marc hulsman selected surfaces",c(1522,237))

##alp analysis
load("alp.high.RDATA")
featureimg2webpage("ALP high.3",x)
load("alp.low.RDATA")
featureimg2webpage("ALP low.3",y)
##################

featureimg2webpage("predicted top final v5 rpart",top_pred_rpart)
featureimg2webpage("predicted bottom final v5 rpart",bottom_pred_rpart)
featureimg2webpage("predicted top final v5 logit",top_pred_logit)
featureimg2webpage("predicted bottom final v5 logit",bottom_pred_logit)
featureimg2webpage("predicted top final v5 svm",top_pred_svm)
featureimg2webpage("predicted bottom final v5 svm",bottom_pred_svm)
##print surfaces selected for fabrication
sel.surf<-read.csv2("D:/projects/phenome_project/surfaces_used_for_fabrication_mask.csv")
featureimg2webpage("phenome_project_surfaces",sel.surf$FeatureIdx)
featureimg2webpage("1087",1087)
featureimg2webpage("1_23",1)
featureimg2webpage("2100",2100)

featureimg2webpage("9",9)

featureimg2webpage<-function(name, fidx){
  #load("andyshi.RData")
  load("indam.RData")
  load("DATA/indam_e.R")
    indam[indam$ARow==17&indam$ACol==17,"FeatureIdx"]<-21777
  htmlfilepath<-paste('D:/projects/andy_project/image_verif/',
                      name,".html", sep = "") 
    ## creating html file
  htmlfile<-file.path(htmlfilepath)
  #specification of html ############################################
  formatspec_head<-'<!DOCTYPE html> \r\n <html> \r\n <body>' 
  formatspec_end<-'</html>' 
  formatspec_h1<-paste('<h1>', name, '</h1>', sep = "")
  formatspec_h1_1<-'<h1> <font color="blue">  </h1>' 
  formatspec_h1_2<-'<h1> Surfaces </h1>' 
  formatspec_h1_3<-'<h1> <font color="blue"> @ </h1>' 
  formatspec_h1_b<-'<h1> <font color="default"> # </h1>' 
  formatspec_h1_n<-'<h1> <font color="red"> 666 </h1>' 
  formatspec_h2<-paste("<h2> Feature number ", fidx, "</h2>\r\n", sep="")   # specify feature number
  formatspec_links<-'<img src='
  formatspec_linke<-' width="300" align="middle"></body> \r\n \r\n' 
  
  formatspec_<-'<hr>' ########
  ## writing data in html file=======
  cat(formatspec_head,file = htmlfile, append=T)
  cat(formatspec_h1,file = htmlfile, append=T)
  cat(formatspec_,file = htmlfile, append=T)
  #=====
  ## write all
  cat(formatspec_h1_2,file = htmlfile, append=T)
  
  #temp_t<-indam[with(indam, FeatureIdx %in% fidx),]
  temp.p<-indam_e[with(indam_e, FeatureIdx %in% fidx),]
  temp<-temp.p[!duplicated(temp.p$FeatureIdx),]
  #temp<-temp_t[temp_t$FeatureIdx%in%]
  for  (i in 1:length(temp[,1])) 
  {
    path1<-paste("../surface_images/surface-Row-Col-",sprintf( "%02d",temp[i,"Row"]),
                 "-",sprintf( "%02d",temp[i,"Col"]),".png", sep="")
    cat(paste(formatspec_links,path1,formatspec_linke,sep=""),file = htmlfile, append=T);
  } 
  sprintf( "%02d", 2 ) 
  cat(formatspec_end,file = htmlfile, append=T)
} 
##selecting features based on size and density
featmodel<-read.csv(file ="DATA/indexes_and_features.csv")
plot(featmodel$FeatSize)
feat.s<-featmodel[featmodel$FeatSize<15,]
nrow(feat.s)
plot(feat.s$FCP)
feat.ss<-feat.s[feat.s$FCP>0.4,]
nrow(feat.ss)
featureimg2webpage("surfaces filtered bysize part1",feat.s[1:248,"feature.idx"])
featureimg2webpage("surfaces filtered bysize part2",feat.s[249:496,"feature.idx"])
featureimg2webpage("surfaces filtered bysize part3",feat.s[497:744,"feature.idx"])
##plot selected values
#import
sel.serf<-unique(read.table("hemant_index.csv",header = T,stringsAsFactors=F))
sel.serf$UIDx<-paste(sel.serf$ROW,sel.serf$COL,sep="_")
#select features
featmodel$UIDx<-paste(featmodel$row.idx.top,featmodel$col.idx.top,sep="_")
sel.hem.feat<-featmodel[featmodel$UIDx%in%sel.serf$UIDx,"feature.idx"]

featmodel[featmodel$row.idx.top==3&featmodel$col.idx.top==19,"feature.idx"]
featmodel[featmodel$row.idx.top==2&featmodel$col.idx.top==42,"feature.idx"]
featureimg2webpage("2135",2135)


  featureimg2webpage("selected hemant features",sel.hem.feat)
