#Generate an network based infographic from my publications
#Date: 3/16/2019
#Last updated: 12/25/2019
#Author: Chris Marcum <cmarcum@uci.edu>

library(network)
library(sna)
library(png)
points.with.raster<-function(x,y,raster,width=10,height=10,...){
  #x,y vectors are centroids
  #raster is a list of raster arrays
  x1<-x-(width/2)
  x2<-x+(width/2)
  y1<-y-(height/2)
  y2<-y+(height/2)
  points(x,y,...)
  for(i in 1:length(x)){
      rasterImage(as.raster(raster[[i]]),x1[i],y1[i],x2[i],y2[i])
   }
}

#My bibtex files includes entries for "keywords" that are comma separated inset 
#  networks, health, aging, methods, animals
# They were imported using bib2df (which is really handy).

if(0){
 library(bib2df)
 marcum<-bib2df("https://www.dropbox.com/s/pmk7aend6z5tqyv/marcum.bib?dl=1")
 keywords<-lapply(strsplit(marcum$KEYWORDS,split=","),function(x) gsub(" ","",x))
 kw.sup<-na.omit(unique(unlist(keywords)))
 topics<-matrix(0,nrow=length(kw.sup),ncol=length(kw.sup),dimnames=list(kw.sup,kw.sup))
 for(i in 1:length(keywords)){
  if(length(keywords[[i]])>0){
   topics[keywords[[i]],keywords[[i]]]<-topics[keywords[[i]],keywords[[i]]]+1
  }
 }
}

load("marcum.cites.RData")
#Read in all the icons
#icons<-sapply(dir(pattern="png"),readPNG)
#icons["marcumtopics.png"]<-NULL

        if(0{
        png("marcumtopics.png",width=800,height=800,res=100)
        par(mar=c(.5,.5,.5,.5))}
        gplot(topics,displaylabels=TRUE,usearrows=FALSE,edge.lwd=sqrt(topics),vertex.cex=log(diag(topics))+.05,vertex.col="cornflowerblue",vertex.border=FALSE,label.pos=3,coord=gcord,edge.col="gray",label.cex=1.25,jitter=FALSE)
        points.with.raster(gcord[,1],gcord[,2],icons[paste(colnames(topics),"png",sep=".")],width= ifelse(prop.table(diag(topics))<.1,.1,prop.table(diag(topics))),height= ifelse(prop.table(diag(topics))<.1,.1,prop.table(diag(topics))),cex=0)
dev.off()

