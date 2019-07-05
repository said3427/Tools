# Introduction ------------------------------------------------------------
# Pipeline to analyze Maxquant output
# Author: Said Muñoz Montero
# Github: said3427
# Mail: said3427@gmail.com
# Last Modification Date: 4/July/2019

# Libraries ---------------------------------------------------------------
# source("https://bioconductor.org/biocLite.R")
# biocLite("DEP")
library("DEP")
library("plyr")

# Importing files ---------------------------------------------------------
setwd("~/Github/Tools/Proteomics/")
data<-data.table::fread("proteinGroups.txt")

colnames(data)<-gsub(" ",".",colnames(data))
colnames(data)<-gsub("\\+",".",colnames(data))

designmatrix<-data.frame(label=paste(rep(c("C-C","C-M","C-S","WD-C","WD-M","WD-S"),each=4),"_",rep(c("1","2","3","4")),sep=""),
                         condition=rep(c("C-C","C-M","C-S","WD-C","WD-M","WD-S"),each=4),
                         replicate=rep(1:4,times=6),stringsAsFactors=FALSE) 

data<-data[,c(1,2,6,7,58,60,61,62,194:217,243:245)]
# Cleaning data -----------------------------------------------------------
LFQ(data,designmatrix,"MinProb","all",c("C.C"))


dataProt$Intensity<-as.numeric(dataProt$Intensity)
dataProt<-dataProt%>%arrange(-Intensity,Score)
dataProt<-dataProt[,c(33,37,1:3,18:23)]

lfc<-function(x,y){
  x<-as.numeric(x)+1
  y<-as.numeric(y)+1
  return(log2(x/y))
}
data<-data.table::fread("Proteins_matrix.txt")
data<-data.table::fread("proteinGroups_ids.txt")
data<-data.table::fread("/Volumes/Said/proteinGroups.txt")

colnames(data)<-gsub("Intensity_","",colnames(data))


intensities<-data[,194:217]
intensities<-t(t(intensities)/colSums(intensities))

intensities[intensities==0]<-NA

boxplot(log2(intensities),las=2)

normalized<-preprocessCore::normalize.quantiles(as.matrix(intensities))
boxplot(normalized)

colSums(!is.na(intensities))
dim(data)
