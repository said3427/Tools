library(plyr)
library(dplyr)

data<-data.table::fread("/Volumes/Said/proteinGroups.txt")
dataProt<-data[-1,10:46]
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


intensities<-data[,69:74]
intensities<-t(t(intensities)/colSums(intensities))

intensities[intensities==0]<-NA

boxplot(log2(intensities),las=2)

normalized<-preprocessCore::normalize.quantiles(as.matrix(intensities))
boxplot(normalized)

colSums(!is.na(intensities))
