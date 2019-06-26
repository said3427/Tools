library(plyr)
library(dplyr)

data<-data.table::fread("~/Downloads/matriz_de_proteinas_filtradas.txt")
dataProt<-data[-1,10:46]
dataProt$Intensity<-as.numeric(dataProt$Intensity)
dataProt<-dataProt%>%arrange(-Intensity,Score)
dataProt<-dataProt[,c(33,37,1:3,18:23)]

lfc<-function(x,y){
  x<-as.numeric(x)+1
  y<-as.numeric(y)+1
  return(log2(x/y))
}

