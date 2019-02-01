#Formatting EZBiocloud taxonomy
source("http://bioconductor.org/biocLite.R")
biocLite("RAM")
library("RAM")
setwd("~/Desktop/")
taxonomy<-data.table::fread("ezbiocloud_id_taxonomy.txt",sep="\t")
taxonomy<-separate(taxonomy,col="V2",sep = ";",into=c("kingdom","phylum","class","order","family","genus","species"))
especies<-strsplit(taxonomy$species," ")
nombres<-c()
for(i in especies){
  if(length(i)>1){
    nombres<-c(nombres,i[2])
    }
  else(
    nombres<-c(nombres,i[1])
    ) 
}

taxonomy<-within(taxonomy,tax<-paste(kingdom,phylum,class,order,family,genus,species,sep=";"))

write.table(taxonomy[,c("V1","tax")],quote = FALSE,row.names = FALSE,col.names = FALSE,sep="\t",file = "EZBIO_ref.txt")
