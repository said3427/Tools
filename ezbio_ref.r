#Formatting EZBiocloud taxonomy
source("http://bioconductor.org/biocLite.R")
biocLite("RAM")
library("RAM")
setwd("~/Desktop/")
taxonomy<-data.table::fread("taxonomy.tsv",sep="\t")
taxonomy<-separate(taxonomy,col="Taxon",sep = ";",into=c("kingdom","phylum","class","order","family","genus","species"))
especies<-strsplit(taxonomy$species,"_")
nombres<-c()
for(i in especies){
  if(length(i)>1){
    if(i[2]=="s"){nombres<-c(nombres,i[1])}else(
    nombres<-c(nombres,i[2])
    )
    }
  else(
    nombres<-c(nombres,i[1])
    ) 
}

taxonomy$species<-nombres
taxonomy<-within(taxonomy,tax<-paste(paste0("k__",kingdom),paste0("p__",phylum),paste0("c__",class),paste0("o__",order),paste0("f__",family),paste0("g__",genus),paste0("s__",species),sep=";"))
write.table(taxonomy[,c("Feature ID","tax")],quote = FALSE,row.names = FALSE,col.names = FALSE,sep="\t",file = "EZBIO_ref.txt")
