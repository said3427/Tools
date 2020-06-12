## I need to push the previous chunk of code

## Just the part of ENSBL Convertion
library(data.table)
library("dplyr")

data<-fread("~/Downloads/SUvsF1.stats.csv")

genes<-data$V1
colnames(data)[1]<-"gene_id"
gene_IDs <- getBM(filters= "ensembl_gene_id", attributes= c("ensembl_gene_id","hgnc_symbol","description"),
                 values = genes, mart= mart)
data<-left_join(data, gene_IDs, by = c("gene_id"="ensembl_gene_id"))

write.csv(data,file="~/Desktop/SUvsF1.stats.csv",quote=F,row.names=F)

data<-fread("~/Downloads/PSvsF1.stats.csv")

genes<-data$V1
colnames(data)[1]<-"gene_id"
gene_IDs <- getBM(filters= "ensembl_gene_id", attributes= c("ensembl_gene_id","hgnc_symbol","description"),
                  values = genes, mart= mart)
data<-left_join(data, gene_IDs, by = c("gene_id"="ensembl_gene_id"))

write.csv(data,file="~/Desktop/PSvsF1.stats.csv",quote=F,row.names=F)
