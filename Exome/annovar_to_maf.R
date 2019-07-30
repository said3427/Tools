#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
#BiocManager::install("PoisonAlien/maftools")
library(maftools)

if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} 

sample=args[1]
maf <- annovarToMaf(paste(sample,".hg38_multianno.txt",sep=""),Center="INMEGEN",refBuild="hg38",sampleAnno=sample)
write.table(maf,paste(sample,".maf",sep=""),sep="\t",quote=FALSE,row.names = FALSE)