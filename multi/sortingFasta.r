#!/bin/bash
#Order constigs by length only .fasta

library("Biostrings")
library("batch")

parseCommandArgs()

name<-contigs
contigs<-readDNAStringSet(contigs)

contigs.sorted<-contigs[order(width(contigs),decreasing=T)]

writeXStringSet(contigs.sorted, filepath=paste(name,"sorted.fasta",sep=""),format="fasta")