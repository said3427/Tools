library("seqinr")
dna<-read.fasta("Ceci-1.derep.vsearch.fasta")

profile=matrix(0,nrow = 4,ncol=292)
rownames(profile)<-c("a","t","c","g")

seqs<-c()
for(seq in dna){
  if(length(seq)==292){
    seqs<-c(seqs,paste(sapply(getSequence(seq),paste,collapse=""),collapse=""))
    for(i in 1:292){
      if(seq[i]=="a"){
        profile[1,i]=profile[1,i]+1
      }
      else if(seq[i]=="t"){
        profile[2,i]=profile[2,i]+1
      }
      else if(seq[i]=="c"){
        profile[3,i]=profile[3,i]+1
      }
      else if(seq[i]=="g"){
        profile[4,i]=profile[4,i]+1
      }
      else(print(seq[i]))
    }
  }
}

profile<-t(t(profile)/colSums(profile))

# 16S rRNA gene forward primer sequence (515f): TTACCGCGGCKGCTGRCAC
# 16S rRNA gene reverse primer sequence (806rB): ATTAGAWACCCBNGTAGTCC

seqs<-sapply(seqs, toupper)

require(ggplot2)
require(ggseqlogo)

#seqs_numeric = chartr('atcg','1234', seqs_dna$MA0001.1)
pdf(width = 1240,"Seqs.pdf")
ggseqlogo(seqs, method = 'prob',seq_type="dna" )
dev.off()
