library(dplyr)
library(tidyr)

data2<-read.table("targets.bed",header=F)
colnames(data2)<-c("ID","exon","chr","start","end","number")
data<-data2
data2<-mutate(data2,chr=paste("chr",chr,sep="")) %>% separate(exon, c("GENE", "EXON"), sep = "\\;", remove=FALSE)
final<-data2[,c("chr","start","end","GENE")]
write.table(final,"targets_final.bed",sep="\t",quote=F,row.names=F)



ref=/data1/said/Exomas_INCan/Homo_sapiens_assembly19.fasta
bed=/data1/said/Exomas_INCan/00_Metadata/targets_genes_exons.bed
dict=/data1/said/Exomas_INCan/Homo_sapiens_assembly19.dict 
picard BedToIntervalList \
      I=$bed \
      O=list.interval_list \
      SD=$dict

intervals=/data1/said/Exomas_INCan/00_Metadata/list.interval_list

gatk -T DiagnoseTargets -R $ref \
-I 2_bam/IPCT-FC167-HCMX2017-Cap1286-17-ID47_170407-K00147-0040-BHJJF5BBXX-4-TCGAAG.bwa_recalibed.bam -L ../list.interval_list -o coverage.test


gatk -Xmx60G -T DepthOfCoverage -R $ref \
-I HCC-001.bam -L $intervals -o HCC-001.coverage.txt




python3.6 ~/miniconda3/bin/cnvkit.py batch -m amplicon /data1/said/Exomas_INCan/02_BAM/*.bam -n -t /data1/platypus/cnvkit/targets_genes_exons.bed -f /data1/said/Exomas_INCan/Homo_sapiens_assembly19.fasta --output-reference /data1/said/Exomas_INCan/Homo_sapiens_assembly19.fasta.cnn -d /data1/platypus/cnvkit/INCAN --annotate /data1/platypus/cnvkit/refFlat_noChr.txt -p 30

#Text
cnvkit.py breaks "$i.cnr"  "$i.cns" -o "$i.breaks.txt"
cnvkit.py gainloss "$i.cnr" -s "$i.cns" -t 0.4 -m 5 -o "$i.gainloss.txt"

#PDF
for file in $(ls *.cnr) ;
do
i=${file:0:7}
cnvkit.py scatter "$i.cnr" -s "$i.cns" -o "$i.scatter.pdf"
cnvkit.py scatter "$i.cnr" -s "$i.cns" -o "$i.scatter.BRCA2.pdf" -g BRCA2
cnvkit.py scatter "$i.cnr" -s "$i.cns" -o "$i.scatter.BRCA1.pdf" -g BRCA1
cnvkit.py scatter "$i.cnr" -s "$i.cns" -o "$i.scatter.VHL.pdf" -g VHL
cnvkit.py scatter "$i.cnr" -s "$i.cns" -o "$i.scatter.MLH1.pdf" -g MLH1
cnvkit.py scatter "$i.cnr" -s "$i.cns" -o "$i.scatter.MSH2.pdf" -g MSH2
cnvkit.py scatter "$i.cnr" -s "$i.cns" -o "$i.scatter.MSH6.pdf" -g MSH6
done




cnvkit.py metrics *.cnr -s *.cns
