library("oligo")

mirna<-read.celfiles(list.files(pattern=".CEL"))

gsub(".CEL","",colnames(mirna))->colnames(mirna)

read.csv("~/Downloads/miRNA-4_0-st/miRNA-4_0-st.csv",comment.char="#")->annotation
annotation.rat<-annotation[annotation$Species.Scientific.Name=="Rattus norvegicus",]

#annotation.human<-annotation[annotation$Species.Scientific.Name=="Homo sapiens",]


mirna.rat<-rma(mirna,normalize=T,background=T)

mirna.rat.expr<-exprs(mirna.rat)[rownames(mirna.rat) %in% annotation.rat$Probe.Set.Name,]



library("affy")
pdf("boxplotrat.pdf")
boxplot(mirna.rat.expr,las=2,main="RMA-normalized",col=colores)
dev.off()

pdf("density.pdf")
plotDensity(mirna.rat.expr,main="Density plot",col=colores)
dev.off()

detach("package:affy")

mirna.experimento<-exprs(mirna.human)[!(rownames(mirna.human) %in% annotation.human$Probe.Set.Name),]

boxplot(mirna.experimento)
plotDensity(mirna.experimento)

boxplot(mirna.human.expr,las=2,cex.axis=.5,col=)

#read.csv("~/Dropbox/INMEGEN/Proyecto Verano/Rafael Velazquez/Resultados/microRNAs/ListaExpDif_Todos.csv")->lista

lista<-cbind(lista,annotation[match(lista[,1],annotation$Accession),c(2,3,4,6)])
write.csv(lista,"Todos.csv")


diseno<-matrix(0,9,3)
diseno[1:3,1]<-1
diseno[4:6,2]<-1
diseno[7:9,3]<-1
colnames(diseno)<-c("Basal","FSH","LNG")


contraste="Osteoporosis-Control"


cont.matrix<-makeContrasts(contraste,levels=diseno)
fit=lmFit(mirna.rma,diseno)
fitC=contrasts.fit(fit,cont.matrix)
fitCB<-eBayes(fitC)
TT=topTable(fitCB,coef=1,adjust="fdr",sort.by="logFC",number=nrow(exprs(mirna.rma)),genelist=fit$genes)


selected<-TT[(abs(TT$logFC)>.5 & TT$P.Value<.01),]

gsub("","",rownames(selected))->rownames(selected)
gsub("_s","",rownames(selected))->rownames(selected)
gsub("_x","",rownames(selected))->rownames(selected)

write.csv(selected,"~/Desktop/selected.CDFviejo.csv")



gsub("","",rownames(mirna.rma))->rownames(mirna.rma)
gsub("_s","",rownames(mirna.rma))->rownames(mirna.rma)
gsub("_x","",rownames(mirna.rma))->rownames(mirna.rma)

