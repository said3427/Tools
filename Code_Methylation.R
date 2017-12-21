#FLUJO Completo

pathmeth="/mnt/d/Aspen/BRCA/DNA_Methylation/JHU_USC__HumanMethylation27/Level_3"
pathexprs="/mnt/d/Aspen/BRCA/Expression-Genes/UNC__AgilentG4502A_07_3/Level_3"
pathresults="/mnt/d/Aspen/BRCA/"

#pathmeth="/mnt/d/Aspen/GBM2/DNA_Methylation/JHU_USC__HumanMethylation27/Level_3"
#pathexprs="/mnt/d/Aspen/GBM2/Expression-Genes/BI__HT_HG-U133A/Level_3"
#pathresults="/mnt/d/Aspen/GBM2/"

pathmeth="/mnt/d/Aspen/LUSC/DNA_Methylation/JHU_USC__HumanMethylation27/Level_3"
pathexprs="/mnt/d/Aspen/LUSC/Expression-Genes/BI__HT_HG-U133A/Level_3"
pathresults="/mnt/d/Aspen/LUSC/"

setwd(pathmeth)
files <- list.files()

sampleNms <- NULL
Beta      <- NULL
annot	  <-NULL

for (i in 1:length(files))
{
	data       <- read.delim(file=files[i], row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA", "---"), blank.lines.skip=TRUE, comment.char="#")
	sample     <- colnames(data)[2]
	sampleNms  <- c(sampleNms,sample)
	if (i == 1) {annot <- data[-1,1]}
	data       <- data[-1,]
	Beta       <- cbind(Beta,data[,2]) 
	print(i)
}

annot<-data[,-2]
colnames(annot)<-c("IlluminaID","GeneSymbol","Chromosome","GenomicCoordinate")

colnames(Beta) <- sampleNms
rownames(Beta) <- annot$IlluminaID

Beta[is.na(Beta)] <- 0
mode(Beta)        <- "numeric"
sums <- apply(Beta, 1, sum)
indx <- which(sums == 0)
beta <- Beta[-indx,]

dim(beta)
print(dim(Beta))
	
#save(Beta,beta,annot, file=name)

beta2 <- beta
	for (i in 1:nrow(beta))
	{
		indx <- which(beta[i,] == 0)
		if (length(indx) > 0){
			beta2[i,indx] <- median(beta[,-indx])
	}	
	print(i)
	}

fannot <- annot[annot$IlluminaID %in% rownames(beta2),]

setwd(pathresults)
save(beta2,fannot,file="MethylationBeta_noNAs.RData")



#######################Expression############################
setwd(pathexprs)

files <- list.files()

sampleNms <- NULL
Gexp      <- NULL

for (i in 1:length(files))
{
	data       <- read.delim(file=files[i], row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA", "---"), blank.lines.skip=TRUE, comment.char="#")
	sample     <- colnames(data)[2]
	sampleNms  <- c(sampleNms,sample)
	if (i == 1) {annot <- data[-1,1]}
	data       <- data[-1,]
	Gexp       <- cbind(Gexp,data[,2]) 
	print(i)
}
colnames(Gexp) <- sampleNms
rownames(Gexp) <- annot



print(dim(Gexp))
mode(Gexp)<-"numeric"

setwd(pathresults)
save(Gexp,annot, file="Expression.RData")



# load librarys
library(rgl)
library(evd)
library(MASS)
library(colorRamps)
library(RColorBrewer)
library(mclust)
library(fpc)



bmean <- apply(beta2, 1, mean,na.rm=TRUE)
gmean <- apply(Gexp, 1, mean,na.rm=TRUE)

pdf("histograma.pdf")
hist(bmean)
hist(gmean)
dev.off()

matching <- NULL
missing  <- NULL

for (i in 1:nrow(fannot))
{
	genes <- unlist(strsplit(fannot$GeneSymbol[i], split=";"))
	indx  <- match(genes, rownames(Gexp))
	indx  <- indx[!is.na(indx)]
	if (length(indx) > 0){
	matching <- c(matching,indx); print(i)
	} else {
		missing <- c(missing,i)
		}
}


mGexp  <- Gexp[matching,]
dim(mGexp)

mbeta2 <- beta2[-missing,]
dim(mbeta2)

fannot2 <- fannot[-missing,]
dim(fannot2)



## Expanding beta matrix for those CpG sites associated with more than one gene
beta_expansion  <- NULL
names_expansion <- NULL
genes_expansion <- NULL
chr_expansion   <- NULL
coordinate_exp  <- NULL
mult            <- NULL

for (i in 1:nrow(fannot2))
{
	genes <- unlist(strsplit(fannot2$GeneSymbol[i], split=";"))
	if (length(genes) > 1){
		mult <- c(mult,i)
		for (j in 1:length(genes))
		{
			beta_expansion  <- rbind(beta_expansion,mbeta2[i,])
			names_expansion <- c(names_expansion,fannot2$IlluminaID[i])
			chr_expansion   <- c(chr_expansion,fannot2$Chromosome[i])
			coordinate_exp  <- c(coordinate_exp,fannot2$GenomicCoordinate[i])
		}
		genes_expansion <- c(genes_expansion,genes)
	}
	print(i)
}

length(mult)


dim(beta_expansion)

length(genes_expansion)

betaNoMult <- mbeta2[-mult,]
dim(betaNoMult )


cpgNoMult  <- fannot2$IlluminaID[-mult]
length(cpgNoMult)

chrNoMult  <- fannot2$Chromosome[-mult]
length(chrNoMult)

coordNoMult <- fannot2$GenomicCoordinate[-mult]
length(coordNoMult)


genesNoMult <- fannot2$GeneSymbol[-mult]
length(genesNoMult)


BETA <- rbind(betaNoMult,beta_expansion)
dim(BETA)


cpg  <-  c(rownames(betaNoMult),names_expansion)
length(cpg)


cpgfannot  <-  c(cpgNoMult,names_expansion)
length(cpgfannot)


chr  <- c(chrNoMult,chr_expansion)
length(chr)


coord <-  c(coordNoMult,coordinate_exp)
length(coord)

genes <- c(genesNoMult, genes_expansion)
length(genes)

matching2 <- match(genes, rownames(mGexp))
length(matching2)







BETA <- BETA[!is.na(matching2),]
dim(BETA)

cpg  <-  cpg[!is.na(matching2)]
length(cpg)

cpgfannot  <-  cpgfannot[!is.na(matching2)]
length(cpgfannot)


chr  <- chr[!is.na(matching2)]
length(chr)

coord <-  coord[!is.na(matching2)]
length(coord)

genes <- genes[!is.na(matching2)]
length(genes)











### Sort gene expression matrix for matching methylation data
GexToCpG  <- NULL
gexnames  <- NULL
gnames    <- rownames(mGexp)
for (i in 1:length(genes))
{
	indx  <- match(genes[i], gnames)
	indx  <- indx[!is.na(indx)]
	if (length(indx) > 0){
	GexToCpG   <- rbind(GexToCpG,mGexp[indx,])
	gexnames   <- c(gexnames,gnames[indx])
	}
	print(i)
}

dim(GexToCpG)


match(genes,gexnames)

save(GexToCpG, BETA, cpg, gexnames, chr, coord, file="Meth-Exp_matchingGenes.RData")
#load("Meth-Exp_matchingGenes.RData")
length(gexnames)
length(cpg)
length(unique(gexnames))
length(unique(cpg))

GexNoDup <- GexToCpG[!duplicated(gexnames),]
dim(GexNoDup)

BnoDup <- BETA[!duplicated(cpg),]
dim(BnoDup)

gexpnames <- gexnames[!duplicated(gexnames)]
methnames <- cpg[!duplicated(cpg)]

save(GexNoDup,BnoDup,gexpnames,methnames, file="Data_for_ARACNE.RData")

bmean <- apply(BETA, 1, mean,na.rm=TRUE)
gmean <- apply(GexToCpG, 1, mean,na.rm=TRUE)

pdf("medianas.pdf")
plot(bmean,gmean, col="blue")
dev.off()

## 3D plot
#M=kde2d(bmean,gmean,n=100)
#col5<- matlab.like(length(M$z))[rank(M$z)]
#persp3d(M$x,M$y,M$z,col=col5,xlab="Methylation",ylab="Expression",zlab="Density", box=FALSE)
#decorate3d(xlim, ylim, zlim)
#if (!rgl.useNULL())
#  play3d(spin3d(axis=c(0,0,1), rpm=8), duration=5)

## Spearman correlation computation
# order patients
#for (i in 1:nrow(BETA))
#{
#	scorr <- corr(ETA[i,])
#} ...

######### Clustering analysis
ME <- cbind(bmean,gmean)
colnames(ME) <- c("Average Methylation Beta value",ylab="Average Gene Expression")
rownames(ME) <- gexnames
gmm <- Mclust(ME, G=1:16)
pdf("gmm-clustering-exploration-16components.pdf")
plot(gmm) # plot results
1
2
3
4
0
dev.off()

gmm <- Mclust(ME, G=1:9)
pdf("gmm-clustering-exploration-9components.pdf")
plot(gmm) # plot results
1
2
3
4
0
dev.off()

save(gmm, file="gmmK9.RData")

### CpG Islands
load("Infium27k-annot.rda")

indx   <- match(cpg, ANNOT$TargetID)
ANNOTs <- ANNOT[indx,]

BetaF <- BETA[ANNOTs$CPG_ISLAND,]
GexpF <- GexToCpG[ANNOTs$CPG_ISLAND,]
bmean <- apply(BetaF, 1, mean,na.rm=TRUE)
gmean <- apply(GexpF, 1, mean,na.rm=TRUE)

## 3D plot
#M=kde2d(bmean,gmean,n=100)
#col5<- matlab.like(length(M$z))[rank(M$z)]
#persp3d(M$x,M$y,M$z,col=col5,xlab="Methylation",ylab="Expression",zlab="Density", box=FALSE)

ME <- cbind(bmean,gmean)
colnames(ME) <- c("Average Methylation Beta value",ylab="Average Gene Expression")
rownames(ME) <- gexnames[ANNOTs$CPG_ISLAND]

gmm <- Mclust(ME, G=1:9)
pdf("gmm-clustering-exploration-9components-CpG-Isl.pdf")
plot(gmm) # plot results
1
2
3
4
0
dev.off()
save(gmm, file="gmmK9_CpG.I.RData")



gmm <- Mclust(ME, G=1:11)
pdf("gmm-clustering-exploration-11components-CpG-Isl.pdf")
plot(gmm) # plot results
1
2
3
4
0

dev.off()
save(gmm, file="gmmK11_CpG.I.RData")

###################
# Include 450K annotation
###################
load("450k-fullannotInd.rda")

## TSS200
length(unlist(TSS200Ind$SID))
length(unique(unlist(TSS200Ind$SID)))

tss200  <- match(unique(unlist(TSS200Ind$SID)),cpg)
tss200  <- tss200[!is.na(tss200)]
length(tss200)

Btss200 <- BETA[tss200,]
Gtss200 <- GexToCpG[tss200,]
gtss200 <- gexnames[tss200]
btss200 <- cpg[tss200]

bmean <- apply(Btss200, 1, mean,na.rm=TRUE)
gmean <- apply(Gtss200, 1, mean,na.rm=TRUE)

#3D
#M=kde2d(bmean,gmean,n=100)
#col5<- matlab.like(length(M$z))[rank(M$z)]
#persp3d(M$x,M$y,M$z,col=col5,xlab="Methylation",ylab="Expression",zlab="Density", box=FALSE)

ME <- cbind(bmean,gmean)
colnames(ME) <- c("Average Methylation Beta value",ylab="Average Gene Expression")
rownames(ME) <- btss200

gmmtss200 <- Mclust(ME, G=1:9)
pdf("gmm-clustering-exploration-9components-tss200.pdf")
plot(gmmtss200) # plot results
1
2
3
4
0
dev.off()

save(gmmtss200, file="gmmK9_tss200.RData")

## UTR5Ind
length(unlist(UTR5Ind$SID))
length(unique(unlist(UTR5Ind$SID)))

UTR5  <- match(unique(unlist(UTR5Ind$SID)),cpg)
UTR5  <- UTR5[!is.na(UTR5)]
length(UTR5)

BUTR5 <- BETA[UTR5,]
GUTR5 <- GexToCpG[UTR5,]
gUTR5 <- gexnames[UTR5]
bUTR5 <- cpg[UTR5]


bmean <- apply(BUTR5, 1, mean,na.rm=TRUE)
gmean <- apply(GUTR5, 1, mean,na.rm=TRUE)

#M=kde2d(bmean,gmean,n=100)
#col5<- matlab.like(length(M$z))[rank(M$z)]
#persp3d(M$x,M$y,M$z,col=col5,xlab="Methylation",ylab="Expression",zlab="Density", box=FALSE)

ME <- cbind(bmean,gmean)
colnames(ME) <- c("Average Methylation Beta value",ylab="Average Gene Expression")
rownames(ME) <- bUTR5

gmmUTR5 <- Mclust(ME, G=1:9)
pdf("gmm-clustering-exploration-9components-UTR5.pdf")
plot(gmmUTR5) # plot results
1
2
3
4
0
dev.off()

save(gmmUTR5, file="gmmK9_tss200.RData")

## GENEBODYInd
length(GENEBODYInd)
length(unique(unlist(GENEBODYInd$SID)))

GENEBODY  <- match(unique(unlist(GENEBODYInd$SID)),cpg)
GENEBODY  <- GENEBODY[!is.na(GENEBODY)]

length(GENEBODY)

BGENEBODY <- BETA[GENEBODY,]
GGENEBODY <- GexToCpG[GENEBODY,]
gGENEBODY <- gexnames[GENEBODY]
bGENEBODY <- cpg[GENEBODY]

bmean <- apply(BGENEBODY, 1, mean,na.rm=TRUE)
gmean <- apply(GGENEBODY, 1, mean,na.rm=TRUE)

#M=kde2d(bmean,gmean,n=100)
#col5<- matlab.like(length(M$z))[rank(M$z)]
#persp3d(M$x,M$y,M$z,col=col5,xlab="Methylation",ylab="Expression",zlab="Density", box=FALSE)

ME <- cbind(bmean,gmean)
colnames(ME) <- c("Average Methylation Beta value",ylab="Average Gene Expression")
rownames(ME) <- GENEBODY

gmmGENEBODY <- Mclust(ME, G=1:9)
pdf("gmm-clustering-exploration-9components-GENEBODY.pdf")
plot(gmmGENEBODY) # plot results
1
2
3
4
0
dev.off()

save(gmmGENEBODY, file="gmmK9_GENEBODY.RData")

## EXON1Ind
length(unique(unlist(EXON1Ind$SID)))

EXON1  <- match(unique(unlist(EXON1Ind$SID)),cpg)
EXON1  <- EXON1[!is.na(EXON1)]

length(EXON1)

BEXON1 <- BETA[EXON1,]
GEXON1 <- GexToCpG[EXON1,]
gEXON1 <- gexnames[EXON1]
bEXON1 <- cpg[EXON1]

bmean <- apply(BEXON1, 1, mean,na.rm=TRUE)
gmean <- apply(GEXON1, 1, mean,na.rm=TRUE)

#M=kde2d(bmean,gmean,n=100)
#col5<- matlab.like(length(M$z))[rank(M$z)]
#persp3d(M$x,M$y,M$z,col=col5,xlab="Methylation",ylab="Expression",zlab="Density", box=FALSE)

ME <- cbind(bmean,gmean)
colnames(ME) <- c("Average Methylation Beta value",ylab="Average Gene Expression")
rownames(ME) <- EXON1

gmmEXON1 <- Mclust(ME, G=1:9)
pdf("gmm-clustering-exploration-9components-EXON1.pdf")
plot(gmmEXON1) # plot results
1
2
3
4
0
dev.off()

save(gmmEXON1, file="gmmK9_EXON1.RData")

##############
library(BUS)
library(minet)
BUS(EXP=mat,measure="MI",n.replica=N,net.trim="aracne",thresh=0.05,nflag=1)
