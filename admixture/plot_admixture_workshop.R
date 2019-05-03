### script to plot ADMIXTURE results ### 
### Andres Moreno Nov 2012 ###


#Set working directory
setwd("~/path/to/your/output/files")

#upload the data
k3 <- read.table ('INMEGEN.YRI.CEU.ZAP.clean.3.Q', header=FALSE)

#transpose the matrix 
K3 <- t(k3)

#upload popinfo file (fam file with additional info after the 6th column)
popinfo <- read.csv("INMEGEN.YRI.CEU.ZAP.clean.fam.spaces.txt",header=FALSE,sep="\t")

#define spaces to separate populations
spaces <- c(0,diff(popinfo[,7]))

#plot ancestry proportions
pdf ("INMEGEN100K.K3.pdf", height=3, width=10)
barplot (K3, col=c("blue", "darkgreen", "red"), border=NA, space=spaces, las=2, xlab= "Individuals", ylab= "K=3")
dev.off()



#################################
###### for additional Ks ########


###### K=4 ########

#edit the read.table function and load new data
k4 <- read.table ('INMEGEN.YRI.CEU.ZAP.clean.4.Q', header=FALSE)
K4 <- t(k4)

#edit the barplot function to create a new pdf
pdf ("INMEGEN100K.K4.pdf", height=3, width=10)
barplot (K4, col=c("darkgreen", "orange", "blue", "red"), border=NA, space=spaces, las=2, xlab= "Individuals", ylab= "K=4")
dev.off()


###### K=5 ########

#edit the read.table function and load new data
k5 <- read.table ('INMEGEN.YRI.CEU.ZAP.clean.5.Q', header=FALSE)
K5 <- t(k5)

#edit the barplot function to create a new pdf
pdf ("INMEGEN100K.K5.pdf", height=3, width=10)
barplot (K5, col=c("purple", "darkgreen", "blue", "orange", "red"), border=NA, space=spaces, las=2, xlab= "Individuals", ylab= "K=5")
dev.off()


###### K=6 ########

#edit the read.table function and load new data
k6 <- read.table ('INMEGEN.YRI.CEU.ZAP.clean.6.Q', header=FALSE)
K6 <- t(k6)

#edit the barplot function to create a new pdf
pdf ("INMEGEN100K.K6.pdf", height=3, width=10)
barplot (K6, col=c("blue", "red", "purple", "orange", "darkgreen", "yellow"), border=NA, space=spaces, las=2, xlab= "Individuals", ylab= "K=6")
dev.off()


