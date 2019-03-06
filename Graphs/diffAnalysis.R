?#Functions Differential-Analysis

readCLS <- function(clsfile){
		list  <-	readLines(clsfile)	
		list  <-	list("Number"=unlist(strsplit(list[[1]],"\t")),"groups"=unlist(strsplit(list[[2]],"\t")),"Names"=unlist(strsplit(list[[3]],"\t")),"Assign"=unlist(strsplit(list[[4]],"\t")))
		return(list)
}

#Design matrix
designMatrix	<-	function(cls){
  design <- matrix(0,nrow=as.numeric(cls$samples.groups[1]),ncol=as.numeric(cls$samples.groups[2]))
  colnames(design)<-cls$groups.names  
  design.row <- NULL
  for( i in 1:nrow(design)){
  design.row[i] <- cls$celfile.names[i]
  design[i,as.numeric(cls$group.assignments[i])] <- 1
  
  }
  rownames(design) <- design.row
  return(design)
}


###Extract by column name, features=column names that will be extracted

extractMatrix 	<-	function(features,data.matrix) {
  matching<-match(as.character(colnames(data.matrix)),as.character(features))
  final<-data.matrix[,!is.na(matching)]
  return(final)
}


##Linear model and TopTable, return topTable genes but also update (or create) the global variable fitCB
TTlinearmodel<-function(design,contraste,data.exp){
  print(contraste)
  cont.matrix <<- makeContrasts(contraste,levels=design)
  # linear model fit
  # -----------------
  fit = lmFit(data.exp,design)
  fitC = contrasts.fit(fit, cont.matrix)
  fitCB <<- eBayes(fitC)
  TT = topTable(fitCB, coef=1, adjust = "fdr", sort.by = "logFC", number=nrow(data.exp), genelist=fit$genes)
  return(TT)
}  


annotation<-function(TT,annDB){
  probenames <- as.character(TT$ID)
  entID <- getEG(probenames,annDB)
  sym <- getSYMBOL(probenames,annDB)
  geneExpData <- data.frame(sym,entID, TT)
  indx.NA = !is.na(geneExpData$sym)
  geneExpData = geneExpData[indx.NA,]
  geneExpData = unique(geneExpData)
  return(geneExpData)
}

volcanoplot2<-function(TT){
  lfc.status = NULL
  B.status = NULL
  pVal.status = NULL
  all.contrasts = NULL
  
  lfc.status = TT$logFC
  B.status = TT$B
  pVal.status = -log10(TT$P.Value)
  
  fitCB=data.frame(coef=lfc.status,lods=B.status)
  x0 = min(lfc.status) -.5
  x1 = max(lfc.status) +.5
  y0 = min(B.status) -.5
  y1 = max(B.status) +.5
  plot(lfc.status,B.status,col="black", ylim=c(y0,y1), xlim=c(x0,x1), main=contraste, pch=16,cex=.35,cex.lab=1.2,xlab="Log Fold Change",ylab="B statistic")
  par(new=T)
  abline(v=-M, col="brown", ylab="", xlab="")
  par(new=T)
  abline(v=M, col="brown", ylab="", xlab="")
  par(new=T)
  abline(h=B, col="black", ylab="", xlab="")
  par(new=T)
  ind1 = abs(fitCB$coef)>M
  ind2 = fitCB$lods >B
  ind3 = (fitCB$coef>M & fitCB$lods>B)
  ind4 = (fitCB$coef< -M & fitCB$lods>B)
  x = as.matrix(fitCB$coef[ind1])
  y = as.matrix(fitCB$lods[ind1])
  plot(x, y, col="magenta",ylim=c(y0,y1), xlim=c(x0,x1),main="", pch = 20, xlab="", ylab="",cex.lab=1.2)
  x = as.matrix(fitCB$coef[ind2])
  y = as.matrix(fitCB$lods[ind2])
  par(new=T)
  plot(x, y, col="orange",  ylim=c(y0,y1), xlim=c(x0,x1), main="", pch = 20, xlab="", ylab="",cex.lab=1.2)
  x = as.matrix(fitCB$coef[ind3])
  y = as.matrix(fitCB$lods[ind3])
  par(new=T)
  plot(x, y, col="red",  ylim=c(y0,y1), xlim=c(x0,x1), main="", pch = 20, xlab="", ylab="",cex.lab=1.2)
  x = as.matrix(fitCB$coef[ind4])
  y = as.matrix(fitCB$lods[ind4])
  par(new=T)
  plot(x, y, col="darkgreen", ylim=c(y0,y1), xlim=c(x0,x1), main="", pch = 20, xlab="", ylab="",cex.lab=1.2)
  
}


pm(data.bygroup)[,1:8]=normalize(pm(data[,1:8]),method="quantile")
pm(data.bygroup)[,c(9:11,15:16)]=normalize(pm(data[,c(9:11,15:16)]),method="quantile")
pm(data.bygroup)[,c(12:14,17:18)]=normalize(pm(data[,c(12:14,17:18)]),method="quantile")

data.bygroup=NULL

data.bygroup=exprs(rma(data[,1:8],normalize=FALSE))
data.bygroup=cbind(data.bygroup,exprs(rma(data[,c(9:11,15:16)],normalize=FALSE)))
data.bygroup=cbind(data.bygroup,exprs(rma(data[,c(12:14,17:18)],normalize=FALSE)))

data.bygroup.quantiles=normalize(data.bygroup,method="quantile")


volcanoplot2pValue<-function(TT){
  lfc.status = NULL
   
    pVal.status = NULL
    all.contrasts = NULL

    lfc.status = TT$logFC
  pVal.status = -log10(TT$P.Value)


  x0 = min(lfc.status) -.5
    x1 = max(lfc.status) +.5
    y0 = min(pVal.status) -.5
    y1 = max(pVal.status) +.5
    plot(lfc.status,pVal.status,col="black", ylim=c(y0,y1), xlim=c(x0,x1), main=contraste, pch=16,cex=.35,cex.lab=1.2,xlab="Log Fold Change",ylab="-log(P-value)")
    par(new=T)
    abline(v=-M, col="brown", ylab="", xlab="")
    par(new=T)
    abline(v=M, col="brown", ylab="", xlab="")
    par(new=T)
    abline(h=B, col="black", ylab="", xlab="")
    par(new=T)
    ind1 = abs(lfc.status)>M
    ind2 = pVal.status >B
    ind3 = (lfc.status>M & pVal.status>B)
    ind4 = (lfc.status< -M & pVal.status>B)
    x = as.matrix(lfc.status[ind1])
    y = as.matrix(pVal.status[ind1])
    plot(x, y, col="magenta",ylim=c(y0,y1), xlim=c(x0,x1),main="", pch = 20, xlab="", ylab="",cex.lab=1.2)
    x = as.matrix(lfc.status[ind2])
    y = as.matrix(pVal.status[ind2])
    par(new=T)
    plot(x, y, col="orange",  ylim=c(y0,y1), xlim=c(x0,x1), main="", pch = 20, xlab="", ylab="",cex.lab=1.2)
    x = as.matrix(lfc.status[ind3])
    y = as.matrix(pVal.status[ind3])
    par(new=T)
    plot(x, y, col="red",  ylim=c(y0,y1), xlim=c(x0,x1), main="", pch = 20, xlab="", ylab="",cex.lab=1.2)
    x = as.matrix(lfc.status[ind4])
    y = as.matrix(pVal.status[ind4])
    par(new=T)
    plot(x, y, col="darkgreen", ylim=c(y0,y1), xlim=c(x0,x1), main="", pch = 20, xlab="", ylab="",cex.lab=1.2)

}

differentialAnalysis <- function(ndata,contraste,design.matrix, M, B){

  gene.symbol <-  ndata[,1]
  entrez.ID <-  ndata[,2]

##Result: TopTable and fitCB (linear model) as global variable
  ndata.numeric<-as.matrix(ndata[,c(-1,-2)])
  mode(ndata.numeric)<-"numeric"

  TT<-TTlinearmodel(design.matrix,contraste,ndata.numeric)
  indx.TT<-rownames(TT)
  TT<-cbind("Entrez_ID"=entrez.ID[as.numeric(rownames(TT))],"Gene_Symbol"=gene.symbol[as.numeric(rownames(TT))],TT)
  rownames(TT)<-indx.TT

   file.name <- paste("VolcanoPlot-",contraste,".pdf", sep="")
    pdf(file=file.name)
    volcanoplot2pValue(TT)
    dev.off()
# Selection point
# --------------------------------
  selected <- TT[(TT$P.Value<(10^-(B)) & abs(TT$logFC)>M),]

  indx.selected<-as.numeric(rownames(selected))

  if (nrow(selected)>1){

    # outputs: Data
    # -----------------
    file.name <- paste("Stats-",contraste,".csv", sep="")
    write.csv(selected, file=file.name)
    save(file=paste("stats-",contraste,".rdata",sep=""),selected)
    # documento .HTML
    file.name <- paste(contraste,"html", sep=".")
    htmlpage(list(selected$Entrez_ID), filename = file.name, title=paste("Differentially Expressed Genes: ",contraste, sep=" "), othernames = selected[,-1], table.head = c("Entrez ID", colnames(selected)[-1]), table.center = TRUE)
  
    # output: Graphics
    # -----------------
    
    ###Volcano Plot
   
    
    #-------------------------------------      
    #Hierarchical Clustering and Heatmaps
    #-------------------------------------
    
    # heatmap with the microarrays involved only
    file.name <- paste("Heatmap-",contraste,".pdf", sep="")
    
    design.contrast<-design.matrix[,rownames(cont.matrix)[cont.matrix!=0]]
    design.contrast<-design.contrast[apply(design.contrast,1,sum)!=0,]
    
    data.Cont=extractMatrix(rownames(design.contrast),ndata)
    data.Cont=cbind("Gene_Symbol"=ndata[,"Gene_Symbol"],"Entrez_ID"=ndata[,"Entrez_ID"],data.Cont)
    datos.clus <- data.Cont[as.numeric(rownames(selected)),]
    
    sym <- datos.clus[,"Gene_Symbol"]
    genelabels <- paste(sym, " (", datos.clus[,"Entrez_ID"], ")", sep="")
    
    pdf(file=file.name,height=15,width=10)
    
    par(oma = c(3,1,3,4),mar=c(12,5,2,2)+0.1)


    datos.clus<-as.matrix(datos.clus[,c(-1,-2)])
    mode(datos.clus)<-"numeric"

    ind.hmap <- heatmap.2(datos.clus, col=greenred(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=.55, cexCol=.7, main = contraste, labRow = genelabels,ColSideColors=colores[colnames(ndata.numeric)%in%colnames(datos.clus)])
    ######, ColSideColors = mycol[c(array.indx)$values] 
  
    Idx <- ind.hmap$rowInd[length(ind.hmap$rowInd):1] 
    genes.hmap <- cbind(selected[Idx,],datos.clus[Idx,])
    dev.off()             

    file.name <- paste("ListaHeatmapGenes-",contraste,".csv", sep="")
    write.csv(genes.hmap, file=file.name)

    
    #-----------------------------------------------------------
    #Hierarchical Clustering and Heatmaps with all Microarrays
    #-----------------------------------------------------------

    
  ##heatmap with all microarrays
    file.name <- paste("Heatmap.All-",contraste,".pdf", sep="")
  
    pdf(file=file.name,height=15,width=10)
  
    datos.clus.all <- ndata[indx.selected,]
  
    sym <-datos.clus.all[,1] 
    genelabels <- paste(sym, " (", datos.clus.all[,2], ")", sep="")
  
    datos.clus.all<-as.matrix(datos.clus.all[,c(-1,-2)])
    mode(datos.clus.all)<-"numeric"
      
    par(oma = c(3,1,3,4),mar=c(12,5,2,2)+0.1)
    ind.hmap <- heatmap.2(as.matrix(datos.clus.all), col=greenred(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=.55, cexCol=.7, main = contraste, labRow = genelabels,ColSideColors = colores)
    Idx <- ind.hmap$rowInd[length(ind.hmap$rowInd):1] 
    genes.hmap <- cbind(selected[Idx,],datos.clus.all[Idx,])
    dev.off()
  
   file.name <- paste("ListaHMapAllGenes-",contraste,".csv", sep="")
   write.csv(genes.hmap, file=file.name)
    
    }


}
