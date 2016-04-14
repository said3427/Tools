differentialAnalysismiRNA <- function(ndata,contraste,design.matrix, M, B){

  ID<-rownames(ndata)
  ##Result: TopTable and fitCB (linear model) as global variable
  ndata.numeric<-as.matrix(ndata)
  mode(ndata.numeric)<-"numeric"

  TT<-TTlinearmodel(design.matrix,contraste,ndata.numeric)
  file.name<-paste(contraste,"_toptable.txt",sep="")
  write.table(cbind(TT,DB.ordered.rat[match(rownames(TT),DB.ordered.rat[,2]),c(4,5)]),file=file.name,sep="\t",quote=F)

   file.name <- paste("VolcanoPlot-",contraste,".pdf", sep="")
    pdf(file=file.name)
    volcanoplot2pValue(TT)
    dev.off()
# Selection point
# --------------------------------
  selected <- TT[(TT$P.Value<(10^-(B)) & abs(TT$logFC)>M),]

  indx.selected<-match(rownames(selected),DB.ordered.rat$Probe.Set.Name)

  if (nrow(selected)>1){

    # outputs: Data
    # -----------------
    file.name <- paste("Stats-",contraste,".txt", sep="")
    write.table(cbind(selected,DB.ordered.rat[indx.selected,c(4,5)]), file=file.name,sep="\t",quote=F)
    save(file=paste("stats-",contraste,".rdata",sep=""),selected)
  
   
    
    #-------------------------------------      
    #Hierarchical Clustering and Heatmaps
    #-------------------------------------
    
    # heatmap with the microarrays involved only
    file.name <- paste("Heatmap-",contraste,".pdf", sep="")
    
    design.contrast<-design.matrix[,rownames(cont.matrix)[cont.matrix!=0]]
    design.contrast<-design.contrast[apply(design.contrast,1,sum)!=0,]
    
    data.Cont=extractMatrix(rownames(design.contrast),ndata)
    
    datos.clus <- data.Cont[indx.selected,]
    
    sym <- DB.ordered.rat[indx.selected,3]
    mirna.name<-DB.ordered.rat[indx.selected,4]
    mirna.type<-DB.ordered.rat[indx.selected,5]

    genelabels <- paste(sym, " (", mirna.name, ")", sep="")
    
    pdf(file=file.name,height=15,width=12)
    
    par(oma = c(3,1,3,4),mar=c(12,5,2,2)+0.1)


    datos.clus<-as.matrix(datos.clus)
    mode(datos.clus)<-"numeric"

    ind.hmap <- heatmap.2(datos.clus, col=greenred(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=.55, cexCol=.7, main = contraste, labRow = genelabels,ColSideColors=colores[colnames(ndata.numeric)%in%colnames(datos.clus)])
    ######, ColSideColors = mycol[c(array.indx)$values] 
  
    Idx <- ind.hmap$rowInd[length(ind.hmap$rowInd):1] 
    genes.hmap <- cbind(sym,mirna.name,mirna.type, selected[Idx,],datos.clus[Idx,])
    dev.off()             

    file.name <- paste("ListaHeatmapGenes-",contraste,".txt", sep="")
    write.table(genes.hmap, file=file.name,sep="\t",quote=F)

    
    #-----------------------------------------------------------
    #Hierarchical Clustering and Heatmaps with all Microarrays
    #-----------------------------------------------------------

    
  ##heatmap with all microarrays
    file.name <- paste("Heatmap.All-",contraste,".pdf", sep="")
  
    pdf(file=file.name,height=15,width=10)
  
    datos.clus.all <- ndata[indx.selected,]
  
    mode(datos.clus.all)<-"numeric"
      
    par(oma = c(3,1,3,4),mar=c(12,5,2,2)+0.1)
    ind.hmap <- heatmap.2(as.matrix(datos.clus.all), col=greenred(75), scale="row", key=TRUE, symkey=FALSE, density.info="none", trace="none", cexRow=.55, cexCol=.7, main = contraste, labRow = genelabels,ColSideColors = colores)
    Idx <- ind.hmap$rowInd[length(ind.hmap$rowInd):1] 
     genes.hmap <- cbind(sym,mirna.name,mirna.type, selected[Idx,],datos.clus.all[Idx,])
   
    dev.off()
  
   file.name <- paste("ListaHMapAllGenes-",contraste,".txt", sep="")
   write.table(genes.hmap, file=file.name,sep="\t",quote=F)
    
    }


}





contraste="FSH-Basal"
differentialAnalysismiRNA(mirna.rat.expr,contraste,diseno, M, B)

contraste="LNG-Basal"
differentialAnalysismiRNA(mirna.rat.expr,contraste,diseno, M, B)

contraste="LNG-FSH"
#differentialAnalysismiRNA(onlyrat.quantile,contraste,design.matrix, M, B)


differentialAnalysismiRNA(mirna.rat.expr,contraste,diseno, M, B)




x<-get.multimir(mirna=mirna.name,table="mirtarbase")[[1]]


x<-x[x$target_entrez%in%rownames(condensado[[1]]),]
correlacion<-NULL
for(i in 1:nrow(x)){
  identrez<-x[i,"target_entrez"]
  idmirna<-x[i,"mature_mirna_id"]
  mirna.iter<-condensado[[2]][match(DB.ordered.rat$Probe.Set.Name[DB.ordered.rat$Transcript.ID.Array.Design. %in% idmirna],rownames(condensado[[2]])),]
  correlacion<-rbind(correlacion,c(idmirna,condensado[[1]][rownames(condensado[[1]])%in%identrez,],mirna.iter))
}

mirna.exp.correlacion<-condensado[[1]][colnames(condensado[[1]])%in%x$target_entrez[x$target_entrez%in%colnames(condensado[[1]])],]

mirna.exp.correlacion[,2]<-x$mature_mirna_id[match(mirna.exp.correlacion[,2],x$target_entrez[x$target_entrez%in%entrez])]



condensado<-list(expression=cbind(apply(exp.ndata[,3:6],1,mean),apply(exp.ndata[,7:10],1,mean),apply(exp.ndata[,11:14],1,mean)),mirna=cbind(apply(ndata.mirna.rat[,1:4],1,mean),apply(ndata.mirna.rat[,5:8],1,mean),apply(ndata.mirna.rat[,9:12],1,mean)))



rownames(condensado[[1]])<-exp.ndata[,2]

colnames(condensado[[1]])<-c("Peri","Sano","Intra")
colnames(condensado[[2]])<-c("Peri","Sano","Intra")

