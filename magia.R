library(oligo)
rawData = read.celfiles(< character vector of celfiles >)

2. perform RMA and get "transcript cluster" summarized data back
 using only "core" genes ("safely" annotated genes according to affy)
 this is the default in oligo.

Eset = rma(rawData,target="core")

3. Load annotation package and annotate "transcript clusters" with
some
stuff contained in that package.

## load Annotation package
library("hugene20sttranscriptcluster.db")

        annotateGene = function ( db , what , missing ) {
        tab = toTable(db[intersect(featureNames(Eset),
mappedkeys(db)) ])
        mt = match ( featureNames ( Eset ) , tab$probe_id )
        ifelse ( is.na(mt), missing , tab[[ what ]][ mt ])
        }


fData(Eset)$symbol = annotateGene( hugene20sttranscriptclusterSYMBOL
,"symbol" , missing = NA )
fData(Eset)$genename = annotateGene(
hugene20sttranscriptclusterGENENAME , "gene_name" , missing = NA )
fData(Eset)$ensembl = annotateGene( hugene20sttranscriptclusterENSEMBL
, "ensembl_id" , missing = NA )


4. After that keep only the "transcript clusters"  that have a ENSEMBL
Gene ID.
(for example)








quitarduplicados<-function(matriz){
	unicos<-unique(matriz[,1])
	ids<-matriz[,1]
	nodup<-NULL
	for(i in unicos){
		index<-grep(i,ids)
		if(length(index)>1){
			mediana<-apply(matriz[index,-1],2,median)
			nodup<-rbind(nodup,c(i,mediana))
		} else(nodup<-rbind(nodup,matriz[index,]))

	}

	return(nodup)

}


analysis ID.
4c3b01a0a8771b53dc50c29fd08a679563e0a646ef8e3b5ba972e8cc1f7eb72e 


ID.
a9316389151035bcc6c90c886be8af23526e3967a53fb89c984742ebbd9ec582

http://gencomp.bio.unipd.it/magia/results/a9316389151035bcc6c90c886be8af23526e3967a53fb89c984742ebbd9ec582


