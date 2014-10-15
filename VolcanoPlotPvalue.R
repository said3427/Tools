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

