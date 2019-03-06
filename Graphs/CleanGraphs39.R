### plotting functions

.barplot <- function(column, variable, trueNgroups = 1, dontPlotData = FALSE) {
    
    if (dontPlotData) {
        
        plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, xlab = "", 
            ylab = "")
        
        axis(1, at = 0:1, labels = FALSE, cex.axis = 1.4, xlab = "")
        axis(2, at = 0:1, labels = FALSE, cex.axis = 1.4, ylab = "")
        
        mtext(text = variable, side = 1, cex = 1.5, line = 3)
        
        return()
    }
    
    maxFrequency <- max(summary(column))
    
    i <- 1
    step <- 1
    
    while (maxFrequency/step > 9) {
        
        if (i == 2) {
            
            step <- 2 * step
            i <- i + 1
            
        } else if (i%%3 == 0) {
            
            step <- 2.5 * step
            i <- i + 1
            
        } else {
            
            step <- 2 * step
            i <- i + 1
        }
        
    }
    
    yticks <- 0
    
    while (yticks[length(yticks)] < maxFrequency) {
        
        yticks <- c(yticks, yticks[length(yticks)] + step)
    }
    
    
    yLabs <- vector("character", length(yticks))
    
    for (i in seq_along(yticks)) {
        
        if (yticks[i] < 10^6) {
            
            yLabs[i] <- format(yticks[i], digits = 3, scientific = FALSE)
            
        } else {
            
            yLabs[i] <- format(yticks[i], digits = 3, scientific = TRUE)
        }
    }
    
    distLab <- max(nchar(yLabs))/1.8
    
    par(mar = c(5, 7.2, 4, 2) + 0.1)
    nGroups <- barplot(summary(column), cex.names = 1.3, axes = FALSE, ylim = range(yticks), 
        plot = FALSE)
    col <- ifelse(seq_along(nGroups) == trueNgroups, "darkred", "grey")
    barplot(summary(column), cex.names = 1.3, axes = FALSE, ylim = range(yticks), col = col)
    axis(2, las = 1, at = yticks, labels = yLabs, cex.axis = 1.4)
    mtext(text = variable, side = 1, cex = 1.5, line = 3)
    mtext(text = "Frequency", side = 2, cex = 1.5, line = distLab + 2, las = 0)
}

plotCoocurrenceHeatMap <- function(coocurrenceProbs, offsetX = 0.3, offsetY = 0, cexXlab = 1.6, 
    cexYlab = 1.6, lwd = 4) {
    
    parameterLabel1 <- "Observations"
    parameterLabel2 <- "Observations"
    
    op <- par(mar = c(5, 6, 3, 4) + 0.1, las = 1)
    
    N <- ncol(coocurrenceProbs)
    
    xRange <- c(0, N)
    yRange <- c(0, N)
    
    image(x = seq_len(N), y = seq_len(N), z = t(coocurrenceProbs), col = topo.colors(12), 
        xlab = "", ylab = "", las = 1, cex.axis = 1.2)
    
    nRect <- N/50
    
    for (nr in seq_len(nRect)) {
        
        rect(1 + (nr - 1) * 50, 1 + (nr - 1) * 50, nr * 50, nr * 50, col = "darkred", 
            density = 0, lwd = lwd)
        
    }
    
    mtext(parameterLabel1, 1, cex = cexXlab, line = 2.5 + offsetX)
    mtext(parameterLabel2, 2, cex = cexYlab, line = 3.2 + offsetY, las = 0)
    
    par(op)
    
}

### 3 groups
load("samplesDPrecover3groups.Rdata")
load("coocurrenceProbs3.Rdata")

op <- par(mfrow = c(1, 2))

index3 <- paste("z[", seq_len(150), "]", sep = "")
zMatrix3 <- samplesDPrecover3groups$BUGSoutput$sims.matrix[, index3]
nGroupsPerIteration3 <- apply(zMatrix3, 1, function(x) length(unique(x)))
.barplot(as.factor(nGroupsPerIteration3), "Number of groups", trueNgroups = 1)

plotCoocurrenceHeatMap(coocurrenceProbs3)

par(op)