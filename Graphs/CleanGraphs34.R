library(BayesFactor)
library(plotrix)

.plotSequentialBF.ttest <- function(x = NULL, y = NULL, paired = FALSE, 
    BF10post, formula = NULL, data = NULL, rscale = 1, oneSided = FALSE, 
    lwd = 2, cexPoints = 1.4, cexAxis = 1.2, cexYlab = 1.5, cexXlab = 1.6, 
    cexTextBF = 1.4, cexText = 1.2, cexLegend = 1.2, cexEvidence = 1.6, 
    lwdAxis = 1.2, plotDifferentPriors = FALSE, BFH1H0 = TRUE, dontPlotData = FALSE, 
    level1 = NULL, level2 = NULL, subDataSet = NULL) {
    
    #### settings ####
    
    if (!plotDifferentPriors) {
        
        evidenceText <- TRUE
    } else {
        
        evidenceText <- FALSE
    }
    
    
    if (rscale == "medium") {
        
        r <- sqrt(2)/2
    }
    
    if (rscale == "wide") {
        
        r <- 1
    }
    
    if (rscale == "ultrawide") {
        
        r <- sqrt(2)
    }
    
    if (mode(rscale) == "numeric") {
        
        r <- rscale
    }
    
    
    if (oneSided == FALSE) {
        
        nullInterval <- NULL
    }
    
    if (oneSided == "right") {
        
        nullInterval <- c(0, Inf)
    }
    
    if (oneSided == "left") {
        
        nullInterval <- c(-Inf, 0)
    }
    
    
    par(mar = c(5.6, 6, 7, 7) + 0.1, las = 1)
    
    
    if (dontPlotData) {
        
        plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, 
            xlab = "", ylab = "")
        
        axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, 
            xlab = "")
        axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, 
            ylab = "")
        
        mtext("n", side = 1, cex = cexXlab, line = 2.5)
        
        if (oneSided == FALSE) {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF[1][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0][1]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            }
        }
        
        if (oneSided == "right") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["+"][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["+"]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            }
        }
        
        if (oneSided == "left") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["-"][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["-"]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            }
        }
        
        return()
    }
    
    
    if (is.null(y) || paired) {
        
        BF10 <- vector("numeric", max(length(x), length(y)))
        BF10w <- vector("numeric", max(length(x), length(y)))
        BF10u <- vector("numeric", max(length(x), length(y)))
        
        idData <- 1
        
        if (is.null(y)) {
            
            ind <- which(x == x[1])
            idData <- sum((ind + 1) - (1:(length(ind))) == 1)
            
        } else {
            
            idData <- 1
            
            
            for (i in 2:(min(c(length(x), length(y))))) {
                
                previous <- c(x[i - 1], y[i - 1])
                
                if (all(c(x[i], y[i]) == previous)) {
                  
                  idData <- idData + 1
                  
                } else if (x[i] == y[i]) {
                  
                  idData <- idData + 1
                  
                } else {
                  
                  break
                }
            }
        }
        
        BF10[1:idData] <- 1
        BF10w[1:idData] <- 1
        BF10u[1:idData] <- 1
        
        
        if (idData < length(x)) {
            
            i <- idData + 1
            
        } else {
            
            i <- idData
            
        }
        
        if (idData < length(y)) {
            
            j <- idData + 1
            
        } else {
            
            j <- idData
            
        }
        
        k <- idData + 1
        
        
        while ((i <= length(x) | j <= length(y)) & k <= length(BF10)) {
            
            if (oneSided == FALSE) {
                
                BF <- BayesFactor::ttestBF(x = x[1:i], y = y[1:j], paired = paired, 
                  rscale = r, nullInterval = nullInterval)
                BF10[k] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, 
                  "bf"]
                
            } else {
                
                BF10[k] <- .oneSidedTtestBFRichard(x = x[1:i], y = y[1:j], 
                  paired = paired, r = r, oneSided = oneSided)
            }
            
            k <- k + 1
            
            if (i < length(x)) {
                
                i <- i + 1
            }
            if (j < length(y)) {
                
                j <- j + 1
            }
        }
        
        
        BF10 <- BF10[is.finite(BF10)]
        
        if (plotDifferentPriors) {
            
            if (idData < length(x)) {
                
                i <- idData + 1
                
            } else {
                
                i <- idData
                
            }
            
            if (idData < length(y)) {
                
                j <- idData + 1
                
            } else {
                
                j <- idData
                
            }
            
            k <- idData + 1
            
            
            while ((i <= length(x) | j <= length(y)) & k <= length(BF10u)) {
                
                if (oneSided == FALSE) {
                  
                  BF <- BayesFactor::ttestBF(x = x[1:i], y = y[1:j], paired = paired, 
                    rscale = "ultrawide", nullInterval = nullInterval)
                  BF10u[k] <- BayesFactor::extractBF(BF, logbf = FALSE, 
                    onlybf = F)[1, "bf"]
                  
                } else {
                  
                  BF10u[k] <- .oneSidedTtestBFRichard(x = x[1:i], y = y[1:j], 
                    paired = paired, r = "ultrawide", oneSided = oneSided)
                }
                
                k <- k + 1
                
                if (i < length(x)) {
                  
                  i <- i + 1
                }
                if (j < length(y)) {
                  
                  j <- j + 1
                }
            }
            
            BF10u <- BF10u[is.finite(BF10u)]
            
            if (idData < length(x)) {
                
                i <- idData + 1
                
            } else {
                
                i <- idData
                
            }
            
            if (idData < length(y)) {
                
                j <- idData + 1
                
            } else {
                
                j <- idData
                
            }
            
            k <- idData + 1
            
            
            while ((i <= length(x) | j <= length(y)) & k <= length(BF10w)) {
                
                if (oneSided == FALSE) {
                  
                  BF <- BayesFactor::ttestBF(x = x[1:i], y = y[1:j], paired = paired, 
                    rscale = "wide", nullInterval = nullInterval)
                  BF10w[k] <- BayesFactor::extractBF(BF, logbf = FALSE, 
                    onlybf = F)[1, "bf"]
                  
                } else {
                  
                  BF10w[k] <- .oneSidedTtestBFRichard(x = x[1:i], y = y[1:j], 
                    paired = paired, r = "wide", oneSided = oneSided)
                }
                
                k <- k + 1
                
                if (i < length(x)) {
                  
                  i <- i + 1
                }
                if (j < length(y)) {
                  
                  j <- j + 1
                }
            }
            
            BF10w <- BF10w[is.finite(BF10w)]
            
        }
        
    } else if (!is.null(y) && !paired) {
        
        idData <- 1
        
        xx <- numeric()
        yy <- numeric()
        
        BF10 <- vector("numeric", nrow(subDataSet))
        BF10w <- vector("numeric", nrow(subDataSet))
        BF10u <- vector("numeric", nrow(subDataSet))
        
        for (i in seq_len(nrow(subDataSet))) {
            
            if (subDataSet[i, 2] == level1) {
                
                xx <- c(xx, subDataSet[i, 1])
                
            } else if (subDataSet[i, 2] == level2) {
                
                yy <- c(yy, subDataSet[i, 1])
                
            }
            
            if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || sd(yy) > 
                0)) {
                
                if (oneSided == FALSE) {
                  
                  BF <- BayesFactor::ttestBF(x = xx, y = yy, paired = paired, 
                    rscale = r, nullInterval = nullInterval)
                  BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, 
                    onlybf = F)[1, "bf"]
                  
                } else if (oneSided == "right") {
                  
                  BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = "right", 
                    r = r)
                  
                } else if (oneSided == "left") {
                  
                  BF10[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = "left", 
                    r = r)
                }
                
            } else {
                
                BF10[i] <- 1
            }
        }
        
        
        if (plotDifferentPriors) {
            
            xx <- numeric()
            yy <- numeric()
            
            for (i in seq_len(nrow(subDataSet))) {
                
                if (subDataSet[i, 2] == level1) {
                  
                  xx <- c(xx, subDataSet[i, 1])
                  
                } else if (subDataSet[i, 2] == level2) {
                  
                  yy <- c(yy, subDataSet[i, 1])
                  
                }
                
                if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || 
                  sd(yy) > 0)) {
                  
                  if (oneSided == FALSE) {
                    
                    BF <- BayesFactor::ttestBF(x = xx, y = yy, paired = paired, 
                      rscale = "ultrawide", nullInterval = nullInterval)
                    BF10u[i] <- BayesFactor::extractBF(BF, logbf = FALSE, 
                      onlybf = F)[1, "bf"]
                    
                  } else if (oneSided == "right") {
                    
                    BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = "right", 
                      r = "ultrawide")
                    
                  } else if (oneSided == "left") {
                    
                    BF10u[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = "left", 
                      r = "ultrawide")
                  }
                  
                } else {
                  
                  BF10u[i] <- 1
                }
            }
            
            xx <- numeric()
            yy <- numeric()
            
            for (i in seq_len(nrow(subDataSet))) {
                
                if (subDataSet[i, 2] == level1) {
                  
                  xx <- c(xx, subDataSet[i, 1])
                  
                } else if (subDataSet[i, 2] == level2) {
                  
                  yy <- c(yy, subDataSet[i, 1])
                  
                }
                
                if (length(xx) > 1 && length(yy) > 1 && (sd(xx) > 0 || 
                  sd(yy) > 0)) {
                  
                  if (oneSided == FALSE) {
                    
                    BF <- BayesFactor::ttestBF(x = xx, y = yy, paired = paired, 
                      rscale = "wide", nullInterval = nullInterval)
                    BF10w[i] <- BayesFactor::extractBF(BF, logbf = FALSE, 
                      onlybf = F)[1, "bf"]
                    
                  } else if (oneSided == "right") {
                    
                    BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = "right", 
                      r = "wide")
                    
                  } else if (oneSided == "left") {
                    
                    BF10w[i] <- .oneSidedTtestBFRichard(xx, yy, oneSided = "left", 
                      r = "wide")
                  }
                  
                } else {
                  
                  BF10w[i] <- 1
                }
            }
        }
    }
    
    ####################### scale y axis ###########################
    
    if (plotDifferentPriors) {
        
        BF <- c(BF10, BF10u, BF10w)
        
    } else {
        
        BF <- BF10
        
    }
    
    
    if (!BFH1H0) {
        
        BF <- 1/BF
        BF10 <- 1/BF10
        
        if (plotDifferentPriors) {
            
            BF10u <- 1/BF10u
            BF10w <- 1/BF10w
        }
    }
    
    
    # y-axis labels larger than 1
    
    y1h <- "1"
    
    i <- 1
    
    while (eval(parse(text = y1h[i])) < max(BF)) {
        
        if (grepl(pattern = "e", y1h[i])) {
            
            newy <- paste(strsplit(y1h[i], split = "+", fixed = TRUE)[[1]][1], 
                "+", as.numeric(strsplit(y1h[i], split = "+", fixed = TRUE)[[1]][2]) + 
                  1, sep = "")
        } else {
            
            newy <- paste(y1h[i], "0", sep = "")
        }
        
        if (eval(parse(text = newy)) >= 10^6) {
            
            newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
        }
        
        y1h <- c(y1h, newy)
        i <- i + 1
    }
    
    
    y3h <- "3"
    
    i <- 1
    
    while (eval(parse(text = y3h[i])) < max(BF)) {
        
        if (grepl(pattern = "e", y3h[i])) {
            
            newy <- paste(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][1], 
                "+", as.numeric(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][2]) + 
                  1, sep = "")
        } else {
            
            newy <- paste(y3h[i], "0", sep = "")
        }
        
        if (as.numeric(newy) >= 10^6) {
            
            newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
        }
        
        y3h <- c(y3h, newy)
        
        i <- i + 1
    }
    
    yhigh <- vector("numeric", length(y1h) + length(y3h))
    
    o <- 1
    e <- 1
    
    for (i in seq_along(yhigh)) {
        
        if (i%%2 == 1) {
            
            yhigh[i] <- y1h[o]
            o <- o + 1
        }
        
        if (i%%2 == 0) {
            
            yhigh[i] <- y3h[e]
            e <- e + 1
        }
    }
    
    yhighLab <- as.character(yhigh)
    
    
    # y-axis labels smaller than 1
    
    y1l <- "1/1"
    
    i <- 1
    
    while (eval(parse(text = y1l[i])) > min(BF)) {
        
        if (grepl(pattern = "e", y1l[i])) {
            
            newy <- paste(strsplit(y1l[i], split = "+", fixed = TRUE)[[1]][1], 
                "+", as.numeric(strsplit(y1l[i], split = "+", fixed = TRUE)[[1]][2]) + 
                  1, sep = "")
        } else {
            
            newy <- paste(y1l[i], "0", sep = "")
        }
        
        if (eval(parse(text = newy)) <= 10^(-6)) {
            
            newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
            newy <- sub("-", "+", x = newy)
            newy <- paste0("1/", newy)
        }
        
        y1l <- c(y1l, newy)
        i <- i + 1
    }
    
    
    y3l <- "1/3"
    
    i <- 1
    
    while (eval(parse(text = y3l[i])) > min(BF)) {
        
        if (grepl(pattern = "e", y3l[i])) {
            
            newy <- paste(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][1], 
                "+", as.numeric(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][2]) + 
                  1, sep = "")
        } else {
            
            newy <- paste(y3l[i], "0", sep = "")
        }
        
        if (newy == "1/3e+9") {
            
            newy <- "1/3e+09"
        }
        
        if (eval(parse(text = newy)) <= 10^(-6) & eval(parse(text = newy)) > 
            10^(-9)) {
            
            newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
            newy <- paste(substring(newy, 1, nchar(newy) - 1), as.numeric(substring(newy, 
                nchar(newy), nchar(newy))) - 1, sep = "")
            newy <- sub(".33", "", newy)
            newy <- sub("-", "+", x = newy)
            newy <- paste0("1/", newy)
        }
        
        y3l <- c(y3l, newy)
        i <- i + 1
    }
    
    ylow <- vector("numeric", length(y1l) + length(y3l))
    o <- 1
    e <- 1
    
    for (i in seq_along(ylow)) {
        
        if (i%%2 == 1) {
            
            ylow[i] <- y1l[o]
            o <- o + 1
        }
        
        if (i%%2 == 0) {
            
            ylow[i] <- y3l[e]
            e <- e + 1
        }
    }
    
    yLab <- c(rev(ylow[-1]), yhighLab)
    
    
    # remove 3's if yLab vector is too long
    omit3s <- FALSE
    
    if (length(yLab) > 9) {
        
        omit3s <- TRUE
        
        ind <- which(yLab == "3")
        
        yLabsHigh <- yLab[ind:length(yLab)]
        
        if (length(yLabsHigh) > 1) {
            
            yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh), 2)]
        } else {
            
            yLabsHigh <- character(0)
        }
        
        yLabsLow <- yLab[1:(ind - 1)]
        yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
        
        yLab1s <- c(yLabsLow, yLabsHigh)
        
        
        if (max(BF) > eval(parse(text = yLab1s[length(yLab1s)]))) {
            
            for (i in 1:2) {
                
                if (grepl(pattern = "e", yLab1s[length(yLab1s)])) {
                  
                  newy <- paste(strsplit(yLab1s[length(yLab1s)], split = "+", 
                    fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], 
                    split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
                } else {
                  
                  newy <- paste(yLab1s[length(yLab1s)], "0", sep = "")
                }
                
                if (eval(parse(text = newy)) >= 10^6) {
                  
                  newy <- format(eval(parse(text = newy)), digits = 3, 
                    scientific = TRUE)
                }
                
                yLab1s <- c(yLab1s, newy)
            }
        }
        
        
        if (yLab1s[1] == "1") {
            
            yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
        }
        
        if (yLab1s[length(yLab1s)] == "1") {
            
            yLab1s <- c(yLab1s, "10")
        }
        
        if (min(BF) < eval(parse(text = yLab1s[1]))) {
            
            for (i in 1:2) {
                
                if (grepl(pattern = "e", yLab1s[1])) {
                  
                  newy <- paste(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][1], 
                    "+", as.numeric(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][2]) + 
                      1, sep = "")
                } else {
                  
                  newy <- paste(yLab1s[1], "0", sep = "")
                }
                
                if (eval(parse(text = newy)) <= 10^(-6)) {
                  
                  newy <- format(eval(parse(text = newy)), digits = 3, 
                    scientific = TRUE)
                  newy <- sub("-", "+", x = newy)
                  newy <- substring(newy, nchar(newy) - 4, nchar(newy))
                  newy <- paste0("1/", newy)
                }
            }
            
            yLab1s <- c(newy, yLab1s)
        }
        
        yLab <- yLab1s
    }
    
    while (length(yLab) > 9) {
        
        ind <- which(yLab == "1")
        
        if (ind == 1) {
            
            yLabLow <- character(0)
        } else {
            
            yLabLow <- yLab[1:(ind - 1)]
        }
        
        if (ind == length(yLab)) {
            
            yLabHigh <- character(0)
        } else {
            
            yLabHigh <- yLab[(ind + 1):length(yLab)]
        }
        
        if (length(yLabLow) > 1) {
            
            yLabLow <- yLabLow[seq(length(yLabLow) - 1, 1, -2)]
        } else {
            
            yLabLow <- yLabLow
        }
        
        
        if (length(yLabHigh) > 1) {
            
            yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
        } else {
            
            yLabHigh <- yLabHigh
        }
        
        if (length(yLabLow) == 1) {
            
            yLabLow <- paste("1/", yLabHigh[1], sep = "")
        }
        
        if (length(yLabHigh) == 1) {
            
            yLabHigh <- strsplit(x = yLabLow[1], "/", fixed = TRUE)[[1]][2]
        }
        
        yLab <- c(rev(yLabLow), "1", yLabHigh)
    }
    
    
    while (eval(parse(text = yLab[1])) > min(BF)) {
        
        for (i in 1:2) {
            
            interval <- as.numeric(strsplit(yLab[1], "+", fixed = TRUE)[[1]][2]) - 
                as.numeric(strsplit(yLab[2], "+", fixed = TRUE)[[1]][2])
            pot <- as.numeric(strsplit(yLab[1], "+", fixed = TRUE)[[1]][2]) + 
                interval
            
            newy <- paste(strsplit(yLab[1], "+", fixed = TRUE)[[1]][1], 
                "+", pot, sep = "")
            yLab <- c(newy, yLab)
        }
    }
    
    while (eval(parse(text = yLab[length(yLab)])) < max(BF)) {
        
        for (i in 1:2) {
            
            interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][2]) - 
                as.numeric(strsplit(yLab[length(yLab) - 1], "+", fixed = TRUE)[[1]][2])
            pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][2]) + 
                interval
            newy <- paste(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][1], 
                "+", pot, sep = "")
            yLab <- c(yLab, newy)
        }
    }
    
    yAt <- vector("numeric", length(yLab))
    
    for (i in seq_along(yLab)) {
        
        yAt[i] <- log(eval(parse(text = yLab[i])))
    }
    
    
    ####################### plot ###########################
    
    xLab <- pretty(c(0, length(BF10) + 2))
    xlim <- range(xLab)
    ylow <- log(eval(parse(text = yLab[1])))
    yhigh <- log(eval(parse(text = yLab[length(yLab)])))
    
    if (is.infinite(yhigh)) {
        
        yhigh <- 1e+308
    }
    
    
    ylim <- c(ylow, yhigh)
    
    plot(1, 1, xlim = xlim, ylim = ylim, ylab = "", xlab = "", type = "n", 
        axes = FALSE)
    
    
    for (i in seq_along(yAt)) {
        
        lines(x = xlim, y = rep(yAt[i], 2), col = "darkgrey", lwd = 1.3, 
            lty = 2)
    }
    
    lines(xlim, rep(0, 2), lwd = lwd)
    
    axis(1, at = xLab, labels = xLab, cex.axis = cexAxis, lwd = lwdAxis)
    axis(2, at = yAt, labels = yLab, cex.axis = cexAxis, lwd = lwdAxis)
    
    # enable plotting in margin
    par(xpd = TRUE)
    xx <- grconvertX(0.79, "ndc", "user")
    
    yAthigh <- yAt[yAt >= 0]
    
    if (!omit3s & eval(parse(text = yLab[1])) >= 1/300 & eval(parse(text = yLab[length(yLab)])) <= 
        300) {
        
        for (i in 1:(length(yAthigh) - 1)) {
            yy <- mean(c(yAthigh[i], yAthigh[i + 1]))
            
            if (yAthigh[i] == log(1)) {
                text(x = xx, yy, "Anecdotal", pos = 4, cex = cexText)
            }
            if (yAthigh[i] == log(3)) {
                text(x = xx, yy, "Moderate", pos = 4, cex = cexText)
            }
            if (yAthigh[i] == log(10)) {
                text(x = xx, yy, "Strong", pos = 4, cex = cexText)
            }
            if (yAthigh[i] == log(30)) {
                text(x = xx, yy, "Very strong", pos = 4, cex = cexText)
            }
            if (yAthigh[i] == log(100)) {
                text(x = xx, yy, "Extreme", pos = 4, cex = cexText)
            }
        }
        
        yAtlow <- rev(yAt[yAt <= 0])
        
        for (i in 1:(length(yAtlow) - 1)) {
            
            yy <- mean(c(yAtlow[i], yAtlow[i + 1]))
            
            if (yAtlow[i] == log(1)) {
                text(x = xx, yy, "Anecdotal", pos = 4, cex = cexText)
            }
            if (yAtlow[i] == log(1/3)) {
                text(x = xx, yy, "Moderate", pos = 4, cex = cexText)
            }
            if (yAtlow[i] == log(1/10)) {
                text(x = xx, yy, "Strong", pos = 4, cex = cexText)
            }
            if (yAtlow[i] == log(1/30)) {
                text(x = xx, yy, "Very strong", pos = 4, cex = cexText)
            }
            if (yAtlow[i] == log(1/100)) {
                text(x = xx, yy, "Extreme", pos = 4, cex = cexText)
            }
        }
        
        axis(side = 4, at = yAt, tick = TRUE, las = 2, cex.axis = cexAxis, 
            lwd = lwdAxis, labels = FALSE, line = -0.6)
        
        xx <- grconvertX(0.96, "ndc", "user")
        yy <- grconvertY(0.5, "npc", "user")
        
        text(xx, yy, "Evidence", srt = -90, cex = cexEvidence)
    }
    
    if (omit3s) {
        
        if (oneSided == FALSE) {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF[1][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 4.3)
            } else {
                
                mtext(text = expression(BF[0][1]), side = 2, las = 0, 
                  cex = cexYlab, line = 4.3)
            }
        }
        
        if (oneSided == "right") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["+"][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 4.3)
            } else {
                
                mtext(text = expression(BF[0]["+"]), side = 2, las = 0, 
                  cex = cexYlab, line = 4.3)
            }
        }
        
        if (oneSided == "left") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["-"][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 4.3)
            } else {
                
                mtext(text = expression(BF[0]["-"]), side = 2, las = 0, 
                  cex = cexYlab, line = 4.3)
            }
        }
    }
    
    if (omit3s == FALSE) {
        
        if (oneSided == FALSE) {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF[1][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0][1]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            }
        }
        
        if (oneSided == "right") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["+"][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["+"]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            }
        }
        
        if (oneSided == "left") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["-"][0]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["-"]), side = 2, las = 0, 
                  cex = cexYlab, line = 3.1)
            }
        }
    }
    
    mtext("n", side = 1, cex = cexXlab, line = 2.5)
    
    xx <- grconvertX(0.1, "npc", "user")
    yy1 <- yAt[length(yAt) - 1]
    yy2 <- yAt[length(yAt)]
    yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
    yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
    
    arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)
    
    xxt <- grconvertX(0.28, "npc", "user")
    
    if (oneSided == FALSE) {
        
        if (BFH1H0) {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", 
                cex = cexText)
        } else {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", 
                cex = cexText)
        }
    }
    
    if (oneSided == "right") {
        
        if (BFH1H0) {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", 
                cex = cexText)
        } else {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", 
                cex = cexText)
        }
    }
    
    if (oneSided == "left") {
        
        if (BFH1H0) {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", 
                cex = cexText)
        } else {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", 
                cex = cexText)
        }
    }
    
    
    yy1 <- yAt[2]
    yy2 <- yAt[1]
    yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
    yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
    
    arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)
    
    if (oneSided == FALSE) {
        
        if (BFH1H0) {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", 
                cex = cexText)
        } else {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", 
                cex = cexText)
        }
    }
    
    if (oneSided == "right") {
        
        if (BFH1H0) {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", 
                cex = cexText)
        } else {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", 
                cex = cexText)
        }
    }
    
    if (oneSided == "left") {
        
        if (BFH1H0) {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", 
                cex = cexText)
        } else {
            
            text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", 
                cex = cexText)
        }
    }
    
    
    # display BF10 value
    if (idData < length(BF10)) {
        
        BF10e <- BF10post
        
    } else {
        
        BF10e <- 1
    }
    
    if (BFH1H0) {
        
        BF01e <- 1/BF10e
        
    } else {
        
        BF01e <- BF10e
        BF10e <- 1/BF01e
    }
    
    # display BF10 value
    
    offsetTopPart <- 0.06
    
    xx <- min(xLab)
    yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
    
    if (BF10e >= 1000000 | BF01e >= 1000000) {
        
        BF10t <- formatC(BF10e, 3, format = "e")
        BF01t <- formatC(BF01e, 3, format = "e")
    }
    
    if (BF10e < 1000000 & BF01e < 1000000) {
        
        BF10t <- formatC(BF10e, 3, format = "f")
        BF01t <- formatC(BF01e, 3, format = "f")
    }
    
    if (oneSided == FALSE) {
        
        text(xx, yy2, bquote(BF[10] == .(BF10t)), cex = cexTextBF, pos = 4, 
            offset = -0.2)
        text(xx, yy, bquote(BF[0][1] == .(BF01t)), cex = cexTextBF, pos = 4, 
            offset = -0.2)
    }
    
    if (oneSided == "right") {
        
        text(xx, yy2, bquote(BF["+"][0] == .(BF10t)), cex = cexTextBF, 
            pos = 4, offset = -0.2)
        text(xx, yy, bquote(BF[0]["+"] == .(BF01t)), cex = cexTextBF, 
            pos = 4, offset = -0.2)
    }
    
    if (oneSided == "left") {
        
        text(xx, yy2, bquote(BF["-"][0] == .(BF10t)), cex = cexTextBF, 
            pos = 4, offset = -0.2)
        text(xx, yy, bquote(BF[0]["-"] == .(BF01t)), cex = cexTextBF, 
            pos = 4, offset = -0.2)
    }
    
    
    # probability wheel
    
    if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
        xx <- grconvertX(0.44, "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 5) {
        xx <- grconvertX(0.44 + 0.001 * 5, "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 6) {
        xx <- grconvertX(0.44 + 0.001 * 6, "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 7) {
        xx <- grconvertX(0.44 + 0.002 * max(nchar(BF10t), nchar(BF01t)), 
            "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) == 8) {
        xx <- grconvertX(0.44 + 0.003 * max(nchar(BF10t), nchar(BF01t)), 
            "ndc", "user")
    }
    
    if (max(nchar(BF10t), nchar(BF01t)) > 8) {
        xx <- grconvertX(0.445 + 0.005 * max(nchar(BF10t), nchar(BF01t)), 
            "ndc", "user")
    }
    
    yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
    
    
    # make sure that colored area is centered
    
    radius <- grconvertX(0.2, "ndc", "user") - grconvertX(0.16, "ndc", 
        "user")
    A <- radius^2 * pi
    alpha <- 2/(BF01e + 1) * A/radius^2
    startpos <- pi/2 - alpha/2
    
    # draw probability wheel
    
    plotrix::floating.pie(xx, yy, c(BF10e, 1), radius = radius, col = c("darkred", 
        "white"), lwd = 2, startpos = startpos)
    
    yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
    
    if (oneSided == FALSE) {
        
        text(xx, yy, "data|H1", cex = 1.1)
        text(xx, yy2, "data|H0", cex = 1.1)
    }
    
    if (oneSided == "right") {
        
        text(xx, yy, "data|H+", cex = 1.1)
        text(xx, yy2, "data|H0", cex = 1.1)
    }
    
    if (oneSided == "left") {
        
        text(xx, yy, "data|H-", cex = 1.1)
        text(xx, yy2, "data|H0", cex = 1.1)
    }
    
    if (length(BF10) <= 60) {
        
        points(log(BF10), pch = 21, bg = "grey", cex = cexPoints, lwd = 1.3)  # user prior
    } else {
        
        lines(log(BF10), col = "black", lwd = 2.7)  # user prior
    }
    
    if (plotDifferentPriors) {
        
        if (length(BF10) <= 60) {
            
            points(log(BF10u), pch = 21, bg = "white", cex = 0.7, lwd = 1.3)  # 'ultrawide' prior
            points(log(BF10w), pch = 21, bg = "black", cex = 0.7, lwd = 1.3)  # 'wide' prior
            
        } else {
            
            greycol <- rgb(0, 0, 0, alpha = 0.95)
            greycol2 <- rgb(0, 0, 0, alpha = 0.5)
            lines(log(BF10u), col = greycol2, cex = 0.7, lwd = 1.3, lty = 1)  # 'ultrawide' prior
            lines(log(BF10w), col = greycol, cex = 0.7, lwd = 1.3, lty = 3)  # 'wide' prior
        }
    }
    
    BFevidence <- BF10e
    
    if (evidenceText) {
        
        if (BF10e < 1) {
            BFevidence <- 1/BF10e
        }
        if (BFevidence >= 1 & BFevidence <= 3) {
            lab <- "Anecdotal"
        }
        if (BFevidence > 3 & BFevidence <= 10) {
            lab <- "Moderate"
        }
        if (BFevidence > 10 & BFevidence <= 30) {
            lab <- "Strong"
        }
        if (BFevidence > 30 & BFevidence <= 100) {
            lab <- "Very strong"
        }
        if (BFevidence > 100) {
            lab <- "Extreme"
        }
        xxT <- max(xLab)
        yyT <- grconvertY(0.775 + offsetTopPart, "ndc", "user")
        
        if (BF10e >= 1) {
            
            if (oneSided == FALSE) {
                text(xxT, yyT, paste("Evidence for H1:\n", lab), cex = 1.4, 
                  pos = 2, offset = -0.2)
            }
            if (oneSided == "right") {
                text(xxT, yyT, paste("Evidence for H+:\n", lab), cex = 1.4, 
                  pos = 2, offset = -0.2)
            }
            if (oneSided == "left") {
                text(xxT, yyT, paste("Evidence for H-:\n", lab), cex = 1.4, 
                  pos = 2, offset = -0.2)
            }
        }
        
        if (BF10e < 1) {
            text(xxT, yyT, paste("Evidence for H0:\n", lab), cex = 1.4, 
                pos = 2, offset = -0.2)
        }
        
    } else {
        
        # add legend
        xx <- grconvertX(0.56, "ndc", "user")
        yy <- grconvertY(0.872 + offsetTopPart, "ndc", "user")
        
        BFind <- sort(c(BF10[length(x)], BF10u[length(x)], BF10w[length(x)]), 
            decreasing = TRUE, index.return = TRUE)$ix
        legend <- c("user prior", "ultrawide prior", "wide prior")
        
        if (length(BF10) <= 60) {
            
            pt.bg <- c("grey", "white", "black")
            pt.cex <- c(cexPoints, 0.7, 0.7)
            legend(xx, yy, legend = legend[BFind], pch = rep(21, 3), pt.bg = pt.bg[BFind], 
                bty = "n", cex = cexLegend, lty = rep(NULL, 3), pt.lwd = rep(1.3, 
                  3), pt.cex = pt.cex[BFind])
        } else {
            
            xx <- grconvertX(0.55, "ndc", "user")
            lty <- c(1, 1, 3)
            lwd <- c(2.7, 1.3, 1.3)
            col <- c("black", greycol2, greycol)
            legend(xx, yy, legend = legend[BFind], lty = lty[BFind], bty = "n", 
                cex = cexLegend, lwd = lwd[BFind], col = col[BFind], seg.len = 0.7)
        }
    }
}

### generate data ###

set.seed(1)
x <- rnorm(30, 0.15)

### calculate Bayes factor ###

library(BayesFactor)
BF <- extractBF(ttestBF(x, rscale = "medium"), onlybf = TRUE)

### plot ###
.plotSequentialBF.ttest(x = x, BF10post = BF, rscale = "medium", plotDifferentPriors = TRUE)