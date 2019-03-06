library(BayesFactor)

.plotBF.robustnessCheck.ttest <- function(x = NULL, y = NULL, paired = FALSE, 
    BF10post, formula = NULL, data = NULL, rscale = 1, oneSided = FALSE, 
    lwd = 2, cexPoints = 1.4, cexAxis = 1.2, cexYXlab = 1.5, cexText = 1.2, 
    cexLegend = 1.4, lwdAxis = 1.2, cexEvidence = 1.6, BFH1H0 = TRUE, 
    dontPlotData = FALSE) {
    
    
    #### settings ####
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
    
    
    par(mar = c(5, 6, 6, 7) + 0.1, las = 1)
    
    if (dontPlotData) {
        
        plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, 
            xlab = "", ylab = "")
        
        axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, 
            xlab = "")
        axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, 
            ylab = "")
        
        
        if (oneSided == FALSE) {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF[1][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0][1]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            }
        }
        
        if (oneSided == "right") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["+"][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["+"]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            }
        }
        
        if (oneSided == "left") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["-"][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["-"]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            }
        }
        
        mtext("Cauchy prior width", side = 1, cex = cexYXlab, line = 2.5)
        
        return()
    }
    
    #### get BFs ###
    rValues <- seq(0.0005, 1.5, length.out = 400)
    
    # BF10
    BF10 <- vector("numeric", length(rValues))
    
    for (i in seq_along(rValues)) {
        
        if (oneSided == FALSE) {
            
            BF <- BayesFactor::ttestBF(x = x, y = y, paired = paired, 
                nullInterval = nullInterval, rscale = rValues[i])
            BF10[i] <- BayesFactor::extractBF(BF, logbf = FALSE, onlybf = F)[1, 
                "bf"]
            
        } else {
            
            BF10[i] <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, 
                oneSided = oneSided, r = rValues[i])
        }
        
    }
    
    # BF10 'medium' prior
    if (oneSided == FALSE) {
        
        BF10m <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, 
            rscale = "medium")
        BF10m <- BayesFactor::extractBF(BF10m, logbf = FALSE, onlybf = F)[1, 
            "bf"]
        
    } else {
        
        BF10m <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, 
            oneSided = oneSided, r = "medium")
    }
    
    BF10mText <- BF10m
    
    # BF10 'wide' prior
    if (oneSided == FALSE) {
        
        BF10w <- BayesFactor::ttestBF(x = x, y = y, paired = paired, nullInterval = nullInterval, 
            rscale = "wide")
        BF10w <- BayesFactor::extractBF(BF10w, logbf = FALSE, onlybf = F)[1, 
            "bf"]
        
    } else {
        
        BF10w <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, 
            oneSided = oneSided, r = "wide")
    }
    
    BF10wText <- BF10w
    
    # BF10 'ultrawide' prior
    if (oneSided == FALSE) {
        
        BF10ultra <- BayesFactor::ttestBF(x = x, y = y, paired = paired, 
            nullInterval = nullInterval, rscale = "ultrawide")
        BF10ultra <- BayesFactor::extractBF(BF10ultra, logbf = FALSE, 
            onlybf = F)[1, "bf"]
        
    } else {
        
        BF10ultra <- .oneSidedTtestBFRichard(x = x, y = y, paired = paired, 
            oneSided = oneSided, r = "ultrawide")
    }
    
    BF10ultraText <- BF10ultra
    
    # BF10 user prior
    BF10user <- BF10post
    BF10userText <- BF10user
    
    ####################### scale y axis ###########################
    
    BF <- c(BF10, BF10m, BF10w, BF10ultra, BF10user)
    
    if (!BFH1H0) {
        
        BF <- 1/BF
        BF10 <- 1/BF10
        BF10m <- 1/BF10m
        BF10w <- 1/BF10w
        BF10ultra <- 1/BF10ultra
        # BF10user <- 1 / BF10user
    }
    
    # y-axis labels larger than 1
    y1h <- "1"
    i <- 1
    
    while (eval(parse(text = y1h[i])) < max(BF10)) {
        
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
    
    while (eval(parse(text = y3h[i])) < max(BF10)) {
        
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
    
    while (eval(parse(text = y1l[i])) > min(BF10)) {
        
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
    
    while (eval(parse(text = y3l[i])) > min(BF10)) {
        
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
        
        if (max(BF10) > eval(parse(text = yLab1s[length(yLab1s)]))) {
            
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
        
        if (max(BF10) > eval(parse(text = yLab1s[length(yLab1s) - 1]))) {
            
            if (grepl(pattern = "e", yLab1s[length(yLab1s)])) {
                
                newy <- paste(strsplit(yLab1s[length(yLab1s)], split = "+", 
                  fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], 
                  split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
            } else {
                
                newy <- paste(yLab1s[length(yLab1s)], "0", sep = "")
            }
            
            if (eval(parse(text = newy)) >= 10^6) {
                
                newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
            }
            
            yLab1s <- c(yLab1s, newy)
        }
        
        if (yLab1s[1] == "1") {
            
            yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
        }
        if (yLab1s[length(yLab1s)] == "1") {
            
            yLab1s <- c(yLab1s, "10")
        }
        
        if (min(BF10) < eval(parse(text = yLab1s[1]))) {
            
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
        
        if (min(BF10) < eval(parse(text = yLab1s[2]))) {
            
            if (grepl(pattern = "e", yLab1s[1])) {
                
                newy <- paste(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][1], 
                  "+", as.numeric(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][2]) + 
                    1, sep = "")
            } else {
                
                newy <- paste(yLab1s[1], "0", sep = "")
            }
            
            if (eval(parse(text = newy)) <= 10^(-6)) {
                
                newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
                newy <- sub("-", "+", x = newy)
                newy <- substring(newy, nchar(newy) - 4, nchar(newy))
                newy <- paste0("1/", newy)
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
    
    while (eval(parse(text = yLab[2])) > min(BF10)) {
        
        interval <- as.numeric(strsplit(format(eval(parse(text = yLab[1])), 
            digits = 3, scientific = TRUE), "-", fixed = TRUE)[[1]][2]) - 
            as.numeric(strsplit(format(eval(parse(text = yLab[2])), digits = 3, 
                scientific = TRUE), "-", fixed = TRUE)[[1]][2])
        pot <- as.numeric(strsplit(format(eval(parse(text = yLab[1])), 
            digits = 3, scientific = TRUE), "-", fixed = TRUE)[[1]][2]) + 
            interval
        
        if (nchar(pot) == 1) 
            pot <- paste("0", pot, sep = "")
        
        newy <- paste("1/1e", "+", pot, sep = "")
        yLab <- c(newy, yLab)

    }
    
    
    while (eval(parse(text = yLab[length(yLab) - 1])) < max(BF10)) {
        
        interval <- as.numeric(strsplit(format(eval(parse(text = yLab[length(yLab)])), 
            digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][2]) - 
            as.numeric(strsplit(format(eval(parse(text = yLab[length(yLab) - 
                1])), digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][2])
        pot <- as.numeric(strsplit(format(eval(parse(text = yLab[length(yLab)])), 
            digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][2]) + 
            interval
        
        if (nchar(pot) == 1) 
            pot <- paste("0", pot, sep = "")
        
        newy <- paste(strsplit(format(eval(parse(text = yLab[length(yLab)])), 
            digits = 3, scientific = TRUE), "+", fixed = TRUE)[[1]][1], 
            "+", pot, sep = "")
        yLab <- c(yLab, newy)
    }
    
    
    yAt <- vector("numeric", length(yLab))
    
    for (i in seq_along(yLab)) {
        
        yAt[i] <- log(eval(parse(text = yLab[i])))
    }
    
    
    ####################### plot ###########################
    
    xLab <- c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
    xlim <- range(xLab)
    ylow <- log(eval(parse(text = yLab[1])))
    yhigh <- log(eval(parse(text = yLab[length(yLab)])))
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
        
        if (eval(parse(text = yLab[1])) <= 1/10^6) {
            
            line <- 4.75
            
        } else {
            
            line <- 4.3
        }
        
        if (oneSided == FALSE) {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF[1][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = line)
            } else {
                
                mtext(text = expression(BF[0][1]), side = 2, las = 0, 
                  cex = cexYXlab, line = line)
            }
        }
        
        if (oneSided == "right") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["+"][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = line)
            } else {
                
                mtext(text = expression(BF[0]["+"]), side = 2, las = 0, 
                  cex = cexYXlab, line = line)
            }
        }
        
        if (oneSided == "left") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["-"][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = line)
            } else {
                
                mtext(text = expression(BF[0]["-"]), side = 2, las = 0, 
                  cex = cexYXlab, line = line)
            }
        }
    }
    
    if (omit3s == FALSE) {
        
        if (oneSided == FALSE) {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF[1][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0][1]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            }
        }
        
        if (oneSided == "right") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["+"][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["+"]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            }
        }
        
        if (oneSided == "left") {
            
            if (BFH1H0) {
                
                mtext(text = expression(BF["-"][0]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            } else {
                
                mtext(text = expression(BF[0]["-"]), side = 2, las = 0, 
                  cex = cexYXlab, line = 3.1)
            }
        }
    }
    
    mtext("Cauchy prior width", side = 1, cex = cexYXlab, line = 2.5)
    
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
    
    # display BF10
    lines(rValues, log(BF10), col = "black", lwd = 2.7)
    
    # display 'wide', user, and 'ultrawide' prior BFs
    points(r, log(BF10user), pch = 21, bg = "grey", cex = cexPoints, lwd = 1.3)  # user prior
    points(1, log(BF10w), pch = 21, bg = "black", cex = 1.1, lwd = 1.3)  # 'wide' prior
    points(sqrt(2), log(BF10ultra), pch = 21, bg = "white", cex = 1.1, 
        lwd = 1.3)  # 'ultrawide' prior
    
    #### add legend BF values
    
    # BFuser
    
    if (BFH1H0) {
        
        BF01userText <- 1/BF10userText
        
    } else {
        
        BF10userText <- 1/BF10userText
        BF01userText <- 1/BF10userText
    }
    
    if (BF10userText >= 1000000 | BF01userText >= 1000000) {
        
        BF10usert <- format(BF10userText, digits = 4, scientific = TRUE)
        BF01usert <- format(BF01userText, digits = 4, scientific = TRUE)
    }
    if (BF10userText < 1000000 & BF01userText < 1000000) {
        
        BF10usert <- formatC(BF10userText, 3, format = "f")
        BF01usert <- formatC(BF01userText, 3, format = "f")
    }
    
    if (oneSided == FALSE) {
        
        if (BF10userText >= BF01userText) {
            userBF <- bquote(BF[10] == .(BF10usert))
        } else {
            userBF <- bquote(BF[0][1] == .(BF01usert))
        }
    }
    if (oneSided == "right") {
        
        if (BF10userText >= BF01userText) {
            userBF <- bquote(BF["+"][0] == .(BF10usert))
        } else {
            userBF <- bquote(BF[0]["+"] == .(BF01usert))
        }
    }
    if (oneSided == "left") {
        
        if (BF10userText >= BF01userText) {
            userBF <- bquote(BF["-"][0] == .(BF10usert))
        } else {
            userBF <- bquote(BF[0]["-"] == .(BF01usert))
        }
    }
    
    # BFwide
    BF01wText <- 1/BF10wText
    
    if (BF10wText >= 1000000 | BF01wText >= 1000000) {
        BF10wt <- format(BF10wText, digits = 4, scientific = TRUE)
        BF01wt <- format(BF01wText, digits = 4, scientific = TRUE)
    }
    if (BF10wText < 1000000 & BF01wText < 1000000) {
        BF10wt <- formatC(BF10wText, 3, format = "f")
        BF01wt <- formatC(BF01wText, 3, format = "f")
    }
    
    if (oneSided == FALSE) {
        
        if (BF10wText >= BF01wText) {
            wBF <- bquote(BF[10] == .(BF10wt))
        } else {
            wBF <- bquote(BF[0][1] == .(BF01wt))
        }
    }
    if (oneSided == "right") {
        
        if (BF10wText >= BF01wText) {
            wBF <- bquote(BF["+"][0] == .(BF10wt))
        } else {
            wBF <- bquote(BF[0]["+"] == .(BF01wt))
        }
    }
    if (oneSided == "left") {
        
        if (BF10wText >= BF01wText) {
            wBF <- bquote(BF["-"][0] == .(BF10wt))
        } else {
            wBF <- bquote(BF[0]["-"] == .(BF01wt))
        }
    }
    
    # BFultrawide
    BF01ultraText <- 1/BF10ultraText
    
    if (BF10ultraText >= 1000000 | BF01ultraText >= 1000000) {
        
        BF10ultrat <- format(BF10ultraText, digits = 4, scientific = TRUE)
        BF01ultrat <- format(BF01ultraText, digits = 4, scientific = TRUE)
    }
    if (BF10ultraText < 1000000 & BF01ultraText < 1000000) {
        
        BF10ultrat <- formatC(BF10ultraText, 3, format = "f")
        BF01ultrat <- formatC(BF01ultraText, 3, format = "f")
    }
    
    if (oneSided == FALSE) {
        
        if (BF10ultraText >= BF01ultraText) {
            ultraBF <- bquote(BF[10] == .(BF10ultrat))
        } else {
            ultraBF <- bquote(BF[0][1] == .(BF01ultrat))
        }
    }
    
    if (oneSided == "right") {
        
        if (BF10ultraText >= BF01ultraText) {
            ultraBF <- bquote(BF["+"][0] == .(BF10ultrat))
        } else {
            ultraBF <- bquote(BF[0]["+"] == .(BF01ultrat))
        }
    }
    
    if (oneSided == "left") {
        
        if (BF10ultraText >= BF01ultraText) {
            ultraBF <- bquote(BF["-"][0] == .(BF10ultrat))
        } else {
            ultraBF <- bquote(BF[0]["-"] == .(BF01ultrat))
        }
    }
    
    xx <- grconvertX(0.2, "ndc", "user")
    yy <- grconvertY(0.965, "ndc", "user")
    
    BFind <- sort(c(BF10userText, BF10ultraText, BF10wText), decreasing = TRUE, 
        index.return = TRUE)$ix
    BFsort <- sort(c(BF10userText, BF10ultraText, BF10wText), decreasing = TRUE, 
        index.return = TRUE)$x
    
    legend <- c("user prior:", "ultrawide prior:", "wide prior:")
    pt.bg <- c("grey", "white", "black")
    pt.cex <- c(cexPoints, 1.1, 1.1)
    
    legend(xx, yy, legend = legend[BFind], pch = rep(21, 3), pt.bg = pt.bg[BFind], 
        bty = "n", cex = cexLegend, lty = rep(NULL, 3), pt.lwd = rep(1.3, 
            3), pt.cex = pt.cex[BFind])
    
    xx <- grconvertX(0.5, "ndc", "user")
    y1 <- grconvertY(0.902, "ndc", "user")
    y2 <- grconvertY(0.852, "ndc", "user")
    y3 <- grconvertY(0.802, "ndc", "user")
    yy <- c(y1, y2, y3)
    
    text(xx, yy[BFsort == BF10userText], userBF, cex = 1.3, pos = 4)
    text(xx, yy[BFsort == BF10ultraText], ultraBF, cex = 1.3, pos = 4)
    text(xx, yy[BFsort == BF10wText], wBF, cex = 1.3, pos = 4)
}

### generate data ###

set.seed(1)
x <- rnorm(30, 0.15)

### calculate Bayes factor ###

library(BayesFactor)
BF <- extractBF(ttestBF(x, rscale = "medium"), onlybf = TRUE)

### plot ###

.plotBF.robustnessCheck.ttest(x = x, BF10post = BF, rscale = "medium")