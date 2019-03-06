.plotMarginalCor <- function(variable, cexYlab = 1.3, lwd = 2, rugs = FALSE) {
    
    # histogram with density estimator
    
    variable <- variable[!is.na(variable)]
    
    density <- density(variable)
    h <- hist(variable, plot = FALSE)
    jitVar <- jitter(variable)
    yhigh <- max(max(h$density), max(density$y))
    ylow <- 0
    xticks <- pretty(c(variable, h$breaks), min.n = 3)
    plot(range(xticks), c(ylow, yhigh), type = "n", axes = FALSE, ylab = "", 
        xlab = "")
    h <- hist(variable, freq = FALSE, main = "", ylim = c(ylow, yhigh), xlab = "", 
        ylab = " ", axes = FALSE, col = "grey", add = TRUE, nbreaks = round(length(variable)/5))
    ax1 <- axis(1, line = 0.3, at = xticks, lab = xticks)
    par(las = 0)
    ax2 <- axis(2, at = c(0, max(max(h$density), max(density$y))/2, max(max(h$density), 
        max(density$y))), labels = c("", "Density", ""), lwd.ticks = 0, 
        pos = range(ax1) - 0.08 * diff(range(ax1)), cex.axis = 1.7, mgp = c(3, 
            0.7, 0))
    
    if (rugs) 
        rug(jitVar)
    
    lines(density$x[density$x >= min(ax1) & density$x <= max(ax1)], density$y[density$x >= 
        min(ax1) & density$x <= max(ax1)], lwd = lwd)
}

.poly.pred <- function(fit, line = FALSE, xMin, xMax, lwd) {
    
    # predictions of fitted model
    
    # create function formula
    f <- vector("character", 0)
    
    for (i in seq_along(coef(fit))) {
        
        if (i == 1) {
            
            temp <- paste(coef(fit)[[i]])
            f <- paste(f, temp, sep = "")
        }
        
        if (i > 1) {
            
            temp <- paste("(", coef(fit)[[i]], ")*", "x^", i - 1, sep = "")
            f <- paste(f, temp, sep = "+")
        }
    }
    
    x <- seq(xMin, xMax, length.out = 100)
    predY <- eval(parse(text = f))
    
    if (line == FALSE) {
        return(predY)
    }
    
    if (line) {
        lines(x, predY, lwd = lwd)
    }
}


.plotScatter <- function(xVar, yVar, cexPoints = 1.3, cexXAxis = 1.3, 
    cexYAxis = 1.3, lwd = 2) {
    
    # displays scatterplot
    
    d <- data.frame(xx = xVar, yy = yVar)
    d <- na.omit(d)
    xVar <- d$xx
    yVar <- d$yy
    
    fit <- lm(yy ~ poly(xx, 1, raw = TRUE), d)
    
    xlow <- min((min(xVar) - 0.1 * min(xVar)), min(pretty(xVar)))
    xhigh <- max((max(xVar) + 0.1 * max(xVar)), max(pretty(xVar)))
    xticks <- pretty(c(xlow, xhigh))
    ylow <- min((min(yVar) - 0.1 * min(yVar)), min(pretty(yVar)), min(.poly.pred(fit, 
        line = FALSE, xMin = xticks[1], xMax = xticks[length(xticks)], 
        lwd = lwd)))
    yhigh <- max((max(yVar) + 0.1 * max(yVar)), max(pretty(yVar)), max(.poly.pred(fit, 
        line = FALSE, xMin = xticks[1], xMax = xticks[length(xticks)], 
        lwd = lwd)))
    
    
    yticks <- pretty(c(ylow, yhigh))
    
    yLabs <- vector("character", length(yticks))
    
    for (i in seq_along(yticks)) {
        
        if (yticks[i] < 10^6) {
            
            yLabs[i] <- format(yticks[i], digits = 3, scientific = FALSE)
            
        } else {
            
            yLabs[i] <- format(yticks[i], digits = 3, scientific = TRUE)
        }
    }
    
    plot(xVar, yVar, col = "black", pch = 21, bg = "grey", ylab = "", 
        xlab = "", axes = FALSE, ylim = range(yticks), xlim = range(xticks), 
        cex = cexPoints)
    .poly.pred(fit, line = TRUE, xMin = xticks[1], xMax = xticks[length(xticks)], 
        lwd = lwd)
    
    par(las = 1)
    
    axis(1, line = 0.4, labels = xticks, at = xticks, cex.axis = cexXAxis)
    axis(2, line = 0.2, labels = yLabs, at = yticks, cex.axis = cexYAxis)
    
    invisible(max(nchar(yLabs)))
}

.plotCorValue <- function(xVar, yVar, cexText = 2.5, cexCI = 1.7, hypothesis = "correlated", 
    pearson = TRUE, kendallsTauB = FALSE, spearman = FALSE, confidenceInterval = 0.95) {
    
    # displays correlation value
    
    CIPossible <- TRUE
    
    tests <- c()
    
    if (pearson) 
        tests <- c(tests, "pearson")
    
    if (spearman) 
        tests <- c(tests, "spearman")
    
    if (kendallsTauB) 
        tests <- c(tests, "kendall")
    
    plot(1, 1, type = "n", axes = FALSE, ylab = "", xlab = "")
    
    lab <- vector("list")
    
    for (i in seq_along(tests)) {
        
        if (round(cor.test(xVar, yVar, method = tests[i])$estimate, 8) == 
            1) {
            
            CIPossible <- FALSE
            
            if (tests[i] == "pearson") {
                lab[[i]] <- bquote(italic(r) == "1.000")
            }
            
            if (tests[i] == "spearman") {
                lab[[i]] <- bquote(italic(rho) == "1.000")
            }
            
            if (tests[i] == "kendall") {
                lab[[i]] <- bquote(italic(tau) == "1.000")
            }
            
        } else if (round(cor.test(xVar, yVar, method = tests[i])$estimate, 
            8) == -1) {
            
            CIPossible <- FALSE
            
            if (tests[i] == "pearson") {
                lab[[i]] <- bquote(italic(r) == "-1.000")
            }
            
            if (tests[i] == "spearman") {
                lab[[i]] <- bquote(italic(rho) == "-1.000")
            }
            
            if (tests[i] == "kendall") {
                lab[[i]] <- bquote(italic(tau) == "-1.000")
            }
            
        } else {
            
            if (tests[i] == "pearson") {
                lab[[i]] <- bquote(italic(r) == .(formatC(round(cor.test(xVar, 
                  yVar, method = tests[i])$estimate, 3), format = "f", 
                  digits = 3)))
            }
            
            if (tests[i] == "spearman") {
                lab[[i]] <- bquote(rho == .(formatC(round(cor.test(xVar, 
                  yVar, method = tests[i])$estimate, 3), format = "f", 
                  digits = 3)))
            }
            
            if (tests[i] == "kendall") {
                lab[[i]] <- bquote(tau == .(formatC(round(cor.test(xVar, 
                  yVar, method = tests[i])$estimate, 3), format = "f", 
                  digits = 3)))
            }
        }
    }
    
    if (length(tests) == 1) {
        ypos <- 1
    }
    
    if (length(tests) == 2) {
        ypos <- c(1.1, 0.9)
    }
    
    if (length(tests) == 3) {
        ypos <- c(1.2, 1, 0.8)
    }
    
    
    for (i in seq_along(tests)) {
        
        text(1, ypos[i], labels = lab[[i]], cex = cexText)
    }
    
    
    if (hypothesis == "correlated" & length(tests) == 1 & any(tests == 
        "pearson")) {
        
        alternative <- "two.sided"
        ctest <- cor.test(xVar, yVar, method = tests, conf.level = confidenceInterval)
    }
    
    if (hypothesis != "correlated" & length(tests) == 1 & any(tests == 
        "pearson")) {
        
        if (hypothesis == "correlatedPositively") {
            
            ctest <- cor.test(xVar, yVar, method = tests, alternative = "greater", 
                conf.level = confidenceInterval)
            
        } else if (hypothesis == "correlatedNegatively") {
            
            ctest <- cor.test(xVar, yVar, method = tests, alternative = "less", 
                conf.level = confidenceInterval)
        }
        
    }
    
    if (any(tests == "pearson") & length(tests) == 1 && CIPossible) {
        
        CIlow <- formatC(round(ctest$conf.int[1], 3), format = "f", digits = 3)
        CIhigh <- formatC(round(ctest$conf.int[2], 3), format = "f", digits = 3)
        
        text(1, 0.8, labels = paste(100 * confidenceInterval, "% CI: [", 
            CIlow, ", ", CIhigh, "]", sep = ""), cex = cexCI)
    }
    
}

### matrix plot ###

dataset <- read.csv("obk.long_correct.csv")
variables <- c("pre.2", "pre.3", "pre.5", "post.3", "post.5")

l <- length(variables)

par(mfrow = c(l, l), cex.axis = 1.3, mar = c(3, 4, 2, 1.5) + 0.1, oma = c(0, 
    2.2, 2, 0))

for (row in seq_len(l)) {
    
    for (col in seq_len(l)) {
        
        if (row == col) {
            .plotMarginalCor(dataset[[variables[row]]])  # plot marginal (histogram with density estimator)
        }
        if (col > row) {
            .plotScatter(dataset[[variables[col]]], dataset[[variables[row]]])  # plot scatterplot
        }
        if (col < row) {
            if (l < 7) {
                .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], 
                  cexCI = 1.2)  # plot r= ...
            }
            if (l >= 7) {
                .plotCorValue(dataset[[variables[col]]], dataset[[variables[row]]], 
                  cexCI = 1.2)
            }
        }
    }
}

textpos <- seq(1/(l * 2), (l * 2 - 1)/(l * 2), 2/(l * 2))
for (t in seq_along(textpos)) {
    mtext(text = variables[t], side = 3, outer = TRUE, at = textpos[t], 
        cex = 1.9, line = -0.8)
    mtext(text = variables[t], side = 2, outer = TRUE, at = rev(textpos)[t], 
        cex = 1.9, line = -0.1)
}