boxplot.ej <- function(y, xloc = 1, width.box = 0.25, lwd.box = 2, width.hor = 0.25, 
    lwd.hor = 2, range.wisk = 1.5, lwd.wisk = 2, pch.box = 16, cex.boxpoint = 2, 
    plot.outliers = FALSE, pch.out = 1, cex.out = 1, color = "black") {
	
    # makes boxplot with dot as median and solid whisker Interquartile range =
    # (.75 quantile) - (.25 quantile).  Note: Wiskers are not always symmetrical;
    # top wisker extends up to max(y) constrained by y <= (.75 quantile) +
    # range.wisk*Interquartile range bottom whisker is determined by min(y)
    # constrained by y >= (.25 quantile) - range.wisk*Interquartile range
	
    Q <- quantile(y, c(0.25, 0.5, 0.75))
    names(Q) <- NULL  # gets rid of percentages
    IQ.range <- Q[3] - Q[1]
    low <- Q[1] - range.wisk * IQ.range
    high <- Q[3] + range.wisk * IQ.range
    index <- which((y >= low) & (y <= high))
    wisk.low <- min(y[index])
    wisk.high <- max(y[index])
    outliers <- y[which((y < low) | (y > high))]
    
    # plot median:
    points(xloc, Q[2], pch = pch.box, cex = cex.boxpoint, col = color)
    
    # plot box:
    xleft <- xloc - width.box/2
    xright <- xloc + width.box/2
    ybottom <- Q[1]
    ytop <- Q[3]
    rect(xleft, ybottom, xright, ytop, lwd = lwd.box, border = color)
    
    # plot whiskers:
    segments(xloc, wisk.low, xloc, Q[1], lwd = lwd.wisk, col = color)
    segments(xloc, Q[3], xloc, wisk.high, lwd = lwd.wisk, col = color)
    
    # plot horizontal segments:
    x0 <- xloc - width.hor/2
    x1 <- xloc + width.hor/2
    segments(x0, wisk.low, x1, wisk.low, lwd = lwd.hor, col = color)
    segments(x0, wisk.high, x1, wisk.high, lwd = lwd.hor, col = color)
    
    # plot outliers:
    if (plot.outliers == TRUE) {
        xloc.p <- rep(xloc, length(outliers))
        points(xloc.p, outliers, pch = pch.out, cex = cex.out, col = color)
    }
}

RT.hf.sp <- rnorm(1000, mean = 0.41, sd = 0.008)
RT.lf.sp <- rnorm(1000, mean = 0.43, sd = 0.01)
RT.vlf.sp <- rnorm(1000, mean = 0.425, sd = 0.012)
RT.hf.ac <- rnorm(1000, mean = 0.46, sd = 0.008)
RT.lf.ac <- rnorm(1000, mean = 0.51, sd = 0.01)
RT.vlf.ac <- rnorm(1000, mean = 0.52, sd = 0.012)

ps <- 1  # size of boxpoint
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = " ", xlab = " ", cex = 1.5, 
    ylim = c(0.3, 0.6), xlim = c(1, 4), lwd = 2, pch = 5, axes = FALSE, main = " ")
axis(1, at = c(1.5, 2.5, 3.5), labels = c("HF", "LF", "VLF"))
mtext("Word Frequency", side = 1, line = 3, cex = 1.5, font = 2)
axis(2, pos = 1.1)
par(las = 0)
mtext("Group Mean M", side = 2, line = 2.9, cex = 1.5, font = 2)

x <- c(1.5, 2.5, 3.5)
boxplot.ej(RT.hf.sp, xloc = 1.5, cex.boxpoint = ps)
boxplot.ej(RT.hf.ac, xloc = 1.5, cex.boxpoint = ps, color = "grey")
boxplot.ej(RT.lf.sp, xloc = 2.5, cex.boxpoint = ps)
boxplot.ej(RT.lf.ac, xloc = 2.5, cex.boxpoint = ps, color = "grey")
boxplot.ej(RT.vlf.sp, xloc = 3.5, cex.boxpoint = ps)
boxplot.ej(RT.vlf.ac, xloc = 3.5, cex.boxpoint = ps, color = "grey")

text(2.5, 0.35, "Speed", cex = 1.4, font = 1, adj = 0.5)
text(2.5, 0.57, "Accuracy", cex = 1.4, font = 1, col = "grey", adj = 0.5)