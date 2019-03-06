plotsegraph <- function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
	
    w <- wiskwidth/2
    segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
        lwd = linewidth)
    segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
        col = color, lwd = linewidth)  # upper whiskers
    segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
        col = color, lwd = linewidth)  # lower whiskers
}

RT.hf.sp <- 0.41
RT.lf.sp <- 0.43
RT.vlf.sp <- 0.425
se.RT.hf.sp <- 0.01
se.RT.lf.sp <- 0.015
se.RT.vlf.sp <- 0.02
RT.hf.ac <- 0.46
RT.lf.ac <- 0.51
RT.vlf.ac <- 0.52
se.RT.hf.ac <- 0.01
se.RT.lf.ac <- 0.015
se.RT.vlf.ac <- 0.02

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = "", xlab = " ", cex = 1.5, 
    ylim = c(0.3, 0.6), xlim = c(1, 4), lwd = 2, pch = 5, axes = F, main = " ")
axis(1, at = c(1.5, 2.5, 3.5), labels = c("HF", "LF", "VLF"))
mtext("Word Frequency", side = 1, line = 3, cex = 1.5, font = 2)
axis(2, pos = 1.2, )
par(las = 0)
mtext(expression(paste("Mean ", mu)), side = 2, line = 2, cex = 1.5, font = 2)
x <- c(1.5, 2.5, 3.5)
points(x, c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), cex = 1.5, lwd = 2, pch = 19)
plot.errbars <- plotsegraph(x, c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), c(se.RT.hf.sp, 
    se.RT.lf.sp, se.RT.vlf.sp), 0.1, color = "black")  #0.1 = wiskwidth
lines(c(1.5, 2.5, 3.5), c(RT.hf.sp, RT.lf.sp, RT.vlf.sp), lwd = 2, type = "c")
points(x, c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), cex = 1.5, lwd = 2, pch = 21)
plot.errbars <- plotsegraph(x, c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), c(se.RT.hf.ac, 
    se.RT.lf.ac, se.RT.vlf.ac), 0.1, color = "black")  #0.1 = wiskwidth
lines(c(1.5, 2.5, 3.5), c(RT.hf.ac, RT.lf.ac, RT.vlf.ac), lwd = 2, type = "c")
points(1.5, 0.6, pch = 21, lwd = 2, cex = 1.5)
text(1.7, 0.6, "Accuracy", cex = 1.2, font = 1, adj = 0)
points(1.5, 0.57, pch = 19, lwd = 2, cex = 1.5)
text(1.7, 0.57, "Speed", cex = 1.2, font = 1, adj = 0)