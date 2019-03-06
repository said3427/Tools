### Plot 1: RTs on first y-axis, errors on second y-axis

plotsebargraph = function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
    w = wiskwidth/2
    segments(x0 = loc, x1 = loc, y0 = value, y1 = value + sterr, col = color, 
        lwd = linewidth)
    segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
        col = color, lwd = linewidth)  # upper whiskers
}
plotsegraph = function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
    w = wiskwidth/2
    segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
        lwd = linewidth)
    segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
        col = color, lwd = linewidth)  # upper whiskers
    segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
        col = color, lwd = linewidth)  # lower whiskers
}

# =======================================================

# Data; order = Speed, neutral, accuracy
MRT <- c(429, 515, 555)
MRT.se <- c(25, 25, 30)
Er <- c(0.23, 0.14, 0.13)
Er.se <- c(0.022, 0.021, 0.021)

# ======================================================

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
# mpg = c(3, 1, 0) is default. first = axis labels!; middle = tick labels mar
# = c(5, 4, 4, 2) + 0.1 is default

digitsize <- 1.2
x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = " Mean Response Time (ms.)", 
    xlab = " ", cex = 1.5, ylim = c(200, 800), xlim = c(1, 4), lwd = 2, pch = 5, 
    axes = F, main = " ")

axis(1, at = c(1.5, 2.5, 3.5), labels = c("Speed", "Neutral", "Accuracy"))
mtext("Cue", side = 1, line = 3, cex = 1.5, font = 2)
axis(2, at = c(300, 400, 500, 600, 700))

x = c(1.5, 2.5, 3.5)
points(x, MRT, cex = 1.5, lwd = 2, pch = 19)
plot.errbars = plotsegraph(x, MRT, MRT.se, 0.1, color = "black")  #0.1 = wiskwidth

lines(c(1.5, 2.5, 3.5), MRT, lwd = 2, type = "c")
text(1.5, MRT[1] + 60, "429", adj = 0.5, cex = digitsize)
text(2.5, MRT[2] + 60, "515", adj = 0.5, cex = digitsize)
text(3.5, MRT[3] + 60, "555", adj = 0.5, cex = digitsize)

par(new = TRUE)

x <- c(1, 2, 3, 4)
plot(x, c(-10, -10, -10, -10), type = "p", ylab = " ", xlab = " ", cex = 1.5, 
    ylim = c(0, 1), xlim = c(1, 4), lwd = 2, axes = FALSE, main = " ")
axis(4, at = c(0, 0.1, 0.2, 0.3, 0.4), las = 1)
grid::grid.text("Mean Proportion of Errors", 0.97, 0.5, rot = 270, gp = grid::gpar(cex = 1.5, 
    font = 2))

width <- 0.25
linewidth <- 2
x0 <- 1.5 - width
x1 <- 1.5 + width
y0 <- 0
y1 <- Er[1]
segments(x0, y0, x0, y1, lwd = linewidth)
segments(x0, y1, x1, y1, lwd = linewidth)
segments(x1, y1, x1, y0, lwd = linewidth)
segments(x1, y0, x0, y0, lwd = linewidth)
x0 <- 2.5 - width
x1 <- 2.5 + width
y0 <- 0
y1 <- Er[2]
segments(x0, y0, x0, y1, lwd = linewidth)
segments(x0, y1, x1, y1, lwd = linewidth)
segments(x1, y1, x1, y0, lwd = linewidth)
segments(x1, y0, x0, y0, lwd = linewidth)
x0 <- 3.5 - width
x1 <- 3.5 + width
y0 <- 0
y1 <- Er[3]
segments(x0, y0, x0, y1, lwd = linewidth)
segments(x0, y1, x1, y1, lwd = linewidth)
segments(x1, y1, x1, y0, lwd = linewidth)
segments(x1, y0, x0, y0, lwd = linewidth)

loc.errbars <- c(1.5, 2.5, 3.5)
plot.errbars <- plotsebargraph(loc.errbars, Er, Er.se, 0.2, color = "black")  # 0.2 = wiskwidth

text(1.5, 0.9, "Behavioral Data", font = 2, cex = 2, pos = 4)

text(1.5, 0.05, "0.23", adj = 0.5, cex = digitsize)
text(2.5, 0.05, "0.14", adj = 0.5, cex = digitsize)
text(3.5, 0.05, "0.13", adj = 0.5, cex = digitsize)