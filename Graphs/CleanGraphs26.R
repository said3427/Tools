library(plotrix)
source("HDIofMCMC.R")

load("mcmcChain1.rda")
load("mcmcChain2.rda")

op <- par(mfrow = c(1, 2))

par(cex.main = 1.5, mar = c(5.5, 5.5, 5.9, 3) + 0.1, mgp = c(3.5, 1, 0), 
    cex.lab = 1.5, font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
y <- hist(mcmcChain1[, "pihat"], freq = F, main = "", xlab = "", ylab = " ", 
    xlim = c(2.1, 5.1), axes = FALSE, breaks = 24, ylim = c(0, 2), yaxt = "n", 
    col = "grey")
axis(1, at = c(2.1, 2.6, pi, 3.6, 4.1, 4.6, 5.1), labels = c(2.1, 2.6, 
    expression(pi), 3.6, 4.1, 4.6, 5.1), lwd = 2, lwd.ticks = 2, line = -0.1)
ablineclip(h = 0, x1 = 5.1, col = "white")
axis(2, at = seq(0, 2, 0.5), line = -0.2, lwd = 2, lwd.ticks = 2)
mtext(expression(hat(pi)), side = 1, line = 4, cex = 2.4, font = 2, adj = 0.5)
mtext("Density", side = 2, line = 3.7, cex = 2.4, font = 2, las = 0)
lines(density(mcmcChain1[, "pihat"], from = 2.1, to = 5.1), lwd = 4)
HDI <- HDIofMCMC(mcmcChain1[, "pihat"])
arrows(x0 = HDI[1], y0 = 1.4, x1 = HDI[2], y1 = 1.4, angle = 90, length = 0.1, 
    code = 3, lwd = 2.2)
text("95% HDI", x = mean(HDI), y = 1.48, cex = 1.8)
text(expression(P(cross) ~ "= .5"), x = 3.99, y = 1, cex = 1.5)


par(cex.main = 1.5, mar = c(5.5, 5.5, 5.9, 3) + 0.1, mgp = c(3.5, 1, 0), 
    cex.lab = 1.5, font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
h <- hist(mcmcChain2[, "pihat"], freq = F, main = "", xlab = "", ylab = " ", 
    xlim = c(2.1, 5.1), axes = FALSE, col = "grey", breaks = 17, ylim = c(0, 
        2), , xaxt = "n")
axis(1, at = c(2.1, 2.6, pi, 3.6, 4.1, 4.6, 5.1), labels = c(2.1, 2.6, 
    expression(pi), 3.6, 4.1, 4.6, 5.1), lwd = 2, lwd.ticks = 2, line = -0.1)
axis(2, at = seq(0, 2, 0.5), lwd = 2, lwd.ticks = 2, line = -0.2)
mtext(expression(hat(pi)), side = 1, line = 4, cex = 2.4, font = 2, adj = 0.5)
mtext("Density", side = 2, line = 3.7, cex = 2.4, font = 2, las = 0)
lines(density(mcmcChain2[, "pihat"], from = 2.1, to = 5.1), lwd = 4)
HDI <- HDIofMCMC(mcmcChain2[, "pihat"])
arrows(x0 = HDI[1], y0 = 1.73, x1 = HDI[2], y1 = 1.73, angle = 90, length = 0.1, 
    code = 3, lwd = 2.2)
text("95% HDI", x = mean(HDI), y = 1.81, cex = 1.8)
text(expression(P(cross) ~ "= .63"), x = 4.12, y = 1, cex = 1.5)
mtext(expression("Posterior of" ~ hat(pi)), side = 3, line = -4.6, outer = TRUE, 
    cex = 3.3)

par(op)