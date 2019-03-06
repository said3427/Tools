xbar.therapy <- 92
s.therapy <- 8.5
xbar.placebo <- 85
s.placebo <- 9.1
n <- 15
xdiff <- xbar.therapy - xbar.placebo
sdiff <- sqrt((s.therapy^2 + s.placebo^2)/2) * sqrt(2/n)
sdiff <- sqrt(s.therapy^2 + s.placebo^2)/sqrt(n)

muH0 <- 0
muH1 <- 8

t0 <- (xdiff - muH0)/sdiff
par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
par(mar = c(4, 4.5, 4.5, 1))
x <- seq(-15, 30, by = 0.001)
y <- dt(x/sdiff, df = 28)
y3 <- dt((x - 9)/sdiff, df = 28)

plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 25), 
    lwd = 2)
lines(x, y3, lwd = 2)
axis(side = 1, at = seq(-15, 30, by = 5), pos = 0, lwd = 2, cex.axis = 1.7)
axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2, padj = 0.1)
abline(v = xdiff, col = "red4", lwd = 2)
L0 <- dt((xdiff/sdiff), df = 28)
L2 <- dt(((xdiff - 9)/sdiff), df = 28)
lines(c(6.7, 7.3), y = rep(L0, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L2, 2), col = "red4", lwd = 2)
text(8, L0, expression(paste(italic("L"), " = .04")), adj = 0, col = "red4", 
    cex = 1.8)
text(7.5, L2, expression(paste(italic("L"), " = .32")), adj = 0, col = "red4", 
    cex = 1.8)
text(-16, 0.35, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), adj = 0, 
    cex = 1.8)
text(-16, 0.3, expression(paste(H[1], " : ", mu[diff], " = 9", sep = "")), adj = 0, 
    cex = 1.8)
mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
    cex = 1.8, padj = 0.1)
text(14, 0.2, expression(paste("LR = ", frac(".32", ".04") %~~% 8, sep = "")), 
    adj = 0, col = "red4", cex = 1.8)