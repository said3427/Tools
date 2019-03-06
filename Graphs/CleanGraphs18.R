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

# Bayes Factor (or, ugliest code ever and I DO NOT CARE TO FIX IT)
x <- seq(-15, 30, by = 0.001)
y <- dt(x/sdiff, df = 28)

y1 <- dt((x - 1)/sdiff, df = 28)
y2 <- dt((x - 2)/sdiff, df = 28)
y3 <- dt((x - 3)/sdiff, df = 28)
y4 <- dt((x - 4)/sdiff, df = 28)
y5 <- dt((x - 5)/sdiff, df = 28)
y6 <- dt((x - 6)/sdiff, df = 28)
y7 <- dt((x - 7)/sdiff, df = 28)
y8 <- dt((x - 8)/sdiff, df = 28)
y9 <- dt((x - 9)/sdiff, df = 28)
y10 <- dt((x - 10)/sdiff, df = 28)

plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 25), 
    lwd = 2)

lines(x, y1, col = "grey70")
lines(x, y2, col = "grey70")
lines(x, y3, col = "grey70", lwd = 2)
lines(x, y4, col = "grey70")
lines(x, y5, col = "grey70", lwd = 2)
lines(x, y6, col = "grey70")
lines(x, y7, col = "grey70")
lines(x, y8, col = "grey70", lwd = 2)
lines(x, y9, col = "grey70")
lines(x, y10, col = "grey70", lwd = 2)

axis(side = 1, at = seq(-15, 30, by = 5), pos = 0, lwd = 2, cex.axis = 1.7)
axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2, padj = 0.1)
abline(v = xdiff, col = "red4", lwd = 2)

L0 <- dt((xdiff/sdiff), df = 28)
L1 <- dt(((xdiff - 1)/sdiff), df = 28)
L2 <- dt(((xdiff - 2)/sdiff), df = 28)
L3 <- dt(((xdiff - 3)/sdiff), df = 28)
L4 <- dt(((xdiff - 4)/sdiff), df = 28)
L5 <- dt(((xdiff - 5)/sdiff), df = 28)
L6 <- dt(((xdiff - 6)/sdiff), df = 28)
L7 <- dt(((xdiff - 7)/sdiff), df = 28)
L8 <- dt(((xdiff - 8)/sdiff), df = 28)
L9 <- dt(((xdiff - 9)/sdiff), df = 28)
L10 <- dt(((xdiff - 10)/sdiff), df = 28)

lines(c(6.7, 7.3), y = rep(L0, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L1, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L2, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L3, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L4, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L5, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L6, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L7, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L8, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L9, 2), col = "red4", lwd = 2)
lines(c(6.7, 7.3), y = rep(L10, 2), col = "red4", lwd = 2)

text(-16.8, 0.35, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), 
    adj = 0, cex = 1.6)
text(-16.8, 0.3, expression(paste(H[1], " : 0", "" <= mu[diff], "" <= 10), sep = ""), 
    adj = 0, cex = 1.6)
text(15, 0.35, expression(paste(italic("L"), "(", H[0], ") = .04")), adj = 0, 
    col = "red4", cex = 1.6)
text(15, 0.3, expression(paste(italic("L"), "(", H[1], ") = .10")), adj = 0, 
    col = "red4", cex = 1.6)
text(14.2, 0.22, expression(paste("BF = ", frac(".10", ".04"), " = ", 2.5, sep = "")), 
    adj = 0, col = "red4", cex = 1.6)
mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
    cex = 1.8, padj = 0.1)