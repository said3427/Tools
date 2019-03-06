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

# H0 distribution with p-value shaded:
par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
par(mar = c(4, 4.5, 4.5, 1))

x <- seq(-15, 15, by = 0.001)
y <- dt(x/sdiff, df = 28)
plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 20), 
    lwd = 2)
axis(side = 1, at = seq(-15, 15, by = 5), pos = 0, lwd = 2)
axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2)

# shade area to right of obtained test statistic:
t0 <- xdiff/sdiff
cord.x <- c(t0, seq(t0, 4, 0.001), 4) * sdiff
cord.y <- c(0, dt(seq(t0, 4, 0.001), df = 28), 0)
polygon(cord.x, cord.y, col = "grey")
cord.x <- c(-4, seq(-4, -t0, 0.001), -t0) * sdiff
cord.y <- c(0, dt(seq(-4, -t0, 0.001), df = 24), 0)
polygon(cord.x, cord.y, col = "grey")

# add lines and text:
abline(v = xdiff, col = "red4", lwd = 2)
text(-15, 0.25, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), adj = 0, 
    cex = 1.8)
text(10, 0.08, paste("p = .04"), adj = 0, col = "red4", cex = 1.8)
lines(c(10, 8), c(0.05, 0.01), col = "red4", lwd = 2)
lines(c(10, -8), c(0.05, 0.01), col = "red4", lwd = 2)
mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
    cex = 1.8)