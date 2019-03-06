op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

yhigh <- 1
xlow <- -3
xhigh <- 3
postmean <- 0.5
postsd <- 0.8
priormean <- 0
priorsd <- 1

plot(function(x) dnorm(x, mean = postmean, sd = postsd), xlow, xhigh, ylim = c(0, 
    yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 1, ylab = "", xlab = "", main = "Inference for Mu", 
    axes = FALSE)
lines(c(0, 0), c(0, 1.25), lwd = 2, col = "grey")

par(new = TRUE)

plot(function(x) dnorm(x, mean = priormean, sd = priorsd), xlow, xhigh, ylim = c(0, 
    yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 2, ylab = "", xlab = "", axes = FALSE)
axis(1)
axis(2)
par(las = 0)
mtext("Mu", side = 1, line = 2.5, cex = 1.5)
mtext("Density", side = 2, line = 3, cex = 1.8)

par(op)