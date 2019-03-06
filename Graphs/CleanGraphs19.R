op <- par(mar = c(4, 0, 0, 4))

x <- seq(-12, 12, 0.1)
x.ticks <- seq(-12, 12, 2)
y <- x
z <- matrix(0, ncol = length(x), nrow = length(y))
z[, 1] <- dnorm(x)
zcol <- matrix(0, ncol = length(x), nrow = length(y))
zcol[, 1] <- "black"

res <- persp(x, y, z, theta = 0, phi = 0, expand = 0.4, xlab = "", ylab = "", 
    ticktype = "detailed", cex.lab = 0.8, zlab = "", box = FALSE, border = FALSE, 
    xlim = c(-13, 13))
polygon(trans3d(c(x, rev(x)), y = rep(y[1], 2 * length(x)), z = c(dnorm(y, 3.8, 
    2), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, blue = 190, 
    alpha = 100, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[41], 2 * length(x)), z = c(dnorm(y, 6.8), 
    rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, blue = 190, 
    alpha = 140, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[81], 2 * length(x)), z = c(dnorm(y, -1, 
    2.5), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, 
    blue = 190, alpha = 180, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[121], 2 * length(x)), z = c(dnorm(y, 
    -5), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, blue = 190, 
    alpha = 220, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[161], 2 * length(x)), z = c(dnorm(y, 
    2.5, 1.5), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, 
    blue = 190, alpha = 260, maxColorValue = 300), border = NA)
polygon(trans3d(c(x, rev(x)), y = rep(y[201], 2 * length(x)), z = c(dnorm(y, 
    -9, 0.8), rep(0, length(x))), pmat = res), col = rgb(red = 190, green = 190, 
    blue = 190, alpha = 300, maxColorValue = 300), border = NA)

### draw x-axis
lines(trans3d(x[which(x == -8):which(x == 10)], min(y) - 2, min(z), res), col = "black", 
    lwd = 1.4)

# tick marks
tick.start <- trans3d(seq(-8, 10, 2), min(y) - 2, min(z), res)
tick.end <- trans3d(seq(-8, 10, 2), min(y) - 2, min(z - 0.01), res)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y, lwd = 2.6)

# labels
labels <- seq(-8, -2, 2)
label.pos <- trans3d(seq(-8, -2, 2), min(y) - 2, min(z - 0.035), res)
text(label.pos$x, label.pos$y, labels = labels, cex = 1.6, adj = 0.65)
labels <- seq(0, 10, 2)
label.pos <- trans3d(seq(0, 10, 2), min(y) - 2, min(z - 0.035), res)
text(label.pos$x, label.pos$y, labels = labels, cex = 1.6, adj = 0.5)

### add labels to distributions
text(trans3d(3.8, y[1], dnorm(3.8, 3.8, 2) + 0.02, res), "a", cex = 1.7)
text(trans3d(6.8, y[41], dnorm(6.8, 6.8) + 0.024, res), "b", cex = 1.7)
text(trans3d(-1, y[81], dnorm(-1, -1, 2.5) + 0.027, res), "c", cex = 1.7)
text(trans3d(-5, y[121], dnorm(-5, -5) + 0.029, res), "d", cex = 1.7)
text(trans3d(2.5, y[161], dnorm(2.5, 2.5, 1.5) + 0.033, res), "e", cex = 1.7)
text(trans3d(-9, y[201], dnorm(-9, -9, 0.8) + 0.039, res), "f", cex = 1.7)

par(op)