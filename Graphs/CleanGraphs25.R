library(plotrix)

# mix of 2 normal distributions
mixedNorm <- function(x) {
    return(0.5 * dnorm(x, 0.25, 0.13) + 0.5 * dnorm(x, 0.7, 0.082))
}
### normalize so that area [0,1] integrates to 1; k = normalizing constant
k <- 1/integrate(mixedNorm, 0, 1)$value

# normalized
pdfmix <- function(x, k) {
    return(k * (0.5 * dnorm(x, 0.25, 0.13) + 0.5 * dnorm(x, 0.7, 0.082)))
}

# integrate(pdfmix, 0.0790321,0.4048)$value # 0.4

op <- par(mfrow = c(1, 2), mar = c(5.9, 6, 4, 2) + 0.1)

barplot(height = c(0.2, 0.25, 0.1, 0.05, 0.35, 0.05), names.arg = c(1, 
    2, 3, 4, 5, 6), axes = FALSE, ylim = c(0, 1), width = 1, cex.names = 1.5)
arrows(x0 = 0.6, x1 = 0.6, y0 = 0.38, y1 = 0.23, length = c(0.2, 0.2), 
    lwd = 2)
text(0.6, 0.41, "0.2", cex = 1.3)
ablineclip(v = 1.9, y1 = 0.28, y2 = 0.375, lwd = 2)
ablineclip(v = 4.2, y1 = 0.28, y2 = 0.375, lwd = 2)
ablineclip(h = 0.375, x1 = 1.9, x2 = 4.2, lwd = 2)
arrows(x0 = 3.05, x1 = 3.05, y0 = 0.525, y1 = 0.375, length = c(0.2, 0.2), 
    lwd = 2)
text(3.05, 0.555, "0.4", cex = 1.3)
ablineclip(v = 5.5, y1 = 0.38, y2 = 0.43, lwd = 2)
arrows(x0 = 6.7, x1 = 6.7, y0 = 0.43, y1 = 0.09, length = c(0.2, 0.2), 
    lwd = 2)
ablineclip(h = 0.43, x1 = 5.5, x2 = 6.7, lwd = 2)
text(6.1, 0.46, "7 x", cex = 1.3)
par(las = 1)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), lwd = 2, cex.axis = 1.3)
par(las = 0)
mtext("Probability Mass", side = 2, line = 3.7, cex = 2)
mtext("Value", side = 1, line = 3.7, cex = 2)

par(mar = c(4.6, 6, 3.3, 2) + 0.1)
xx <- c(0.0790321, 0.079031, seq(0.08, 0.4, 0.01), 0.4084, 0.4084)
yy <- c(0, pdfmix(0.079031, k = k), pdfmix(seq(0.08, 0.4, 0.01), k = k), pdfmix(0.4084, k = k), 
    0)
plot(1, type = "n", axes = FALSE, ylab = "", xlab = "", xlim = c(0, 1), 
    ylim = c(0, 3))
polygon(xx, yy, col = "grey", border = NA)
curve(pdfmix(x, k = k), from = 0, to = 1, lwd = 2, ylab = "", xlab = "", xlim = c(0, 
    1), ylim = c(0, 3), add = TRUE)
text(0.25, 0.7, "0.4", cex = 1.3)
par(las = 1)
axis(2, at = seq(0, 3, 0.5), labels = seq(0, 3, 0.5), lwd = 2, cex.axis = 1.3)
points(0.539580297, pdfmix(0.539580297, k = k), pch = 21, bg = "white", cex = 1.4, 
    lwd = 2.7)
points(uniroot(function(x) pdfmix(x, k = k) - 5 * pdfmix(0.539580297, k = k), interval = c(0.56, 
    0.7))$root, pdfmix(uniroot(function(x) pdfmix(x, k = k) - 5 * pdfmix(0.539580297, k = k), 
    interval = c(0.56, 0.7))$root, k = k), pch = 21, bg = "white", cex = 1.4, 
    lwd = 2.7)
arrows(x0 = 0.539580297, x1 = 0.539580297, y0 = 2.7, y1 = 0.7, length = c(0.17, 
    0.17), angle = 19, lwd = 2)
ablineclip(h = 2.7, x1 = 0.539580297, x2 = 0.6994507, lwd = 2)
ablineclip(v = 0.6994507, y1 = 2.55, y2 = 2.7, lwd = 2)
text(0.6194593, 2.79, "5 x", cex = 1.3)
axis(1, at = seq(0, 1, 0.1), labels = c("0", ".1", ".2", ".3", ".4", ".5", 
    ".6", ".7", ".8", ".9", "1"), line = -1.2, lwd = 2, cex.axis = 1.37)
par(las = 0)
mtext("Probability Density", side = 2, line = 3.7, cex = 2)
mtext("Value", side = 1, line = 2.4, cex = 2)

par(op)