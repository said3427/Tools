library(plotrix)

### plot multinomial BF

load("BFMultiPi.rda")
load("maxBFMultiPi.rda")
load("exBFMultiPi.rda")

N <- seq(1000, 1e+08, 1000)

par(cex.main = 1.3, mar = c(4.5, 6, 4, 7) + 0.1, mgp = c(3, 1, 0), cex.lab = 1.3, 
    font.lab = 2, cex.axis = 1.3, las = 1)
plot(c(0, log(BFMultiPi)), xlim = c(0, 100001), ylim = c(-20, 80), xlab = "", 
    ylab = "", cex.lab = 1.3, cex.axis = 1.3, las = 1, yaxt = "n", bty = "n", 
    type = "n", pch = 21, bg = "grey", axes = FALSE, lwd = 4, main = expression(paste("Multinomial", 
        ~logBF[0][1], ~"for", ~pi, sep = " ")), cex.main = 2)
axis(2, at = seq(-20, 80, 20), labels = seq(-20, 80, 20))
options(scipen = 100, digits = 4)
axis(1, at = seq(from = 0, to = 1e+05, by = 10000), labels = seq(from = 0, 
    to = 1e+05, by = 10000))
ablineclip(h = 0, lty = 2, x2 = 1e+05, y2 = 0)
mtext(expression(logBF[0][1]), side = 2, line = 3.1, las = 0, cex = 1.7)
mtext(expression("No. of Decimal Places of" ~ pi ~ "(No./1000)"), side = 1, 
    line = 3.1, las = 1, cex = 1.3)
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 170, maxColorValue = 255)
points(c(0, seq_along(N)), c(0, log(maxBFMultiPi)), type = "l", lwd = 4, 
    col = "red")
points(c(0, seq_along(N)), c(0, log(exBFMultiPi)), type = "l", lwd = 3, 
    col = greycol)
yy <- c(log(maxBFMultiPi), rev(log(exBFMultiPi)))
xx <- c(N/1000, rev(N/1000))
polygon(xx, yy, col = greycol)
text(10000, 7, "Evidence for H0", pos = 4, cex = 1.3)
text(10000, -7, "Evidence for H1", pos = 4, cex = 1.3)
arrows(7000, -2, 7000, -14, length = 0.25, angle = 30, code = 2, lwd = 2)
arrows(7000, 2, 7000, 14, length = 0.25, angle = 30, code = 2, lwd = 2)
points(c(0, seq_along(N)), c(0, log(BFMultiPi)), type = "l", lwd = 3, 
    col = "black")
op <- par(lend = 1)
legend(x = 54000, 45, legend = c(expression(max.BF[0][1]), expression(BF[0][1]), 
    expression("95%" ~ BF[0][1] ~ "|" ~ H[0])), lty = c(1, 1, 1), lwd = c(3, 
    3, 20), col = c("red", "black", greycol), bty = "n", x.intersp = 0.5, 
    cex = 1.2)
par(op)
text(39600, 76, "D(a=1)", cex = 1.3)
text(40000, 34.5, "D(a=50)", cex = 1.3)

# add Dirichlet a=50 prior

load("BFMultiPi50.rda")
load("maxBFMultiPi50.rda")
load("exBFMultiPi50.rda")

greycol2 <- rgb(red = 190, green = 190, blue = 190, alpha = 60, maxColorValue = 255)
yy <- c(log(maxBFMultiPi50), rev(log(exBFMultiPi50)))
xx <- c(N/1000, rev(N/1000))
polygon(xx, yy, col = greycol2)
red2 <- rgb(red = 255, green = 0, blue = 0, alpha = 80, maxColorValue = 255)
points(c(0, seq_along(N)), c(0, (log(maxBFMultiPi50))), type = "l", lwd = 4, 
    col = red2, lty = 1)
black2 <- red2 <- rgb(red = 100, green = 100, blue = 100, alpha = 200, 
    maxColorValue = 300)
points(c(0, seq_along(N)), c(0, (log(BFMultiPi50))), type = "l", lwd = 4, 
    lty = 1, col = black2)