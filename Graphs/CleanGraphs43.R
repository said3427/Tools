### prior & posterior parameters
mean.prior <- 75
sd.prior <- 12
mean.posterior <- 73.33644
sd.posterior <- 4.831067

### plot settings
xlim <- c(40, 115)
ylim <- c(0, 0.117)
lwd <- 2
lwd.points <- 2
lwd.axis <- 1.2
cex.points <- 1.4
cex.axis <- 1.2
cex.text <- 1.1
cex.labels <- 1.3
cexLegend <- 1.2

op <- par(mar = c(5.1, 4.1, 4.1, 2.1))

### create empty canvas
plot(1, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "")

### shade prior area < 70
greycol1 <- rgb(0, 0, 0, alpha = 0.2)
greycol2 <- rgb(0, 0, 0, alpha = 0.4)

polPrior <- seq(xlim[1], 70, length.out = 400)
xx <- c(polPrior, polPrior[length(polPrior)], polPrior[1])
yy <- c(dnorm(polPrior, mean.prior, sd.prior), 0, 0)
polygon(xx, yy, col = greycol1, border = NA)

### shade posterior area < 70
polPosterior <- seq(xlim[1], 70, length.out = 400)
xx <- c(polPosterior, polPosterior[length(polPosterior)], polPosterior[1])
yy <- c(dnorm(polPosterior, mean.posterior, sd.posterior), 0, 0)
polygon(xx, yy, col = greycol2, border = NA)

### shade posterior area on interval (81, 84)
polPosterior2 <- seq(81, 84, length.out = 400)
xx <- c(polPosterior2, polPosterior2[length(polPosterior2)], polPosterior2[1])
yy <- c(dnorm(polPosterior2, mean.posterior, sd.posterior), 0, 0)
polygon(xx, yy, col = greycol2, border = NA)

### grey dashed lines to prior mean, posterior mean and posterior at 77
lines(rep(mean.prior, 2), c(0, dnorm(mean.prior, mean.prior, sd.prior)), lty = 2, col = "grey", 
    lwd = lwd)
lines(rep(mean.posterior, 2), c(0, dnorm(mean.posterior, mean.posterior, sd.posterior)), 
    lty = 2, col = "grey", lwd = lwd)
lines(rep(mean.posterior + (mean.posterior - 70), 2), c(0, dnorm(mean.posterior + (mean.posterior - 
    70), mean.posterior, sd.posterior)), lty = 2, col = "grey", lwd = lwd)

### axes
axis(1, at = seq(xlim[1], xlim[2], 5), cex.axis = cex.axis, lwd = lwd.axis)
axis(2, labels = FALSE, tck = 0, lwd = lwd.axis, line = -0.5)

### axes labels
mtext("IQ Bob", side = 1, cex = 1.6, line = 2.4)
mtext("Density", side = 2, cex = 1.5, line = 0)

### plot prior and posterior

# prior
plot(function(x) dnorm(x, mean.prior, sd.prior), xlim = xlim, ylim = ylim, xlab = "", 
    ylab = "", lwd = lwd, lty = 3, add = TRUE)

# posterior
plot(function(x) dnorm(x, mean.posterior, sd.posterior), xlim = xlim, ylim = ylim, add = TRUE, 
    lwd = lwd)

### add points

# posterior density at 70
points(70, dnorm(70, mean.posterior, sd.posterior), pch = 21, bg = "white", cex = cex.points, 
    lwd = lwd.points)

# posterior density at 76.67
points(mean.posterior + (mean.posterior - 70), dnorm(mean.posterior + (mean.posterior - 
    70), mean.posterior, sd.posterior), pch = 21, bg = "white", cex = cex.points, lwd = lwd.points)

# maximum a posteriori value
points(mean.posterior, dnorm(mean.posterior, mean.posterior, sd.posterior), pch = 21, 
    bg = "white", cex = cex.points, lwd = lwd.points)

### credible interval
CIlow <- qnorm(0.025, mean.posterior, sd.posterior)
CIhigh <- qnorm(0.975, mean.posterior, sd.posterior)
yCI <- 0.11

arrows(CIlow, yCI, CIhigh, yCI, angle = 90, code = 3, length = 0.1, lwd = lwd)
text(mean.posterior, yCI + 0.0042, labels = "95%", cex = cex.text)
text(CIlow, yCI, labels = paste(round(CIlow, 2)), cex = cex.text, pos = 2, offset = 0.3)
text(CIhigh, yCI, labels = paste(round(CIhigh, 2)), cex = cex.text, pos = 4, offset = 0.3)

### legend
legendPosition <- 115
legend(legendPosition, ylim[2] + 0.002, legend = c("Posterior", "Prior"), lty = c(1, 
    3), bty = "n", lwd = c(lwd, lwd), cex = cexLegend, xjust = 1, yjust = 1, x.intersp = 0.6, 
    seg.len = 1.2)

### draw labels

# A
arrows(x0 = 57, x1 = 61, y0 = dnorm(62, mean.prior, sd.prior) + 0.0003, y1 = dnorm(62, 
    mean.prior, sd.prior) - 0.007, length = c(0.08, 0.08), lwd = lwd, code = 2)
text(55.94, dnorm(5, mean.prior, sd.prior) + 0.0205, labels = "A", cex = cex.labels)

# B
arrows(x0 = 64.5, x1 = 69, y0 = dnorm(68, mean.posterior, sd.posterior) + 0.003, y1 = dnorm(68, 
    mean.posterior, sd.posterior) - 0.005, length = c(0.08, 0.08), lwd = lwd, code = 2)
text(63.5, dnorm(68, mean.posterior, sd.posterior) + 0.0042, labels = "B", cex = cex.labels)

# C
arrows(x0 = mean.posterior + 1, x1 = mean.posterior + 6, y0 = dnorm(mean.posterior, mean.posterior, 
    sd.posterior) + 0.001, y1 = dnorm(mean.posterior, mean.posterior, sd.posterior) + 
    0.008, length = c(0.08, 0.08), lwd = lwd, code = 1)
text(mean.posterior + 7, dnorm(mean.posterior, mean.posterior, sd.posterior) + 0.0093, 
    labels = "C", cex = cex.labels)

# D
arrows(x0 = 70 - 0.25, x1 = 70 - 0.25, y0 = dnorm(70, mean.posterior, sd.posterior) + 
    0.005, y1 = 0.092, length = c(0.08, 0.08), lwd = lwd, code = 1)
lines(c(70 - 0.25, mean.posterior), rep(0.092, 2), lwd = lwd)
arrows(x0 = mean.posterior, x1 = mean.posterior, y0 = 0.092, y1 = dnorm(mean.posterior, 
    mean.posterior, sd.posterior) + 0.003, length = c(0.08, 0.08), lwd = lwd, code = 2)
ratio <- dnorm(mean.posterior, mean.posterior, sd.posterior)/dnorm(70, mean.posterior, 
    sd.posterior)
text(mean(c(70 - 0.255, mean.posterior)), 0.096, labels = paste(round(ratio, 2), "x"), 
    cex = cex.text)
text(70 - 1.5, dnorm(70, mean.posterior, sd.posterior) + 0.02, labels = "D", cex = cex.labels)

# E
arrows(x0 = 70 + 1, x1 = mean.posterior + (mean.posterior - 70) - 1, y0 = dnorm(70, mean.posterior, 
    sd.posterior), y1 = dnorm(mean.posterior + (mean.posterior - 70), mean.posterior, 
    sd.posterior), length = c(0.08, 0.08), lwd = lwd, code = 3)
text(74.9, dnorm(mean.posterior + (mean.posterior - 70), mean.posterior, sd.posterior) - 
    0.005, labels = "E", cex = cex.labels)

# F
arrows(x0 = 82.5, x1 = 87, y0 = dnorm(82, mean.posterior, sd.posterior) - 0.012, y1 = dnorm(82, 
    mean.posterior, sd.posterior) - 0.005, length = c(0.08, 0.08), lwd = lwd, code = 1)
text(88, dnorm(82, mean.posterior, sd.posterior) - 0.0034, labels = "F", cex = cex.labels)

# G
arrows(x0 = CIhigh + 6, x1 = CIhigh + 8.2, y0 = yCI, y1 = yCI, length = c(0.08, 0.08), 
    lwd = lwd, code = 1)
text(CIhigh + 9.5, yCI, labels = "G", cex = cex.labels)

### additional information
scores <- "Bob's IQ scores: {73, 67, 79}"
priorText1 <- "Prior distribution:"
priorText2 <- expression(paste("IQ Bob ~ N(", 75, ", ", 12^2, ")"))
posteriorText1 <- "Posterior distribution:"
posteriorText2 <- expression(paste("IQ Bob ~ N(", 73.34, ", ", 4.83^2, ")"))

xx <- 87
yCI2 <- 0.12

text(xx, yCI2 - 0.033, labels = priorText1, cex = cexLegend, pos = 4, offset = 0.3)
text(xx, yCI2 - 0.042, labels = priorText2, cex = cexLegend, pos = 4, offset = 0.3)
text(xx, yCI2 - 0.059, labels = scores, cex = cexLegend, pos = 4, offset = 0.3)
text(xx, yCI2 - 0.074, labels = posteriorText1, cex = cexLegend, pos = 4, offset = 0.3)
text(xx, yCI2 - 0.083, labels = posteriorText2, cex = cexLegend, pos = 4, offset = 0.3)

par(op)