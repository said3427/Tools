Max.BF10 = function(p) {
    # Computes the upper bound on the Bayes factor As in Sellke, Bayarri, &
    # Berger, 2001
    Max.BF10 <- -1/(exp(1) * p * log(p))
    return(Max.BF10)
}

# Plot this function for p in .001 to .1
xlow <- 0.001
xhigh <- 0.1
p1 <- 0.0373
p2 <- 0.00752
p3 <- 0.001968
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
plot(function(p) Max.BF10(p), xlow, xhigh, xlim = c(xlow, xhigh), lwd = 2, xlab = " ", 
    ylab = " ")
mtext("Two-sided p value", 1, line = 2.5, cex = 1.5, font = 2)
mtext("Maximum Bayes factor for H1", 2, line = 2.8, cex = 1.5, font = 2, las = 0)
lines(c(0, p1), c(3, 3), lwd = 2, col = "grey")
lines(c(0, p2), c(10, 10), lwd = 2, col = "grey")
lines(c(0, p3), c(30, 30), lwd = 2, col = "grey")
lines(c(p1, p1), c(0, 3), lwd = 2, col = "grey")
lines(c(p2, p2), c(0, 10), lwd = 2, col = "grey")
lines(c(p3, p3), c(0, 30), lwd = 2, col = "grey")

cexsize <- 1.2
text(0.005, 31, expression(max((BF[10])) == 30 %<->% p %~~% 0.002), cex = cexsize, 
    pos = 4)
text(0.01, 11, expression(max((BF[10])) == 10 %<->% p %~~% 0.008), cex = cexsize, 
    pos = 4)
text(p1 - 0.005, 5, expression(max((BF[10])) == 3 %<->% p %~~% 0.037), cex = cexsize, 
    pos = 4)