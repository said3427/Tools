par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

yhigh <- 3
xi <- c(1, 0.5, 0.1)
plot(function(x) dbeta(x, xi[1], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
    lwd = 2, lty = 1, xlab = " ", ylab = " ")
mtext("p value", 1, line = 2.5, cex = 1.5, font = 2)
mtext("Density", 2, line = 3, cex = 1.5, font = 2, las = 0)

par(new = TRUE)
plot(function(x) dbeta(x, xi[2], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
    lwd = 2, lty = 2, xlab = " ", ylab = " ")

par(new = TRUE)
plot(function(x) dbeta(x, xi[3], 1), 0, 1, ylim = c(0, yhigh), xlim = c(0, 1), 
    lwd = 2, lty = 3, xlab = " ", ylab = " ")

cexsize <- 1.5
text(0.5, 1.15, expression(xi == 1(i.e., H[0])), cex = cexsize, pos = 4)
text(0.1, 1.6, expression(xi == 0.5), cex = cexsize, pos = 4)
text(0, 0.2, expression(xi == 0.1), cex = cexsize, pos = 4)