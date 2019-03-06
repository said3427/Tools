############################### @ plot replication BFs @##

load("logRepBFs.Rdata")
load("scaleBF.Rdata")
load("scaleBF2.Rdata")
load("indices.Rdata")

studies <- c("MoralInversion", "BadTipper", "BeliefInconsistency", "MoralCliff", "ColdHearted", "BurnInHell", "Biggot", "PresumptionOfGuilt", "intuitiveEconomics", 
    "HigherCompany", "higherCharity")

op <- par(mar = c(4, 26, 1, 4), mfrow = c(2, 1))

plot(1, xlim = scaleBF$lim, ylim = c(0, 12), type = "n", axes = FALSE, xlab = "", ylab = "")

lines(rep(0, 2), c(0.5, 11), lty = 2, lwd = 2)
greycol <- rgb(0, 0, 0, alpha = 0.2)
rect(xleft = log(1/100), ybottom = 0.5, xright = log(100), ytop = 11, lwd = 2, col = greycol, border = NA)

y <- 11
for (study in studies) {
    
    points(logRepBFs[[study]], rep(y, length(logRepBFs[[study]])), pch = 21, bg = "grey", cex = 1.5, lwd = 1.3)
    y <- y - 1
    
}

axis(1, at = scaleBF$at, labels = scaleBF$lab, cex.axis = 1.4, lwd = 1.4, line = -1.5)
axis(2, at = 1:11, labels = NA, las = 1, cex.axis = 1.4, lwd = 1.4)
axis(2, at = 11, labels = expression(paste("Moral Inversion ", (BF[1][0] == 7.526))), las = 1, cex.axis = 1.4)
axis(2, at = 10, labels = expression(paste("Bad Tipper ", (BF[1][0] == 5.525))), las = 1, cex.axis = 1.4)
axis(2, at = 9, labels = expression(paste("Belief-Act Inconsistency ", (BF[1][0] == 1.119))), las = 1, cex.axis = 1.4)
axis(2, at = 8, labels = expression(paste("Moral Cliff ", (BF[1][0] == 3.084 %*% 10^7))), las = 1, cex.axis = 1.4)
axis(2, at = 7, labels = expression(paste("Cold Hearted Prosociality ", (BF[1][0] == 4.155 %*% 10^24))), las = 1, cex.axis = 1.4)
axis(2, at = 6, labels = expression(paste("Burn in Hell ", (BF[1][0] == 3.031))), las = 1, cex.axis = 1.4)
axis(2, at = 5, labels = expression(paste("Bigot-Misanthrope ", (BF[1][0] == 61465))), las = 1, cex.axis = 1.4)
axis(2, at = 4, labels = expression(paste("Presumption of Guilt ", (BF[0][1] == 5.604))), las = 1, cex.axis = 1.4)
axis(2, at = 3, labels = expression(paste("Intuitive Economics ", (BF[1][0] == 7.454 %*% 10^6))), las = 1, cex.axis = 1.4)
axis(2, at = 2, labels = expression(paste("Higher Standards-Company ", (BF[0][1] == 1.781))), las = 1, cex.axis = 1.4)
axis(2, at = 1, labels = expression(paste("Higher Standards-Charity ", (BF[1][0] == 131.7))), las = 1, cex.axis = 1.4)

mtext(expression(BF["r"]["0"]), side = 1, line = 1.6, cex = 1.7)


## zoom-in ##

plot(1, xlim = scaleBF2$lim, ylim = c(0, 12), type = "n", axes = FALSE, xlab = "", ylab = "")

rect(xleft = log(1/100), ybottom = 0.5, xright = log(100), ytop = 11.2, lwd = 2, col = greycol, border = greycol, density = NA)

y <- 11
lines(rep(0, 2), c(0.5, 11), lty = 2, lwd = 2)

for (study in studies) {
    
    points(logRepBFs[[study]][indices[[study]]], rep(y, length(logRepBFs[[study]][indices[[study]]])), pch = 21, bg = "grey", cex = 1.5, lwd = 1.3)
    y <- y - 1
    
}

axis(1, at = scaleBF2$at, labels = scaleBF2$lab, cex.axis = 1.4, lwd = 1.2, line = -1.5)
axis(2, at = 1:11, labels = NA, las = 1, cex.axis = 1.4, lwd = 1.4)
axis(2, at = 11, labels = expression(paste("Moral Inversion ", (BF[1][0] == 7.526))), las = 1, cex.axis = 1.4)
axis(2, at = 10, labels = expression(paste("Bad Tipper ", (BF[1][0] == 5.525))), las = 1, cex.axis = 1.4)
axis(2, at = 9, labels = expression(paste("Belief-Act Inconsistency ", (BF[1][0] == 1.119))), las = 1, cex.axis = 1.4)
axis(2, at = 8, labels = expression(paste("Moral Cliff ", (BF[1][0] == 3.084 %*% 10^7))), las = 1, cex.axis = 1.4)
axis(2, at = 7, labels = expression(paste("Cold Hearted Prosociality ", (BF[1][0] == 4.155 %*% 10^24))), las = 1, cex.axis = 1.4)
axis(2, at = 6, labels = expression(paste("Burn in Hell ", (BF[1][0] == 3.031))), las = 1, cex.axis = 1.4)
axis(2, at = 5, labels = expression(paste("Bigot-Misanthrope ", (BF[1][0] == 61465))), las = 1, cex.axis = 1.4)
axis(2, at = 4, labels = expression(paste("Presumption of Guilt ", (BF[0][1] == 5.604))), las = 1, cex.axis = 1.4)
axis(2, at = 3, labels = expression(paste("Intuitive Economics ", (BF[1][0] == 7.454 %*% 10^6))), las = 1, cex.axis = 1.4)
axis(2, at = 2, labels = expression(paste("Higher Standards-Company ", (BF[0][1] == 1.781))), las = 1, cex.axis = 1.4)
axis(2, at = 1, labels = expression(paste("Higher Standards-Charity ", (BF[1][0] == 131.7))), las = 1, cex.axis = 1.4)

mtext(expression(BF["r"]["0"]), side = 1, line = 1.6, cex = 1.7)


par(xpd = NA)

polygon(x = c(log(1/100), log(1/42.5), log(1/38.5), log(100), log(1/100)), y = c(11.2, 15.63, 15.63, 11.2, 11.2), col = greycol, border = NA)

par(op)