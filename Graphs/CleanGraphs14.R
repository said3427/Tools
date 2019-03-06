x <- seq(0, 1, 0.001)
y <- dbeta(x, 2, 4)

y1 <- 0.25
y2 <- 0.62

par(cex.main = 2, mar = c(4, 2, 4, 2) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 2, 
    font.lab = 2, cex.axis = 2, bty = "n", las = 1, lwd = 3)

layout(matrix(c(1, 2), 1, 2))

########################################################## UNBIASED THRESHOLDS ######

plot(x, y, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "Unbiased Thresholds", bty = "n", yaxt = "n", xaxt = "n", xpd = FALSE)
polygon(c(x[which(x < 0.2)], 0.2, 0), c(y[which(x < 0.2)], 0, 0), col = "#E0E0E0", 
    xpd = FALSE)
polygon(c(x[which(x >= 0.2 & x < 0.4)], 0.4, 0.2), c(y[which(x >= 0.2 & x < 0.4)], 
    0, 0), col = "#C0C0C0", xpd = FALSE)
polygon(c(x[which(x >= 0.4 & x < 0.6)], 0.6, 0.4), c(y[which(x >= 0.4 & x < 0.6)], 
    0, 0), col = "#A0A0A0", xpd = FALSE)
polygon(c(x[which(x >= 0.6 & x < 0.8)], 0.8, 0.6), c(y[which(x >= 0.6 & x < 0.8)], 
    0, 0), col = "#808080", xpd = FALSE)
polygon(c(x[which(x >= 0.8)], 1, 0.8), c(y[which(x >= 0.8)], 0, 0), col = "#606060", 
    xpd = FALSE)
axis(1, c(0, 1), c("0", "1"), lwd = 0, cex = 2, pos = -0.09)
abline(v = 0)
abline(v = 1)
l <- seq(0, 3.2, 0.01)
x1 <- rep(0.2, length(l))
par(new = TRUE)
plot(x1, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
x2 <- rep(0.4, length(l))
par(new = TRUE)
plot(x2, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
x3 <- rep(0.6, length(l))
par(new = TRUE)
plot(x3, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
x4 <- rep(0.8, length(l))
par(new = TRUE)
plot(x4, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
text(0.2, 3.4, expression(gamma[1]), cex = 2)
text(0.4, 3.4, expression(gamma[2]), cex = 2)
text(0.6, 3.4, expression(gamma[3]), cex = 2)
text(0.8, 3.4, expression(gamma[4]), cex = 2)
text(0.1, 2.6, "1", cex = 2)
text(0.3, 2.6, "2", cex = 2)
text(0.5, 2.6, "3", cex = 2)
text(0.7, 2.6, "4", cex = 2)
text(0.9, 2.6, "5", cex = 2)

########################################################## BIASED THRESHOLDS ######

a <- 0.75  #scaling parameter
b <- 1.5  #shifting parameter
thr_1 <- (b * (0.2^a))/((1 - 0.2)^a + b * (0.2^a))
thr_2 <- (b * (0.4^a))/((1 - 0.4)^a + b * (0.4^a))
thr_3 <- (b * (0.6^a))/((1 - 0.6)^a + b * (0.6^a))
thr_4 <- (b * (0.8^a))/((1 - 0.8)^a + b * (0.8^a))

plot(x, y, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "Biased Thresholds", bty = "n", yaxt = "n", xaxt = "n", xpd = FALSE)
polygon(c(x[which(x < thr_1)], thr_1, 0), c(y[which(x < thr_1)], 0, 0), col = "#E0E0E0", 
    xpd = FALSE)
polygon(c(x[which(x >= thr_1 & x < thr_2)], thr_2, thr_1), c(y[which(x >= thr_1 & 
    x < thr_2)], 0, 0), col = "#C0C0C0", xpd = FALSE)
polygon(c(x[which(x >= thr_2 & x < thr_3)], thr_3, thr_2), c(y[which(x >= thr_2 & 
    x < thr_3)], 0, 0), col = "#A0A0A0", xpd = FALSE)
polygon(c(x[which(x >= thr_3 & x < thr_4)], thr_4, thr_3), c(y[which(x >= thr_3 & 
    x < thr_4)], 0, 0), col = "#808080", xpd = FALSE)
polygon(c(x[which(x >= thr_4)], 1, thr_4), c(y[which(x >= thr_4)], 0, 0), col = "#606060", 
    xpd = FALSE)
axis(1, c(0, 1), c("0", "1"), lwd = 0, cex = 2, pos = -0.09)
abline(v = 0)
abline(v = 1)
l <- seq(0, 3.2, 0.01)
x1 <- rep(thr_1, length(l))
par(new = TRUE)
plot(x1, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
x2 <- rep(thr_2, length(l))
par(new = TRUE)
plot(x2, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
x3 <- rep(thr_3, length(l))
par(new = TRUE)
plot(x3, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
x4 <- rep(thr_4, length(l))
par(new = TRUE)
plot(x4, l, type = "l", ylim = c(0, 3.5), xlim = c(0, 1), xlab = "", ylab = "", 
    main = "", bty = "n", yaxt = "n", xaxt = "n")
text(thr_1, 3.4, expression(delta[i1]), cex = 2)
text(thr_2, 3.4, expression(delta[i2]), cex = 2)
text(thr_3, 3.4, expression(delta[i3]), cex = 2)
text(thr_4, 3.4, expression(delta[i4]), cex = 2)
text(thr_1/2, 2.6, "1", cex = 2)
text((thr_1 + thr_2)/2, 2.6, "2", cex = 2)
text((thr_2 + thr_3)/2, 2.6, "3", cex = 2)
text((thr_3 + thr_4)/2, 2.6, "4", cex = 2)
text((thr_4 + 1)/2, 2.6, "5", cex = 2)