library(plyr)

mean.prop.sw <- c(0.7, 0.6, 0.67, 0.5, 0.45, 0.48, 0.41, 0.34, 0.5, 0.33)
sd.prop.sw <- c(0.3, 0.4, 0.2, 0.35, 0.28, 0.31, 0.29, 0.26, 0.21, 0.23)
N <- 100
b <- barplot(mean.prop.sw, las = 1, xlab = " ", ylab = " ", col = "grey", cex.lab = 1.7, 
    cex.main = 1.5, axes = FALSE, ylim = c(0, 1))

axis(1, c(0.8, 2, 3.2, 4.4, 5.6, 6.8, 8, 9.2, 10.4, 11.6), 1:10, cex.axis = 1.3)
axis(2, seq(0, 0.8, by = 0.2), cex.axis = 1.3, las = 1)
mtext("Block", side = 1, line = 2.5, cex = 1.5, font = 2)
mtext("Proportion of Switches", side = 2, line = 3, cex = 1.5, font = 2)
l_ply(seq_along(b), function(x) arrows(x0 = b[x], y0 = mean.prop.sw[x], x1 = b[x], 
    y1 = mean.prop.sw[x] + 1.96 * sd.prop.sw[x]/sqrt(N), code = 2, length = 0.1, 
    angle = 90, lwd = 1.5))
