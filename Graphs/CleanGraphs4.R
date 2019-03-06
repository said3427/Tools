# rm(list = ls())
library(plyr)  # needed for function 'l_ply'
# Data: Proportion of choices from the good decks as reported in 39 studies
good.choices <- c(0.43, 0.47, 0.47, 0.48, 0.5, 0.52, 0.53, 0.53, 0.54, 0.54, 
    0.54, 0.54, 0.55, 0.55, 0.55, 0.56, 0.56, 0.57, 0.57, 0.57, 0.57, 0.58, 0.58, 
    0.58, 0.59, 0.59, 0.6, 0.62, 0.63, 0.63, 0.64, 0.64, 0.66, 0.66, 0.67, 0.67, 
    0.68, 0.7, 0.7)
yhigh <- 8
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
h <- hist(good.choices, freq = FALSE, main = "", xlab = "", ylab = " ", ylim = c(0, 
    yhigh), xlim = c(0.3, 0.8), axes = FALSE, col = "grey")
l_ply(seq_along(h$density), function(x) text(h$mids[x], h$density[x] + 0.32, 
    round(h$density[x], 2), cex = 1.2))
axis(1, seq(0.3, 0.8, by = 0.1))
axis(2, labels = FALSE, lwd.ticks = 0)
rug(jitter(good.choices))
mtext("Prop. Choices from Good Decks", side = 1, line = 2.5, cex = 1.5, font = 2)
mtext("Density of Studies", side = 2, line = 1, cex = 1.5, font = 2, las = 0)