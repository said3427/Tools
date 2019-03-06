# rm(list=ls())

# height of president divided by height of most successful opponent:
height.ratio <- c(0.924324324, 1.081871345, 1, 0.971098266, 1.029761905, 
    0.935135135, 0.994252874, 0.908163265, 1.045714286, 1.18404908, 1.115606936, 
    0.971910112, 0.97752809, 0.978609626, 1, 0.933333333, 1.071428571, 
    0.944444444, 0.944444444, 1.017142857, 1.011111111, 1.011235955, 1.011235955, 
    1.089285714, 0.988888889, 1.011111111, 1.032967033, 1.044444444, 1, 
    1.086705202, 1.011560694, 1.005617978, 1.005617978, 1.005494505, 1.072222222, 
    1.011111111, 0.983783784, 0.967213115, 1.04519774, 1.027777778, 1.086705202, 
    1, 1.005347594, 0.983783784, 0.943005181, 1.057142857)

# proportion popular vote for president vs most successful opponent
pop.vote <- c(0.427780852, 0.56148981, 0.597141922, 0.581254292, 0.530344067, 
    0.507425996, 0.526679292, 0.536690951, 0.577825976, 0.573225387, 0.550410082, 
    0.559380032, 0.484823958, 0.500466176, 0.502934212, 0.49569636, 0.516904414, 
    0.522050547, 0.531494442, 0.60014892, 0.545079801, 0.604274986, 0.51635906, 
    0.63850958, 0.652184407, 0.587920412, 0.5914898, 0.624614752, 0.550040193, 
    0.537771958, 0.523673642, 0.554517134, 0.577511576, 0.500856251, 0.613444534, 
    0.504063153, 0.617883695, 0.51049949, 0.553073235, 0.59166415, 0.538982024, 
    0.53455133, 0.547304058, 0.497350649, 0.512424242, 0.536914796)

## now calculate BF sequentially; two-sided test
library("hypergeo")
BF10.HG.exact = function(n, r) {
    # Jeffreys' test for whether a correlation is zero or not Jeffreys
    # (1961), pp. 289-292 Note that if the means are subtracted, n needs to
    # be replaced by n-1
    hypgeo = hypergeo((0.25 + n/2), (-0.25 + n/2), (3/2 + n/2), r^2)
    BF10 = (sqrt(pi) * gamma(n/2 + 1) * (hypgeo))/(2 * gamma(3/2 + n/2))
    return(as.numeric(BF10))
}

BF10 <- array()
BF10[1] <- 1
BF10[2] <- 1

for (i in 3:length(height.ratio)) {
    BF10[i] <- BF10.HG.exact(n = i - 1, r = cor(height.ratio[1:i], pop.vote[1:i]))
}

# We wish to plot this Bayes factor sequentially, as it unfolds as more
# elections become available: ============ Plot log Bayes factors ================

par(cex.main = 1.3, mar = c(4.5, 6, 4, 7) + 0.1, mgp = c(3, 1, 0), cex.lab = 1.3, 
    font.lab = 2, cex.axis = 1.3, las = 1)
xhigh <- 60
plot(log(BF10), xlim = c(1, xhigh), ylim = c(-1 * log(200), log(200)), 
    xlab = "", ylab = "", cex.lab = 1.3, cex.axis = 1.3, las = 1, yaxt = "n", 
    bty = "n", type = "p", pch = 21, bg = "grey")

labelsUpper = log(c(100, 30, 10, 3, 1))
labelsLower = -1 * labelsUpper
criticalP = c(labelsLower, 0, labelsUpper)
for (idx in 1:length(criticalP)) {
    abline(h = criticalP[idx], col = "darkgrey", lwd = 1, lty = 2)
}
abline(h = 0)
axis(side = 4, at = criticalP, tick = TRUE, las = 2, cex.axis = 1, labels = FALSE)
axis(side = 4, at = labelsUpper + 0.602, tick = FALSE, cex.axis = 1, labels = c("Extreme", 
    "Very strong", "Strong", "Moderate", "Anecdotal"))
axis(side = 4, at = labelsLower - 0.602, tick = FALSE, cex.axis = 1, labels = c("Extreme", 
    "Very strong", "Strong", "Moderate", "Anecdotal"))

axis(side = 2, at = c(criticalP), tick = TRUE, las = 2, cex.axis = 1, labels = c("1/100", 
    "1/30", "1/10", "1/3", "1", "", "100", "30", "10", "3", ""))

mtext(expression(BF[1][0]), side = 2, line = 2.5, las = 0, cex = 1.3)
grid::grid.text("Evidence", 0.97, 0.5, rot = 270, gp = grid::gpar(cex = 1.3))
mtext("No. of Elections", side = 1, line = 2.5, las = 1, cex = 1.3)

arrows(20, -log(10), 20, -log(100), length = 0.25, angle = 30, code = 2, 
    lwd = 2)
arrows(20, log(10), 20, log(100), length = 0.25, angle = 30, code = 2, 
    lwd = 2)
text(25, -log(70), "Evidence for H0", pos = 4, cex = 1.3)
text(25, log(70), "Evidence for H1", pos = 4, cex = 1.3)