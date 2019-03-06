################################## load data #############

# Social Priming Research, all between-subject studies, N=159:
dataSocialPriming <- read.csv("SocialPriming.csv", sep = ";")
# Control Studies, between-subject, N=130:
dataControlsSocialPriming <- read.csv("ControlsSocialPriming.csv", sep = ";")

# p values
pValuesSocialPriming <- dataSocialPriming$pvalue
pValuesControlsSocialPriming <- dataControlsSocialPriming$pvalue

################################## 4-panel plot ###########

source("HDIofMCMC.R")

plotphi <- function(samples, Nbreaks = 80, xlow = 0, xhigh = 1, ylow = 0, 
    yhigh = 10) {
	
    # Plots the mixture proportion p(H0)

    phi <- samples$BUGSoutput$sims.list$phi
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), 
        cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
    y <- hist(phi, Nbreaks, plot = F)
    plot(c(y$breaks, max(y$breaks)), c(0, y$density, 0), type = "S", lwd = 2, 
        lty = 1, xlim = c(xlow, xhigh), ylim = c(ylow, yhigh), xlab = "", 
        ylab = "", col = "black")
    mtext("H0 Assignment Rate", side = 1, line = 2.7, cex = 1.5)
    par(las = 0)
    mtext("Posterior Density", side = 2, line = 2.5, cex = 1.5)
    lines(c(0.5, 0.5), c(ylow, yhigh), cex = 2, lty = 2)
}

plotz <- function(pvals, samples) {
	
    # Plots the p-value against the probability of being classified as from H0
	
    z <- samples$BUGSoutput$mean$ind
    par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), 
        cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
    plot(sort(pvals, dec = TRUE), z, col = "black", pch = 21, bg = "grey", 
        cex = 2, xlim = c(0, 0.05), ylim = c(0, 1), ylab = "", xlab = "", 
        axes = F)
    axis(1)
    axis(2)
    par(las = 0)
    mtext("Significant P Values", side = 1, line = 2.9, cex = 1.5)
    mtext("Probability H0 Assignment", side = 2, line = 3.5, cex = 1.5)
    lines(c(0, 0.05), c(0.5, 0.5), cex = 2, lty = 2)
}

plotpredqq <- function(pvals, samples, ks.test = FALSE) {
	
    # Draws qq plot; computes ks test
	
    predp <- pnorm(samples$BUGSoutput$sims.list$predqp)
    par(cex.main = 1.5, mar = c(5, 6, 4, 4) + 0.1, mgp = c(3.5, 1, 0), 
        cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
    qqplot(pvals, predp, xlab = "", ylab = "", axes = F, col = "black")
    axis(1)
    axis(2)
    mtext("Observed P Value Quantiles", side = 1, line = 2.9, cex = 1.5)
    par(las = 0)
    mtext("Predicted Quantiles ", side = 2, line = 3.7, cex = 1.7)
    abline(a = 0, b = 1, lty = 3)
    if (ks.test == TRUE) 
        ks.test(pvals, predp)
}

histP <- function(pvals, samples, yhigh = 100, col = "lightblue") {
	
    # Plots histogram of the p-values
	
    predp <- pnorm(samples$BUGSoutput$sims.list$predqp)
    par(cex.main = 1.5, mar = c(5, 6, 4, 4) + 0.1, mgp = c(3.5, 1, 0), 
        cex.lab = 1.5, font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
    hist(pvals, freq = TRUE, xlab = "", ylab = "", col = col, main = "", 
        ylim = c(0, yhigh), xlim = c(0, 0.05), axes = F)
    axis(1)
    par(las = 1)
    axis(2)
    mtext("Significant P Values", side = 1, line = 2.9, cex = 1.5)
    par(las = 0)
    mtext("Number of P Values", side = 2, line = 3.3, cex = 1.6)
    rug(pvals)
}

### load samples

load("samplesSocialPriming_DirichletJags.Rdata")
load("samplesControlsSocialPriming_DirichletJags.Rdata")

pvals <- pValuesSocialPriming
pvals2 <- pValuesControlsSocialPriming
samplesP <- samplesSocialPriming_DirichletJags
samplesP2 <- samplesControlsSocialPriming_DirichletJags

### create plot

op <- par(mfrow = c(2, 2))

yhigh <- 80
greycol2 <- rgb(0, 0, 0, alpha = 0.7)
histP(pvals, samplesP, yhigh, col = greycol2)
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 95, maxColorValue = 255)
hist(pvals2, freq = TRUE, xlab = "", ylab = "", main = "", ylim = c(0, 
    yhigh), xlim = c(0, 0.05), col = greycol, axes = F, add = TRUE, lty = 2)
op2 <- par(lend = 1)
legend(x = 0.006, 45, legend = c("Social Priming Studies (N=159)", "Control Studies (N=130)"), 
    lty = c(1, 1, 1), lwd = c(15, 15), col = c(greycol2, greycol), bty = "n", 
    x.intersp = 0.5, cex = 1.4)
par(op2)

plotphi(samplesP, yhigh = 6)
Nbreaks <- 80
xlow <- 0
xhigh <- 1
ylow <- 0
yhigh <- 10
phi1 <- samplesP$BUGSoutput$sims.list$phi
phi2 <- samplesP2$BUGSoutput$sims.list$phi
y <- hist(phi2, Nbreaks, plot = F)
greycol2 <- rgb(0, 0, 0, alpha = 0.5)
lines(c(y$breaks, max(y$breaks)), c(0, y$density, 0), lwd = 2, lty = 1, 
    col = greycol2)
par(las = 1)
text(0.91, 2.7, c("Social \nPriming \nStudies"), cex = 1.3)
text(0.09, 3.5, c("Control \nStudies"), cex = 1.3)
HDI1 <- HDIofMCMC(phi1)  # [0.414, 0.865]
HDI2 <- HDIofMCMC(phi2)  # [0.061, 0.446]
arrows(x0 = HDI1[1], y0 = 4, x1 = HDI1[2], y1 = 4, angle = 90, code = 3, 
    length = 0.08)
mtext("95%", side = 3, at = mean(HDI1), line = -5)
arrows(x0 = HDI2[1], y0 = 4.8, x1 = HDI2[2], y1 = 4.8, angle = 90, code = 3, 
    length = 0.08, col = greycol2)
mtext("95%", side = 3, at = mean(HDI2), line = -3.1, col = greycol2)

plotz(pvals, samplesP)
z <- samplesP2$BUGSoutput$mean$ind
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 40, maxColorValue = 255)
greycol2 <- rgb(0, 0, 0, alpha = 0.12)
points(sort(pvals2, dec = TRUE), z, col = greycol2, pch = 21, bg = greycol, 
    cex = 2)
text(0.02, 0.92, "Social Priming Studies", cex = 1.4, pos = 4)
text(0.025, 0.38, "Control Studies", cex = 1.4, pos = 4)

plotpredqq(pvals, samplesP)
predp <- pnorm(samplesP2$BUGSoutput$sims.list$predqp)
d <- qqplot(pvals2, predp, xlab = "", ylab = "", axes = FALSE, plot = FALSE)
greycol2 <- rgb(0, 0, 0, alpha = 0.3)
points(d$x, d$y, col = greycol2)
legend(0.019, 0.019, legend = c("Social Priming Studies", "Control Studies"), 
    pch = rep(1, 2), col = c("black", greycol2), lwd = c(2.3, 2.3), lty = c(NA, 
        NA), bty = "n", x.intersp = 0, cex = 1.4)

par(op)