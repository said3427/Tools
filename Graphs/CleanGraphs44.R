### joint prior density function
djointprior <- function(BobsTrueIQ, TestSD) {
    
    dnorm(BobsTrueIQ, mean = 75, sd = 12) * 1/10  # 1/10 because TestSD \in (5,15)
}

### function that plots the joint prior
heatmap.prior <- function() {
    
    op <- par(mar = c(5.1, 5.1, 5.1, 3.1))
    
    BobsTrueIQ <- seq(40, 110, length.out = 151)
    TestSD <- seq(5, 15, length.out = 151)
    density <- outer(BobsTrueIQ, TestSD, djointprior)
    
    image(x = BobsTrueIQ, y = TestSD, z = density, col = topo.colors(50), xlab = "", 
        ylab = "", axes = FALSE)
    
    axis(1, at = seq(40, 110, 10), las = 1, cex.axis = 1.6)
    axis(2, at = seq(5, 15, 2), las = 1, cex.axis = 1.6)
    
    mtext("IQ Bob", 1, cex = 1.4, line = 2.75)
    mtext("Test SD", 2, cex = 1.4, line = 2.8)
    
    par(op)
}

### function that plots the joint posterior
library(ks)
heatmap.posterior <- function(samples) {
    
    op <- par(mar = c(5.1, 5.1, 5.1, 3.1))
    
    BobsTrueIQ <- samples$BUGSoutput$sims.list$BobsTrueIQ
    TestSD <- samples$BUGSoutput$sims.list$TestSD
    
    d <- kde(cbind(BobsTrueIQ, TestSD), xmin = c(40, 5), xmax = c(110, 15))
    
    image(x = d$eval.points[[1]], y = d$eval.points[[2]], z = d$estimate, col = topo.colors(50), 
        xlab = "", ylab = "", axes = FALSE)
    
    axis(1, at = seq(40, 110, 10), las = 1, cex.axis = 1.6)
    axis(2, at = seq(5, 15, 2), las = 1, cex.axis = 1.6)
    
    mtext("IQ Bob", 1, cex = 1.4, line = 2.75)
    mtext("Test SD", 2, cex = 1.4, line = 2.8)
    
    par(op)
}

### function that plots the predictive data distribution
plot.predictive <- function(samples, prior = FALSE, xlim = c(15, 135), ylim = c(0, 0.36), 
    lwd = 2, lwd.axis = 1, cex.axis = 1.6) {
    
    if (prior) {
        
        pred <- samples$BUGSoutput$sims.list$BobsIQScoresPriorPred
        
    } else {
        
        pred <- samples$BUGSoutput$sims.list$BobsIQScoresPostPred
        
    }
    
    mean.pred <- mean(pred)
    sd.pred <- sd(pred)
    
    ### plot settings
    op <- par(mar = c(5.1, 5.1, 5.1, 3.1))
    
    ### create empty canvas
    plot(1, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "")
    
    plot(function(x) dnorm(x, mean.pred, sd.pred), xlim = xlim, ylim = ylim, xlab = "", 
        ylab = "", lwd = lwd, add = TRUE)
    
    ### axes
    axis(1, at = seq(15, 135, 20), cex.axis = cex.axis, lwd = lwd.axis)
    axis(2, at = ylim, labels = FALSE, tck = 0, lwd = lwd.axis)
    
    ### axes labels
    mtext("Predicted IQ Scores", side = 1, cex = 1.4, line = 2.75)
    mtext("Density", side = 2, cex = 1.4, line = 0.8)
    
    par(op)
    
}

########################## 8-panel plot ##

### load samples

load(file = "samplesBob1.Rdata")
load(file = "samplesBob2.Rdata")
load(file = "samplesBob3.Rdata")

### plot

op <- par(mfrow = c(2, 4), xpd = NA)

heatmap.prior()
arrows(x0 = 75, x1 = 75, y0 = 2, y1 = -1, length = c(0.2, 0.2), lwd = 3, code = 2)
arrows(x0 = 95, x1 = 125, y0 = -1, y1 = 2, length = c(0.2, 0.2), lwd = 3, code = 2)

heatmap.posterior(samplesBob1)
arrows(x0 = 75, x1 = 75, y0 = 2, y1 = -1, length = c(0.2, 0.2), lwd = 3, code = 2)
arrows(x0 = 95, x1 = 125, y0 = -1, y1 = 2, length = c(0.2, 0.2), lwd = 3, code = 2)

heatmap.posterior(samplesBob2)
arrows(x0 = 75, x1 = 75, y0 = 2, y1 = -1, length = c(0.2, 0.2), lwd = 3, code = 2)
arrows(x0 = 95, x1 = 125, y0 = -1, y1 = 2, length = c(0.2, 0.2), lwd = 3, code = 2)

heatmap.posterior(samplesBob3)
arrows(x0 = 75, x1 = 75, y0 = 2, y1 = -1, length = c(0.2, 0.2), lwd = 3, code = 2)

plot.predictive(samplesBob1, prior = TRUE, ylim = c(0, 0.027))
points(73, 0.0005, col = "darkred", pch = 4, cex = 1.6, lwd = 3)

plot.predictive(samplesBob1, ylim = c(0, 0.034))
points(73, 0.0005, pch = 4, cex = 1.6, lwd = 3)
points(67, 0.0005, col = "darkred", pch = 4, cex = 1.6, lwd = 3)

plot.predictive(samplesBob2, ylim = c(0, 0.038))
points(73, 0.0005, pch = 4, cex = 1.6, lwd = 3)
points(67, 0.0005, pch = 4, cex = 1.6, lwd = 3)
points(79, 0.0005, col = "darkred", pch = 4, cex = 1.6, lwd = 3)

plot.predictive(samplesBob3, ylim = c(0, 0.041))
points(73, 0.0005, pch = 4, cex = 1.6, lwd = 3)
points(67, 0.0005, pch = 4, cex = 1.6, lwd = 3)
points(79, 0.0005, pch = 4, cex = 1.6, lwd = 3)

par(op)