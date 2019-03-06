### All Priming likelihood surface with posterior of uniform mu prior on top

# MCMC samples
load("samplesAllPriming_uniformMuPrior_SigmaTruncatedOnlyAtOne.Rdata")
samples <- samplesAllPriming_uniformMuPrior_SigmaTruncatedOnlyAtOne
posteriorMu <- samples$BUGSoutput$sims.list$muH1
posteriorPhi <- samples$BUGSoutput$sims.list$phi

library(ks)
d1 <- kde(cbind(posteriorMu, posteriorPhi))

# logLikelihood3d
load("logLikelihood3d_allPriming.Rdata")

ll <- logLikelihood3d_allPriming

likelihood <- exp(ll + 100)

sumLikelihood <- sum(likelihood)
weights <- likelihood/sumLikelihood
likelihoodTimesWeights <- likelihood * weights
averagedLikelihood <- apply(likelihoodTimesWeights, c(1, 2), sum)

mu <- seq(-6, 0, length.out = 200)
phi <- seq(0, 1, length.out = 200)

op <- par(mar = c(5, 5, 5, 5))

image(x = mu, y = phi, z = averagedLikelihood, col = topo.colors(12), xlab = "", ylab = "", 
    las = 1, cex.axis = 1.4)

mtext(expression(mu), 1, cex = 2.2, line = 2.55)
mtext(expression(phi), 2, cex = 2.35, line = 3.3, las = 1)

contour(x = d1$eval.points[[1]], y = d1$eval.points[[2]], z = d1$estimate, add = TRUE, 
    lty = 1)

mtext(expression("Priming Studies (N=268)"), side = 3, cex = 2, line = 0.7)
par(op)