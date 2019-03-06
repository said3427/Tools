NormBF10 <- function(dat, mu = 0, m = 1, priordat = NULL, plot = F, xwide = 3) {
	
    # dat ~ N(theta,1); theta ~ N(mu, 1/m); mu is prior mean, m is prior precision
    if (is.null(priordat)) {
        # no prior data
        priormean <- mu
        priorprec <- m
    }
    if (!is.null(priordat)) {
        # prior data
        n <- length(priordat)
        priormean <- (m * mu + n * mean(priordat))/(m + n)
        priorprec <- m + n
    }
    n <- length(dat)
    posteriormean <- (priorprec * priormean + n * mean(dat))/(priorprec + n)
    posteriorprec <- priorprec + n
    
    prior.height <- dnorm(0, mean = priormean, sd = priorprec^(-0.5))
    posterior.height <- dnorm(0, mean = posteriormean, sd = posteriorprec^(-0.5))
    BF10 <- prior.height/posterior.height
    if (plot == TRUE) {
        par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
            font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
        yhigh <- 1.5
        xlow <- -3
        xhigh <- 3
        plot(function(x) dnorm(x, mean = posteriormean, sd = posteriorprec^(-0.5)), 
            xlow, xhigh, ylim = c(0, yhigh), xlim = c(xlow, xhigh), lwd = 2, 
            lty = 1, ylab = "", xlab = "", axes = FALSE)
        lines(c(0, 0), c(0, 1.25), lwd = 2, col = "grey")
        par(new = TRUE)
        plot(function(x) dnorm(x, mean = priormean, sd = priorprec^(-0.5)), xlow, 
            xhigh, ylim = c(0, yhigh), xlim = c(xlow, xhigh), lwd = 2, lty = 2, 
            ylab = "", xlab = "", axes = FALSE)
        axis(1)
        axis(2)
        par(las = 0)
        mtext("Mu", side = 1, line = 2.5, cex = 1.5)
        mtext("Density", side = 2, line = 3, cex = 1.8)
        # Show Savage-Dickey density ratio:
        points(0, prior.height, cex = 2, pch = 21, bg = "grey")
        points(0, posterior.height, cex = 2, pch = 21, bg = "grey")
    }
    invisible(BF10)
}
dat <- c(0, 1, -1)
# dat <- c(-1,1,0)

#### simultaneous #### 1/NormBF10(dat, plot = TRUE) #2 text(-3, 1.4,
#### expression(BF[0][1](y[1],y[2],y[3]) == 2), cex = 1.5, pos = 4)

##### y1 #### 1/NormBF10(dat = dat[1], plot = TRUE) #sqrt(2) text(-3, 1.4,
##### expression(BF[0][1](y[1]) == sqrt(2)), cex = 1.5, pos = 4)

##### y2, given y1 #### 1/NormBF10(dat = dat[2], plot = TRUE, priordat = dat[1]) #1.04
##### composite.expression <- expression(paste(BF[0][1], '(', y[2], ' | ', y[1],
##### ')' %~~% 1.04)) text(-3, 1.4, composite.expression, cex = 1.5, pos = 4)

##### y3, given y1 and y2 ####
BF01 <- 1/NormBF10(dat = dat[3], plot = TRUE, priordat = dat[1:2])  #1.36
composite.expression <- expression(paste(BF[0][1], "(", y[3], " | ", y[1], ",", 
    y[2], ")" %~~% 1.36))
text(-3, 1.4, composite.expression, cex = 1.5, pos = 4)