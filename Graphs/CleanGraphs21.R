gendat = function(ndat = 1000, dt = 0.1) {
    # Outputs a sequence of Brownian motion data
    dat <- array()
    dat[1] <- rnorm(1, mean = 0, sd = sqrt(dt))
    
    for (j in 1:(ndat - 1)) {
        drift <- 0
        diffvar <- 1
        
        error <- rnorm(1, 0, sqrt(diffvar * dt))
        dat[j + 1] <- dat[j] + drift * dt + error  # Cobb & Zacks (1985), Eq. 1.14
    }
    
    invisible(dat)  # same as 'return', but without printing to console    
}

## General settings:
dt <- 0.1
ntime <- 1000
times <- c(1:ntime)
nsims <- 1000

# Plot settings:
ylow <- -40
yhigh <- 40
xhigh <- 1.3 * ntime
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 170, maxColorValue = 255)
# For transparent lines set, set 'alpha' between 0 (invisible) and 255
# (opaque)

op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
plot(times, gendat(ndat = ntime, dt), type = "l", lwd = 1, main = "", ylab = "", 
    xlab = "", axes = FALSE, ylim = c(ylow, yhigh), xlim = c(0, xhigh), cex.lab = 1, 
    font.lab = 2, cex.axis = 0.9, col = greycol, bty = "n")
axis(1, at = c(0, 200, 400, 600, 800, 1000), lab = c("0", "20", "40", "60", "80", 
    "100"))
# NB the labels are divided by 10 because dt = .1
axis(2)
par(las = 0)
mtext("Time", side = 1, line = 2.5, cex = 1.5, at = 500)
mtext("Evidence", side = 2, line = 2.8, cex = 1.5)
lines(c(1, ntime), c(0, 0), lwd = 1, lty = 2, col = "black")

for (i in 1:9) {
    par(new = TRUE)
    plot(times, gendat(ndat = ntime, dt), type = "l", lwd = 1, main = "", ylab = "", 
        xlab = "", axes = FALSE, ylim = c(ylow, yhigh), xlim = c(0, xhigh), cex.lab = 1, 
        font.lab = 2, cex.axis = 0.9, col = greycol, bty = "n")
}

std <- sqrt(ntime * dt)
c <- 7000  # A multiplication factor so the Normal density is visible
df1 <- data.frame(yval = seq(from = -35, to = 35, by = 0.1), xval = (dnorm(seq(from = -35, 
    to = 35, by = 0.1), 0, std) * c))
with(df1, lines(xval + ntime + 20, yval, lwd = 2))

# Optional: Check by simulation and plot density estimate
do.sim <- FALSE
if (do.sim == TRUE) {
    x <- array()
    for (i in 1:nsims) {
        x[i] <- gendat(ntime, dt)[ntime]  # end point of simulation process
    }
    startx <- ntime + 20
    yhigh.c <- yhigh * c
    y <- density(x)
    lines((y$y * c) + startx, y$x, lwd = 2, col = greycol)
}

par(op)