library(polspline)
Gen.p.within = function(n.draws = 1000, n.data = 20, d = 0, s = 1) {
	
    # Generates p-values from a within-subject (paired) t-test
	
    p <- array(dim = n.draws)
    for (i in 1:n.draws) {
        # yes I know, vectorize is better
        dat <- rnorm(n.data, mean = d, sd = s)
        p[i] <- as.numeric(t.test(dat)$p.value)
    }
    return(p)
}

n.draws <- 20000
n.data <- 20
dfr <- n.data - 1
s <- 1

p.observed <- 0.045
t.observed <- qt(1 - (p.observed/2), dfr)

set.seed(1)
pvalues <- Gen.p.within(n.draws, n.data, d = s * t.observed/sqrt(n.data), s)

par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)

# Start with probit-transformed uniform distribution, that is, N(0,1):
y.high <- 1
x.high <- 4
x.low <- min(qnorm(pvalues))
curve(dnorm(x), from = -4, to = 4, add = FALSE, col = "black", lwd = 2, ylim = c(0, 
    y.high), xlim = c(x.low, x.high), ylab = "Density", xlab = " ", main = " ", 
    axes = FALSE)
axis(1)
axis(2)
mtext("Probit-transformed p Value", side = 1, line = 2.8, cex = 1.5)
greycolhist <- rgb(0, 0, 0, alpha = 0.7)
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 280, maxColorValue = 280)
# For transparent lines set, set 'alpha' between 0 (invisible) and 255
# (opaque)
lines(c(qnorm(p.observed), qnorm(p.observed)), c(0, y.high), lwd = 2, col = greycol)  #at observed p
points(qnorm(p.observed), dnorm(qnorm(p.observed)), pch = 21, cex = 2, bg = "grey")  # height under H0
height.H0 <- dnorm(qnorm(p.observed))

# Now for probit-transformed distribution of p-values under H1:
par(new = TRUE)
Nbreaks <- 20
small.y <- 0.05

y <- hist(qnorm(pvalues), Nbreaks, plot = FALSE)
plot(c(y$breaks, max(y$breaks)), c(0, y$density, 0), col = greycolhist, type = "S", 
    lwd = 2, lty = 1, ylim = c(0, y.high), xlim = c(x.low, x.high), xlab = " ", 
    ylab = "Density", main = " ")
pvalues.denspp <- logspline(qnorm(pvalues))
par(new = TRUE)
plot(pvalues.denspp, xlim = c(x.low, x.high), ylim = c(0, y.high), col = greycol, 
    lwd = 2)
height.H1 <- dlogspline(qnorm(p.observed), pvalues.denspp)  # height under H1
points(qnorm(p.observed), height.H1, pch = 21, cex = 2, bg = "grey")