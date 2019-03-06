#### Create Data #### Data compiled by Allan J. Rossman (1994) from The World Almanac
#### and Book of Facts, 1993
#### (http://www.amstat.org/publications/jse/v2n2/datasets.rossman.html)
life.expectancy <- c(70.5, 53.5, 65, 76.5, 70, 71, 60.5, 51.5, 78, 76, 57.5, 61, 64.5, 
    78.5, 79, 61, 70, 70, 72, 64.5, 54.5, 56.5, 64.5, 64.5, 73, 72, 69, 64, 78.5, 53, 
    75, 68.5, 70, 70.5, 76, 75.5, 74.5, 65)
ppl.per.tv <- c(4, 315, 4, 1.7, 8, 5.6, 15, 503, 2.6, 2.6, 44, 24, 23, 3.8, 1.8, 96, 
    90, 4.9, 6.6, 21, 592, 73, 14, 8.8, 3.9, 6, 3.2, 11, 2.6, 23, 3.2, 11, 5, 3, 3, 1.3, 
    5.6, 29)
# Centre predictor so prediction interval estimates can be determined from the
# intercept and its SE
c.ppl.per.tv <- ppl.per.tv - mean(ppl.per.tv)

#### Run the Omnibus Linear Model ####
linear.model <- glm(life.expectancy ~ c.ppl.per.tv)

#### Create Plot Data (Predicted Values Using Omnibus Model) ####
x <- seq(from = 0, to = max(ppl.per.tv))
y <- array(NA, dim = length(x))
y.upper <- y
y.lower <- y
for (i in 1:length(x)) {
    raw.y <- coef(summary(update(linear.model, . ~ . - c.ppl.per.tv + eval(ppl.per.tv - 
        x[i]))))[1, 1]
    raw.se <- coef(summary(update(linear.model, . ~ . - c.ppl.per.tv + eval(ppl.per.tv - 
        x[i]))))[1, 2]
    y[i] <- raw.y
    y.upper[i] <- raw.y + raw.se
    y.lower[i] <- raw.y - raw.se
}

#### Create Plot ####
op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.5, bty = "n", las = 1)
plot(x, y, xlab = "", ylab = "", type = "n", xlim = c(0, max(x)), ylim = c(40, 80), axes = FALSE)
axis(1)
axis(2)
polygon(c(x, rev(x)), c(y.upper, rev(y.lower)), col = "lightsteelblue", border = NA)
lines(x, y, lwd = 2)
mtext("People Per TV", side = 1, line = 2.5, cex = 1.5)
mtext("Life Expectancy", side = 2, line = 3.7, cex = 1.5, las = 0)