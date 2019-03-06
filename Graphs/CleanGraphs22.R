# rm(list = ls())

IndividualPerformance <- function(choice, lo, show.losses = FALSE) {
    # Plots the choice profile Args: choice: A vector containing the choices on
    # each trial lo: A vector containing the losses on each trial show.losses:
    # logical: Should the losses be indicated by filled dots?
    
    par(mar = c(4, 4.5, 0.5, 1))
    plot(choice, type = "b", axes = FALSE, xlab = "Trial", ylab = "Deck", cex.lab = 2)
    axis(1, seq(0, 100, length = 6), cex.axis = 1.8)
    axis(2, 1:4, labels = c("A", "B", "C", "D"), cex.axis = 1.8, las = 1)
    if (show.losses == TRUE) {
        index.losses <- which(lo < 0)
        points(matrix(c(index.losses, choice[index.losses]), byrow = FALSE, nrow = length(index.losses)), 
            pch = 19, lwd = 1.5)
    }
}

# Synthetic data
choice <- sample(1:4, 100, replace = TRUE)
lo <- sample(c(-1250, -250, -50, 0), 100, replace = TRUE)

# postscript('DiversePerformance.eps', width = 7, height = 7)
IndividualPerformance(choice, lo, show.losses = TRUE)
# dev.off()