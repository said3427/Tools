library(ggmcmc)
library(gridExtra)
library(grid)

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

load("PostPredCheck.Rdata")

############################################## GRAPHS ###

postpred <- list()
for (i in 1:3) postpred[[i]] <- list()

for (i in 1:3) {
    for (j in 1:3) {
        postpred[[i]][[j]] <- ggplot(data[[i]][[j]], aes(x = ypred, y = yobs, 
            size = factor(nrow))) + geom_point() + coord_flip() + theme(panel.grid.minor = element_blank(), 
            plot.title = element_blank(), panel.grid.major = element_blank(), 
            legend.position = "none", axis.title.x = element_blank(), 
            axis.title.y = element_blank(), axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14), panel.background = element_rect(fill = "white", 
                colour = "white"), panel.border = element_blank(), axis.line = element_line(size = 1.1), 
            plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) + geom_abline(size = 1.1)
    }
}

############################################## PLOT ALL GRAPHS IN ONE FIGURE ###

T1 <- textGrob("1 Truth", gp = gpar(cex = 1.5))
T2 <- textGrob("2 Truths", gp = gpar(cex = 1.5))
T3 <- textGrob("3 Truths", gp = gpar(cex = 1.5))

xlab <- textGrob("Predicted Ranking", gp = gpar(cex = 2))
ylab <- textGrob("Observed Ranking", gp = gpar(cex = 2), rot = 90)

Tlab <- textGrob("Data Generated Using", gp = gpar(cex = 2), rot = 270)
Mlab <- textGrob("Model Assumption", gp = gpar(cex = 2))
blank <- textGrob("", gp = gpar(cex = 1.5))

grid.arrange(arrangeGrob(blank, blank, ylab, blank, nrow = 4, heights = c(0.2, 
    0.2, 3, 0.3)), arrangeGrob(arrangeGrob(blank, Mlab, blank, ncol = 3, 
    widths = c(0.3, 3, 0.1)), arrangeGrob(blank, T1, blank, T2, blank, 
    T3, ncol = 6, widths = c(0.15, 1, 0.15, 1, 0.15, 1)), arrangeGrob(postpred[[1]][[1]], 
    postpred[[1]][[2]], postpred[[1]][[3]], ncol = 3), arrangeGrob(postpred[[2]][[1]], 
    postpred[[2]][[2]], postpred[[2]][[3]], ncol = 3), arrangeGrob(postpred[[3]][[1]], 
    postpred[[3]][[2]], postpred[[3]][[3]], ncol = 3), arrangeGrob(blank, 
    xlab, blank, ncol = 3, widths = c(0.3, 3, 0.1)), nrow = 6, heights = c(0.2, 
    0.2, 1, 1, 1, 0.2)), arrangeGrob(blank, blank, T1, T2, T3, blank, 
    nrow = 6, heights = c(0.2, 0.2, 1, 1, 1, 0.3)), arrangeGrob(blank, 
    blank, Tlab, blank, nrow = 4, heights = c(0.2, 0.2, 3, 0.3)), ncol = 4, 
    widths = c(0.2, 2.8, 0.4, 0.2))