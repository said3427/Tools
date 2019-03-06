library("psych")
library("qgraph")

# Load BFI data:
data(bfi)
bfi <- bfi[, 1:25]

# Groups and names object (not needed really, but make the plots easier to
# interpret):
Names <- scan("http://sachaepskamp.com/files/BFIitems.txt", what = "character", sep = "\n")

# Create groups object:
Groups <- rep(c("A", "C", "E", "N", "O"), each = 5)

# Compute correlations:
cor_bfi <- cor_auto(bfi)

# Plot correlation network:
graph_cor <- qgraph(cor_bfi, layout = "spring", nodeNames = Names, groups = Groups, legend.cex = 0.6, 
    DoNotPlot = TRUE)

# Plot partial correlation network:
graph_pcor <- qgraph(cor_bfi, graph = "concentration", layout = "spring", nodeNames = Names, 
    groups = Groups, legend.cex = 0.6, DoNotPlot = TRUE)

# Plot glasso network:
graph_glas <- qgraph(cor_bfi, graph = "glasso", sampleSize = nrow(bfi), layout = "spring", 
    nodeNames = Names, legend.cex = 0.6, groups = Groups, legend.cex = 0.7, GLratio = 2, 
    DoNotPlot = TRUE)
# centrality plot (all graphs):
centralityPlot(list(r = graph_cor, `Partial r` = graph_pcor, glasso = graph_glas), 
    labels = Names) + labs(colour = "") + theme_bw() + theme(legend.position = "bottom")