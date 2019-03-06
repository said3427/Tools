# rm(list = ls())

library(ggplot2)
clamp <- function(x, MIN, MAX) {
    if (x < MIN) 
        x <- MIN
    if (x > MAX) 
        x <- MAX
    x
}

# these contain the necessary labels
x <- read.csv("all3.csv", sep = ";", header = TRUE)
# these contain the numeric values
bf <- read.csv("all3_bfs.csv", sep = ";", header = TRUE)

ALL <- data.frame(study = as.character(x$Study1), HPD.median = bf$HPD.median, HPD.upper = bf$HPD.upper, 
    HPD.lower = bf$HPD.lower, logBF10 = bf$logBF10, logBF10_clamped = sapply(bf$logBF10, 
        function(lbf) {
            clamp(lbf, log(1/110), log(110))
        }), test = as.character(x$Testtype), number = as.integer(x[, 1]))

rm(x, bf, clamp)

# reorder factor levels based on another variable (HPD.mean)
ALL$study.ES_order <- reorder(ALL$study, ALL$HPD.median, mean)

### BF plot
forH1 <- TRUE
fontsize <- 3.2

# reorder factor levels based on BF
ALL$study.BF_order <- reorder(ALL$study, ALL$logBF10, mean)

# reorder factor levels as in data frame
ALL$study.df_order <- factor(ALL$study, levels = rev(ALL$study))

# Define shape: Show an arrow if the BF has been clamped
ALL$shape <- "point"
ALL$shape[ALL$logBF10 > log(110)] <- "rightarrow"
ALL$shape[ALL$logBF10 < -log(110)] <- "leftarrow"

# Define size: < and > must be larger to be visible
ALL$size <- "small"
ALL$size[ALL$logBF10 > log(110)] <- "large"
ALL$size[ALL$logBF10 < -log(110)] <- "large"

p2 <- ggplot(ALL, aes(x = study.BF_order, y = logBF10_clamped)) + geom_point(aes(shape = shape, 
    size = size)) + theme_bw() + geom_hline(yintercept = 0, linetype = "dotted") + ylab("Bayes Factor") + 
    xlab("") + scale_shape_manual(guide = FALSE, values = c(60, 19, 62)) + scale_size_manual(guide = FALSE, 
    breaks = c("small", "large"), values = c(5, 2))

# All the annotation stuff ...
hlines <- c(-log(c(100, 30, 10, 3)), log(c(3, 10, 30, 100)))
p2 <- p2 + geom_hline(yintercept = hlines, linetype = "dotted", color = "darkgrey")
p2 <- p2 + geom_hline(yintercept = log(1), linetype = "dashed", color = "grey20")

p2 <- p2 + annotate("text", x = Inf, y = -5.15, label = paste0("~~Extreme~H[", ifelse(forH1 == 
    TRUE, 0, 1), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = -4, label = paste0("~~Very~strong~H[", ifelse(forH1 == 
    TRUE, 0, 1), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = -2.85, label = paste0("~~Strong~H[", ifelse(forH1 == 
    TRUE, 0, 1), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = -1.7, label = paste0("~~Moderate~H[", ifelse(forH1 == 
    TRUE, 0, 1), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = -0.55, label = paste0("~~Anectodal~H[", ifelse(forH1 == 
    TRUE, 0, 1), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = 0.55, label = paste0("~~Anectodal~H[", ifelse(forH1 == 
    TRUE, 1, 0), "]"), hjust = 0, vjust = 0.5, vjust = 0.5, size = fontsize, color = "black", 
    parse = TRUE, angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = 5.15, label = paste0("~~Extreme~H[", ifelse(forH1 == 
    TRUE, 1, 0), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = 4, label = paste0("~~Very~strong~H[", ifelse(forH1 == 
    TRUE, 1, 0), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = 2.86, label = paste0("~~Strong~H[", ifelse(forH1 == 
    TRUE, 1, 0), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

p2 <- p2 + annotate("text", x = Inf, y = 1.7, label = paste0("~~Moderate~H[", ifelse(forH1 == 
    TRUE, 1, 0), "]"), hjust = 0, vjust = 0.5, size = fontsize, color = "black", parse = TRUE, 
    angle = 90)

# Add study type to right margin
p2 <- p2 + geom_text(aes(y = Inf, x = study.BF_order, label = test), size = 2, hjust = -0.1)

# set scale ticks
y_breaks <- c(c(-log(c(100, 30, 10, 3)), 0, log(c(3, 10, 30, 100))))
p2 <- p2 + scale_y_continuous(breaks = y_breaks, labels = c("1/100", "1/30", "1/10", 
    "1/3", "1", "3", "10", "30", "100"))

p2 <- p2 + coord_flip()

## Here comes the direty hack:
p2 <- p2 + theme(plot.margin = grid::unit(c(5, 5, 1, 1), "lines"))

# Code to override clipping, from
# http://stackoverflow.com/questions/10014187/displaying-text-below-the-plot-generated-by-ggplot2
gt <- ggplot_gtable(ggplot_build(p2))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

grid::grid.draw(gt)