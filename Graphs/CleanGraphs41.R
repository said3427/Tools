# rm (list = ls())

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

### Effect size plot
p <- ggplot(ALL[!is.na(ALL$HPD.median), ], aes(x = study.ES_order, y = HPD.median, ymin = HPD.lower, 
    ymax = HPD.upper)) + geom_pointrange() + theme_bw() + coord_flip() + geom_hline(yintercept = 0, 
    linetype = "dotted") + ylab("Posterior Effect Size (Unrestricted)") + xlab("")
# Some HPD intervals are missing (NA): These pertain to ANOVAS and Contingency
# tables, where there are multiple parameters per test.
print(p)