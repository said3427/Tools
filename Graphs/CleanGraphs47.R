# SETUP local R-------------------------------------------------------------------
library(grid)

# Use this code (from the devtools package) to source C-3PR directly from GitHub:
library(devtools)
source_url('https://raw.githubusercontent.com/FredHasselman/toolboxR/master/C-3PR.R')

# This will load and (if necessary) install libraries frequently used for data management and plotting
in.IT(c('ggplot2','RColorBrewer','lattice','gridExtra','plyr','dplyr','httr'))


RPPdata<-read.csv('rpp_data.csv',stringsAsFactors=F )
RPPdata<-df.Clean(RPPdata)
RPPdata<-RPPdata$df

# Select the completed replication studies
RPPdata <- dplyr::filter(RPPdata, !is.na(T.pval.USE.O),!is.na(T.pval.USE.R))
# We have 99 studies for which p-values and effect sizes could be calculated
nrow(RPPdata)
# We have 97 studies for which p-values of the original effect were significantly below .05
idOK <- complete.cases(RPPdata$T.r.O,RPPdata$T.r.R)
# sum(idOK)

# Get ggplot2 themes predefined in C-3PR
mytheme <- gg.theme("clean")

# Get the Replication observed power
RPPdata$Power.Rn <- as.numeric(RPPdata$Power.R)

########################
# FIGURE 3
# EFFECT SIZE DENSITY PLOTS -------------------------------------------------------------
########################

# Setup some variables
RPPdata$oriSig <- "Not Significant"
# 3 studies claimed an effect at .05 < p < .06
RPPdata$oriSig[RPPdata$T.pval.USE.O<=.06] <- "Significant"
RPPdata$oriSig <- factor(RPPdata$oriSig)

RPPdata$repSig <- "Not Significant"
RPPdata$repSig[RPPdata$T.pval.USE.R<=.05] <- "Significant"
RPPdata$repSig <- factor(RPPdata$repSig)
RPPdata$repSig <- factor(RPPdata$repSig)

# Create a scatterplot with density margin plots

# The plotHolder() function from C-3PR creates a blank plot template that will hold the figures
blankPlot <- plotHolder()

# X margin density plot (note: gg.theme() from C-3PR can be used directly in a ggplot2() call)
xDense <- ggplot(RPPdata, aes(x=T.r.O, fill=oriSig)) + 
  geom_density(aes(y= ..count..),trim=F,alpha=.5) + 
  xlab("") + ylab("") + xlim(0,1) +
  gg.theme("noax") + 
  theme(legend.position = "none",plot.margin = unit(c(0,0,0,4), "lines"))

## Uncomment to save subplot
# ggsave("RPP_F3_xDense.png",plot=xDense)

# Y margin density plot (note: gg.theme() from C-3PR can be used directly in a ggplot2() call)
yDense <- ggplot(RPPdata, aes(x=T.r.R, fill=repSig)) + 
  geom_density(aes(y= ..count..),trim=F,alpha=.5) + 
  xlab("") + ylab("") + xlim(-.5,1) + 
  coord_flip() + 
  gg.theme("noax") + 
  theme(legend.position = "none", plot.margin = unit(c(0,0,3,0), "lines")) 

## Uncomment to save subplot
# ggsave("RPP_F3_yDense.png",plot=yDense)

# The main scatterplot (note: gg.theme() from C-3PR can be used directly in a ggplot2() call)
scatterP<-
  ggplot(RPPdata,aes(x=T.r.O,y=T.r.R)) +  
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_abline(intercept=0,slope=1,color="Grey60")+
  geom_point(aes(size=Power.Rn,fill=repSig),color="Grey30",shape=21,alpha=.8) + 
  geom_rug(aes(color=oriSig),size=1,sides="b",alpha=.6) + 
  geom_rug(aes(color=repSig),,size=1,sides="l",alpha=.6) + 
  scale_x_continuous(name="Original Effect Size",limits=c(0,1),breaks=c(0,.25,.5,.75,1)) + 
  scale_y_continuous(name="Replication Effect Size",limits=c(-.5,1),breaks=c(-.5,-.25,0,.25,.5,.75,1)) + 
  ggtitle("") + xlab("") + ylab("") + 
  scale_size_continuous(name="Replication Power",range=c(2,9)) + 
  scale_color_discrete(name="p-value") +
  scale_fill_discrete(name="p-value") +
  gg.theme("clean") + 
  theme(legend.position=c(.87,.185), legend.text=element_text(size=16), legend.title=element_text(size=18), plot.margin = unit(c(-2,-1.5,2,2), "lines"), axis.text.x=element_text(size=20),  axis.text.y=element_text(size=20), axis.title.x=element_text(size=25, vjust=-1.6), axis.title.y=element_text(size=26, vjust=2.6)) 

## Uncomment to save subplot
# ggsave("RPP_F3_scatter.png",plot=scatterP)

# Yet another way to organise plots: grid.arrange() from the gridExtra package.
grid.arrange(xDense, blankPlot, scatterP, yDense, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))