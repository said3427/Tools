library(dplyr)
library(ggplot2)
library(mclust)
library(gganimate)

load("~/Downloads/ExpMeth.RDATA")
colnames(mean.ME)[1]<-"ID"

DataExpr<-reshape2::melt(mean.ME[,c(1,8:13)])
DataMeth<-reshape2::melt(mean.ME[,c(1:7)])
DataMethExpr<-cbind(DataMeth,DataExpr[,3])
colnames(DataMethExpr)[2]<-"Condition"
colnames(DataMethExpr)[3]<-"Methylation"
colnames(DataMethExpr)[4]<-"Expression"
DataMethExpr$Condition<-gsub("mean.M.",replacement = "",DataMethExpr$Condition)

clusters<-Mclust(subset(DataMethExpr,Condition=="0105")[3:4])

plot(clusters,what = "density", type = "hdr")
plot(clusters,what = "density", type = "persp")

theme_set(theme_bw())

p<-ggplot(DataMethExpr,
          aes(
            x=Methylation,
            y=Expression,
            colour=rep(
              factor(array(clusters$classification))
                        ,6))) +
geom_point(show.legend = FALSE, alpha = 0.7)+ scale_color_viridis_d()

p+transition_states(Condition)+
shadow_wake(wake_length = 0.1, alpha = FALSE)+ labs(title = "Condition: {closest_state}")
