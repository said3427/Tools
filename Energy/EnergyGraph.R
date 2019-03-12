#Graph for energy production

library("ggplots")
library("ggplot2")
library(reshape2)
library(gridExtra)

data<-data.table::fread("~/Desktop/consulta-sin-restricciones.csv")

table(data$localization)

#ACA  CVC  MEX  MTY  PUE  TOL  VDB 
#58  433 9214 1124 1080  369  109 

data$mes <- factor(data$mes, levels=c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE"))
data$behaviour<-data$generated_energy/data$expected_energy

#Separar por localización
#j=0;localization=c();for(i in data$nupe){j=j+1;if(i!=""){data$localization[j]=tail(unlist(strsplit(i,"[\\_\\-]+")),n=1)}}

dat<-melt(data,id.vars=c("mes","year","plant_id","nupe","localization"),measure.vars=c("expected_energy", "generated_energy","behaviour"))
dat.behaviour<-melt(data,id.vars=c("mes","year","plant_id","nupe","localization"),measure.vars=c("behaviour"))

# p<-ggplot(dat, aes(x=mes, y=value, color=variable)) + 
#   geom_boxplot() +
#   geom_jitter()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
#   facet_grid(. ~ year)

#Gráficas por cliente

for (cliente in unique(dat$plant_id)){
  ### process data for plotting here ####
  nupe<-dat[dat$plant_id==cliente,]$nupe[1]
  p<-ggplot(dat[dat$plant_id==cliente,], aes(x=mes, y=value, group=variable,color=variable)) + 
    geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
    geom_line()+
    facet_grid(. ~ year)+ ggtitle(nupe) + ylab("Valor (kWh)")
  
#  p<-ggplot(dat[dat$plant_id==cliente,], aes(x=mes, y=value, color=variable)) + 
#    geom_jitter()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
#    geom_line()+
#    facet_grid(. ~ year)+ ggtitle(nupe) 
  ggsave(p,filename=paste(cliente,".jpeg",sep="")) 
}

#Gráfica de cajas y bigotes (boxplot del comportamiento)
p<-ggplot(subset(dat.behaviour,value<2), aes(x=mes, y=value)) + 
  geom_boxplot()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("generated energy/ expected energy")+
  facet_grid(localization ~ year)+ ggtitle("Comportamiento de plantas (2017-2019)") +geom_hline(yintercept=1,color='coral') 

p

#Gráfica de violín
p.violin<-ggplot(subset(dat.behaviour,value<2), aes(x=mes, y=value)) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("generated energy/ expected energy")+
   facet_grid(. ~ year)+ ggtitle("Comportamiento de plantas (2017-2019)") +geom_hline(yintercept=1,color='coral')+geom_violin()

p.violin

means <- aggregate(value ~  mes, dat.behaviour, mean)

p+stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE)
