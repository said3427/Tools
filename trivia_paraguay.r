library(ggplot2)
library(tidyr)
library(data.table)

antes<-c("Vori_Vori",
         "Defensores_del_Chaco",
         "Asunción",
         "Vori_Vori",
         "Pantanal",
         "El_presidente_tiene_zoo",
         "Sopa_paraguaya",
         "Asunción",
         "Triple_frontera",
         "Maíz",
         "Veronica",
         "Bandera_blanca_azul_rojo",
         "Veronica",
         "Iguazú",
         "Ciudad_del_Este",
         "Iguazú",
         "Maíz",
         "Veronica",
         "Paragua",
         "No_tiene_ninguna_costa",
         "Guaraní",
         "Vori_Vori",
         "José_Saturnino_Cardozo",
         "Iguazú",
         "Maíz",
         "Veronica",
         "Guaraní",
         "Asunción",
         "Futbol",
         "Guaraní",
         "Asunción",
         "Cumbia_Villera",
         "Bad_Bunny_va_en_Noviembre",
         "Guaraní",
         "Veronica")

despues<-c("Veronica",
           "Veronica",
           "Vori_Vori",
           "Moneda_más_antigua",
           "Energía_limpia",
           "Soy_mala_en_menti",
           "Tereré",
           "Energía_limpia",
           "Dos_caras_de_bandera",
           "Energía_limpia",
           "Tereré",
           "Dos_caras_de_bandera",
           "Veronica",
           "Iguazú",
           "Dos_caras_de_bandera",
           "Iguazú",
           "Asunción",
           "Saltos_del_Monday",
           "Paragua",
           "No_tiene_ninguna_costa",
           "Itaipú",
           "Sopa_paraguaya",
           "Tereré",
           "Energía_limpia",
           "Dos_caras_de_bandera",
           "Veronica",
           "Moneda_más_antigua",
           "Guaraní",
           "Tereré",
           "Asunción",
           "Guaraní",
           "Energía_limpia",
           "Saltos_del_Monday",
           "Dos_caras_de_bandera",
           "Veronica",
           "Guaraní",
           "Tereré")

antes<-data.frame(table(antes))
despues<-data.frame(table(despues))
matriz<-merge(antes,despues,by.x = c("antes"),by.y="despues",all = T)
colnames(matriz)<-c("Palabra","Antes","Después")

matriz[is.na(matriz)]<-0
matriz.ggplot<-data.table::melt(matriz)

ggplot(matriz,aes(x=Antes,y=Después))+geom_point()+geom_text_repel(aes(label=Palabra))+geom_hline(yintercept = 1.3)+geom_vline(xintercept = 1.3)+theme_minimal()

ggplot(matriz.ggplot,aes(x=value,y=..count..,color=variable,fill=variable))+geom_histogram()+facet_grid(variable~.)+geom_density(alpha=0.3)+ theme(legend.position = "bottom")

rownames(matriz)<-matriz$Palabra
matriz<-matriz[,-1]
matriz<-t(t(matriz)/colSums(matriz))
matriz<-matriz[order(matriz[,2],decreasing = T),]
corrplot(matriz,is.corr = F)