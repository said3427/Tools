EPL<-c();for(i in dir()){H<-data.table::fread(i)$HomeTeam;A<-data.table::fread(i)$AwayTeam;Res<-data.table::fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL<-data.frame(EPL)
table(EPL$H)
table(EPL$A)

table(subset(EPL,H%in%c("Roma")&A%in%c("Lazio"))$Res)
progol<-data.table::fread("~/Downloads/Progol.csv")
table(progol$R1)
table(progol$R2)
table(progol$R3)
table(progol$R4)
table(progol$R5)
table(progol$R6)
table(progol$R7)
table(progol$R8)
table(progol$R9)
table(progol$R10)
table(progol$R12)
table(progol$R13)
table(progol$R14)
library(ggplot2)
colnames(progol)
apply(data[,3:16],2,table)
apply(data[,3:16],1,table)
apply(data[,3:16],2,table)
sapply(data[,3:16],2,table)
lapply(data[,3:16],2,table)
sapply(data[,3:16],table)
progol$BOLSA
apply(data[,3:16],1,paste0)
apply(progol[,3:16],1,paste0)
apply(progol[,3:16],1,paste0,collapse="")
table(apply(progol[,3:16],1,paste0,collapse=""))
max(table(apply(progol[,3:16],1,paste0,collapse="")))
table(apply(progol[,3:16],1,paste0,collapse=""))
apply(progol[,3:16],1,paste0,collapse="")
write.table(apply(progol[,3:16],1,paste0,collapse=""),file = "Progol.txt")
write.table(apply(progol[,3:16],1,paste0,collapse=""),file = "Progol.txt",quote=FALSE,row.names = FALSE)
revancha<-data.table::fread("~/Downloads/Progol-Revancha.csv")
write.table(apply(revancha[,3:16],1,paste0,collapse=""),file = "Revancha.txt",quote=FALSE,row.names = FALSE)
revancha
write.table(apply(revancha[,3:9],1,paste0,collapse=""),file = "Revancha.txt",quote=FALSE,row.names = FALSE)
table(apply(progol[,3:16],1,paste0,collapse=""))
max(table(apply(progol[,3:16],1,paste0,collapse="")))
max(table(apply(revancha[,3:9],1,paste0,collapse="")))
table(progol$R1)
table(progol$R2)
apply(progol[,3:16],1,table)
apply(progol[,3:16],2,table)
apply(progol[,3:16],2,table)/1257
ligaMx<-data.table::fread("~/Downloads/MEX.csv")
ligaMx
ligaMx$Home
table(ligaMx$Home)
subset(ligaMx,Home%in%c("Atlas","Tijuana"))
subset(ligaMx,Home%in%c("Atlas","Club Tijuana")&Away%in%c("Atlas","Club Tijuana"))
table(subset(ligaMx,Home%in%c("Atlas","Club Tijuana")&Away%in%c("Atlas","Club Tijuana"))$Res)
table(subset(ligaMx,Home%in%c("Atlas")&Away%in%c("Atlas","Club Tijuana"))$Res)
ligaMx$Home
table(ligaMx$Home)
table(subset(ligaMx,Home%in%c("Monarcas")&Away%in%c("Club Leon"))$Res)
table(subset(ligaMx,Home%in%c("Club Tijuana")&Away%in%c("Club America"))$Res)
table(subset(ligaMx,Home%in%c("Cruz Azul")&Away%in%c("Santos Laguna"))$Res)
table(subset(ligaMx,Home%in%c("Guadalajara Chivas")&Away%in%c("Toluca"))$Res)
table(subset(ligaMx,Home%in%c("U.N.A.M.- Pumas")&Away%in%c("Monterrey"))$Res)
table(subset(ligaMx,Home%in%c("Necaxa")&Away%in%c("Atl. San Luis"))$Res)
table(subset(ligaMx,Away%in%c("Atl. San Luis"))$Res)
(subset(ligaMx,Away%in%c("Atl. San Luis"))$Res)
(subset(ligaMx,Away%in%c("Atl. San Luis")))
table(subset(ligaMx,Home%in%c("Club Leon")&Away%in%c("Pachuca"))$Res)
table(subset(ligaMx,Home%in%c("Monarcas")&Away%in%c("Toluca"))$Res)
table(subset(ligaMx,Home%in%c("Club Tijuana")&Away%in%c("Santos Laguna"))$Res)
table(subset(ligaMx,Home%in%c("U.N.A.M.- Pumas")&Away%in%c("Pachuca"))$Res)
table(subset(ligaMx,Home%in%c("Atlas")&Away%in%c("Atlas","Club Tijuana"))$Res)
table(subset(ligaMx,Home%in%c("Atlas")&Away%in%c("Club Tijuana"))$Res)
table(subset(ligaMx,Home%in%c("Monarcas")&Away%in%c("Club Leon"))$Res)
table(subset(ligaMx,Home%in%c("Pachuca")&Away%in%c("U.A.N.L.- Tigres"))$Res)
table(subset(ligaMx,Home%in%c("Necaxa")&Away%in%c("Puebla"))$Res)
table(subset(ligaMx,Home%in%c("Toluca")&Away%in%c("Cruz Azul"))$Res)
table(subset(ligaMx,Home%in%c("Monterrey")&Away%in%c("Queretaro"))$Res)
table(subset(ligaMx,Home%in%c("Santos Laguna")&Away%in%c("U.N.A.M.- Pumas"))$Res)
revancha
revancha[,3:9]
apply(revancha[,3:9],2,table)
140+305+156
apply(revancha[,3:9],2,table)/601
apply(revancha[,3:9],1,table)
EPL<-data.table::fread("~/Downloads/EPL/EPL.csv")
EPL<-data.table::fread("~/Downloads/EPL/EPL.csv")
EPL<-data.table::fread("~/Downloads/EPL/EPL.csv")
EPL<-data.table::fread("~/Downloads/EPL/EPL.csv",fill=TRUE)
EPL
table(subset(ligaMx,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("West Ham"))$FTR)
table(subset(EPL,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("West Ham"))$FTR)
table(subset(EPL,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("Fulham"))$FTR)
table(subset(EPL,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("Fulham"))$FTR)
table(subset(EPL,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("Fulham"))
)
subset(EPL,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("Fulham"))
EPL$HomeTeam
table(EPL$HomeTeam)
colnames(EPL)
table(EPL$AwayTeam)
subset(EPL,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("Leicester"))
subset(EPL,HomeTeam%in%c("Liverpool")&AwayTeam%in%c("Leicester"))$FTR
setwd("~/Downloads/EPL/")
for(i in dir(pattern = "E0")){print(i)}
EPL<-c();for(i in dir(pattern = "E0")){H<-data.table.fread(i)$HomeTeam;A<-data.table.fread(i)$AwayTeam;Res<-data.table.fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL<-c();for(i in dir(pattern = "E0")){H<-data.table::fread(i)$HomeTeam;A<-data.table::fread(i)$AwayTeam;Res<-data.table::fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL
table(EPL$
H)
table(EPL$H)
EPL["H"]
EPL
colnames(EPL)
colnames(EPL)<-c("Home","Away","Res")
data.frame(EPL)
EPL<-data.frame(EPL)
EPL$Home
table(EPL$Home)
subset(EPL,Home%in%c("Leicester")&AwayTeam%in%c("Chelsea"))$FTR
subset(EPL,Home%in%c("Leicester")&Away%in%c("Chelsea"))$FTR
subset(EPL,Home%in%c("Leicester")&Away%in%c("Chelsea"))$Res
table(subset(EPL,Home%in%c("Leicester")&Away%in%c("Chelsea"))$Res)
setwd("../Liga/")
EPL<-c();for(i in dir(pattern = "SP")){H<-data.table::fread(i)$HomeTeam;A<-data.table::fread(i)$AwayTeam;Res<-data.table::fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL
EPL<-data.frame(EPL)
EPL$H
table(EPL$H)
table(EPL$A)
table(subset(EPL,Home%in%c("Valencia")&Away%in%c("Barcelona"))$Res)
table(subset(EPL,H%in%c("Valencia")&A%in%c("Barcelona"))$Res)
table(subset(EPL,H%in%c("Alaves")&A%in%c("Villareal"))$Res)
table(subset(EPL,H%in%c("Getafe")&A%in%c("Betis"))$Res)
table(subset(EPL,H%in%c("Espanol")&A%in%c("Ath Bilbao"))$Res)
setwd("../German/")
EPL<-c();for(i in dir()){H<-data.table::fread(i)$HomeTeam;A<-data.table::fread(i)$AwayTeam;Res<-data.table::fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL<-data.frame(EPL)
table(EPL$H)
table(EPL$A)
table(subset(EPL,H%in%c("Werder Bremen")&A%in%c("Hoffenheim"))$Res)
table(subset(EPL,H%in%c("Fortuna Dusseldorf")&A%in%c("Werder Bremen"))$Res)
setwd("../Liga/")
EPL<-c();for(i in dir()){H<-data.table::fread(i)$HomeTeam;A<-data.table::fread(i)$AwayTeam;Res<-data.table::fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL<-data.frame(EPL)
table(EPL$H)
table(EPL$A)
table(subset(EPL,H%in%c("Betis")&A%in%c("Sociedad"))$Res)
setwd("../EPL")
EPL<-c();for(i in dir()){H<-data.table::fread(i)$HomeTeam;A<-data.table::fread(i)$AwayTeam;Res<-data.table::fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL<-data.frame(EPL)
table(EPL$H)
table(subset(EPL,H%in%c("Southampton")&A%in%c("Wolves"))$Res)
table(subset(EPL,H%in%c("West Ham")&A%in%c("Everton"))$Res)
getwd("../SerieA/")
setwd("../SerieA/")
EPL<-c();for(i in dir()){H<-data.table::fread(i)$HomeTeam;A<-data.table::fread(i)$AwayTeam;Res<-data.table::fread(i)$FTR;EPL<-rbind(EPL,cbind(H,A,Res))}
EPL<-data.frame(EPL)
table(EPL$H)
table(EPL$A)
table(subset(EPL,H%in%c("Roma")&A%in%c("Lazio"))$Res)
history()
