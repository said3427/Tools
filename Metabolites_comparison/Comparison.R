#install.packages("UpSetR")
library(UpSetR)

#### Functions
Intersect <- function (x) {
  # Multiple set version of intersect
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}
Union <- function (x) {
  # Multiple set version of union
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}
Setdiff <- function (x, y) {
  # Remove the union of the y's from the common x's.
  # x and y are lists of characters.
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}


### Importing Data

#Concat Positive and Negative identifications
#K155
K155_d5<-data.table::fread("~/Downloads/Negative/results_negative_K155_d5/result.tsv")$name
K155_d5<-unique(data.table::fread("~/Downloads/Positive/results_positive_K155_d5/result.tsv")$name)

K155_d9<-data.table::fread("~/Downloads/Negative/results_negative_K155_d9/result.tsv")$name
K155_d9<-unique(data.table::fread("~/Downloads/Positive/results_positive_K155_d9/result.tsv")$name)

#L06
L06_d5<-data.table::fread("~/Downloads/Negative/results_negative_L06_d5/result.tsv")$name
L06_d5<-unique(data.table::fread("~/Downloads/Positive/results_positive_L06_d5/result.tsv")$name)

L06_d9<-data.table::fread("~/Downloads/Negative/results_negative_L06_d9/result.tsv")$name
L06_d9<-unique(data.table::fread("~/Downloads/Positive/results_positive_L06_d9/result.tsv")$name)

#NF3
NF3_d5<-data.table::fread("~/Downloads/Negative/results_negative_NF3_d5/result.tsv")$name
NF3_d5<-unique(data.table::fread("~/Downloads/Positive/results_positive_NF3_d5/result.tsv")$name)

NF3_d9<-data.table::fread("~/Downloads/Negative/results_negative_NF3_d9/result.tsv")$name
NF3_d9<-unique(data.table::fread("~/Downloads/Positive/results_positive_NF3_d9/result.tsv")$name)

#TFC3
TFC3_d5<-data.table::fread("~/Downloads/Negative/results_negative_TFC3_d5/result.tsv")$name
TFC3_d5<-unique(data.table::fread("~/Downloads/Positive/results_positive_TFC3_d5/result.tsv")$name)

TFC3_d9<-data.table::fread("~/Downloads/Negative/results_negative_TFC3_d9/result.tsv")$name
TFC3_d9<-unique(data.table::fread("~/Downloads/Positive/results_positive_TFC3_d9/result.tsv")$name)

# df_k155<-data.table::fread("~/Downloads/results_K155/result.tsv")
# df_l06<-data.table::fread("~/Downloads/results_L06/result.tsv")
# df_nf3<-data.table::fread("~/Downloads/results_NF3/result.tsv")
# df_tfc3<-data.table::fread("~/Downloads/results_TFC3/result.tsv")

lista=list(K155_D5=K155_d5,K155_D9=K155_d9,L06_D5=L06_d5,L06_D9=L06_d9,NF3_D5=NF3_d5,NF3_D9=NF3_d9,TFC3_D5=TFC3_d5,TFC3_D9=TFC3_d9)

# Upset Plot
upset(fromList(lista),keep.order = T)


#Extracting the elements in comparisons
xx.1<-lista

combs <- unlist(lapply(1:length(xx.1),
function(j) combn(names(xx.1), j, simplify = FALSE)),
recursive = FALSE)
names(combs) <- sapply(combs, function(i) paste0(i, collapse = ""))
str(combs)

elements <- lapply(combs, function(i) Setdiff(xx.1[i], xx.1[setdiff(names(xx.1), i)]))
n.elements <- sapply(elements, length)
print(n.elements)
elements

#Exporting lists of elements in comparison

write.csv(file = "~/Desktop/K155.csv",subset(df_k155,name%in%elements$K155),quote=FALSE)
write.csv(file = "~/Desktop/L06.csv",subset(df_l06,name%in%elements$L06),quote=FALSE)
write.csv(file = "~/Desktop/NF3.csv",subset(df_nf3,name%in%elements$NF3),quote=FALSE)
write.csv(file = "~/Desktop/TFC3.csv",subset(df_tfc3,name%in%elements$TFC3),quote=FALSE)
write.csv(file = "~/Desktop/K155-L06.csv",subset(df_k155,name%in%elements$K155L06),quote=FALSE)
write.csv(file = "~/Desktop/K155-NF3.csv",subset(df_k155,name%in%elements$K155NF3),quote=FALSE)
write.csv(file = "~/Desktop/K155-TFC3.csv",subset(df_k155,name%in%elements$K155TFC3),quote=FALSE)
write.csv(file = "~/Desktop/L06-NF3.csv",subset(df_l06,name%in%elements$L06NF3),quote=FALSE)
write.csv(file = "~/Desktop/L06-TFC3.csv",subset(df_l06,name%in%elements$L06TFC3),quote=FALSE)
write.csv(file = "~/Desktop/NF3-TFC3.csv",subset(df_nf3,name%in%elements$NF3TFC3),quote=FALSE)
write.csv(file = "~/Desktop/K155-L06-NF3-TFC3.csv",subset(df_nf3,name%in%elements$K155L06NF3TFC3),quote=FALSE)
