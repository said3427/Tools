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

df_k155<-data.table::fread("~/Downloads/results_K155/result.tsv")
df_l06<-data.table::fread("~/Downloads/results_L06/result.tsv")
df_nf3<-data.table::fread("~/Downloads/results_NF3/result.tsv")
df_tfc3<-data.table::fread("~/Downloads/results_TFC3/result.tsv")

lista=list(K155=df_k155$name,L06=df_l06$name,NF3=df_nf3$name,TFC3=df_tfc3$name)

# Upset Plot
upset(fromList(lista), order.by = "freq")


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
