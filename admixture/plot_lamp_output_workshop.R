
# a simple script to plot locus-specific ancestry estimates, e.g. from LAMP


# define your input file between the quotes in the line below
lamp_output_file = "LatinAm_chromosome_22-LAMPout.txt"

# read in data
lamp_output = read.table(lamp_output_file)

# count number of snps in data
nsnps = dim(lamp_output)[2] - 1

# reformat lamp data as matrix, without the first column (that just identifies individuals)
lamp_matrix = as.matrix(lamp_output[,2:nsnps])

# define vector of colors you want to see
colors = c('darkgreen','lightblue','red')


############ pop 1 ###############

# create output file name.
png('locus_specific_ancestry_workshop_pop1.png', height=800,width=600)
# 25 individuals per picture, make margins tigher than default
par(mfrow=c(25,1))
par(mar=c(0.5,0.5,0.5,0.5))

# loop through 25 individuals worth of data, create visualization for each chromosome
for (i in 0:24) {
barplot(lamp_matrix[(3*i+1):(3*i+3),],col=colors, border=NA, space=0, xlab=NULL, xaxt='n', yaxt='n') 
# print individual number on the side of the chromosome
mtext((i+1), side=2, las=2, cex=1, line=-2)
# display individual being plotted.
print(i+1)
}

dev.off()


############ pop 2 ###############

# create output file name.
png('locus_specific_ancestry_workshop_pop2.png', height=800,width=600)
# 25 individuals per picture, make margins tigher than default
par(mfrow=c(25,1))
par(mar=c(0.5,0.5,0.5,0.5))

# loop through 25 individuals worth of data, create visualization for each chromosome
for (i in 25:49) {
  barplot(lamp_matrix[(3*i+1):(3*i+3),],col=colors, border=NA, space=0, xlab=NULL, xaxt='n', yaxt='n') 
  # print individual number on the side of the chromosome
  mtext((i+1), side=2, las=2, cex=1, line=-2)
  # display individual being plotted.
  print(i+1)
}

dev.off()


############ pop 3 ###############

# create output file name.
png('locus_specific_ancestry_workshop_pop3.png', height=800,width=600)
# 25 individuals per picture, make margins tigher than default
par(mfrow=c(25,1))
par(mar=c(0.5,0.5,0.5,0.5))

# loop through 25 individuals worth of data, create visualization for each chromosome
for (i in 50:74) {
  barplot(lamp_matrix[(3*i+1):(3*i+3),],col=colors, border=NA, space=0, xlab=NULL, xaxt='n', yaxt='n') 
  # print individual number on the side of the chromosome
  mtext((i+1), side=2, las=2, cex=1, line=-2)
  # display individual being plotted.
  print(i+1)
}

dev.off()


############ pop 4 ###############


# create output file name.
png('locus_specific_ancestry_workshop_pop4.png', height=800,width=600)
# 25 individuals per picture, make margins tigher than default
par(mfrow=c(25,1))
par(mar=c(0.5,0.5,0.5,0.5))

# loop through 25 individuals worth of data, create visualization for each chromosome
for (i in 75:99) {
  barplot(lamp_matrix[(3*i+1):(3*i+3),],col=colors, border=NA, space=0, xlab=NULL, xaxt='n', yaxt='n') 
  # print individual number on the side of the chromosome
  mtext((i+1), side=2, las=2, cex=1, line=-2)
  # display individual being plotted.
  print(i+1)
}

dev.off()

