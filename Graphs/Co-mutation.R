# how to make a Co-mutation (comut) plot with ggplot2
library("ggplot2")
library('biomaRt')

# first - lets get random gene names from biomaRt
listMarts() # will show all available databases

mart <- useMart(biomart = "ensembl")
listDatasets(mart) # will list all available datasets 
mart <- useMart(biomart = 'ensembl', dataset = "mmusculus_gene_ensembl")
head(listAttributes(mart), n = 50)

# Extract information from biomart for uniprot gene names
results <- getBM(attributes = c("hgnc_symbol"), mart = mart)

#random sample of 40 genes
RandomGenes <- sample(results$uniprot_genename, 40)
#RandomGenes

# human / mouse orthologs in biomart
human <- useMart("ensembl", dataset = 'hsapiens_gene_ensembl')

attributes <- c("ensembl_gene_id", "mmusculus_homolog_ensembl_gene", "mmusculus_homolog_perc_id_r1")
attributes <- c(attributes, "mmusculus_homolog_orthology_type", "mmusculus_homolog_subtype", "mmusculus_homolog_perc_id")

ort.mouse <- getBM(attributes, filters = "with_homolog_mmus", values = TRUE, mart = human, bmHeader = FALSE)

# Now we will make an example data frame for a Co-mutation plot 
# with 40 genes of interest in 100 subjects
df <- expand.grid(gene = 1:40, subj = 1:100)
df$Mutation <- as.factor(sample(c(rep("Missense",1500),
                                  rep("Nonsense",500),
                                  rep("Frame Shift",1000),
                                  rep("Indel", 250),
                                  rep("Splice Site", 749), 4000)))
df$Mutation[sample(4000,3700)] <- NA

# now for a Comut plot with ggplot2 
(mut <- ggplot(df, aes(x = subj, y = gene, height = 0.8, width = 0.8)) + geom_tile(aes(fill = Mutation)) + 
  scale_fill_brewer(palette = "Set1", na.value = "Grey90") +
  scale_x_discrete(rev(ord)) + 
  xlab("Subject") +
  ggtitle("Example Comut plot") + 
   theme( 
     legend.key = element_rect(fill='NA'),
     legend.key.size = unit(0.4, 'cm'),
     legend.title = element_blank(),
     legend.position = "bottom",
     legend.text = element_text(size=8, face="bold"),
     axis.ticks.x = element_blank(), 
     axis.ticks.y = element_blank(),
     axis.text.x = element_blank(),
     axis.text.y = element_text(colour = "Black"), 
     axis.title.x = element_text(face = "bold"), 
     axis.title.y = element_blank(),
     panel.grid.major.x = element_blank(), 
     panel.grid.major.y = element_blank(),
     panel.grid.minor.x = element_blank(), 
     panel.grid.minor.y = element_blank(), 
     panel.background = element_blank()
   ))

ggsave(mut,file="2015-5-24-ExampleComutplot.png", width = 10, height = 8)

# lets place a bargraph to the right of the plot of FDR values
FDR <- runif(40,0,0.1)
head(FDR)
FDR <- FDR[order(FDR)]
df_FDR <- data.frame(RandomGenes, FDR)
df_FDR

(p <- ggplot(data = df_FDR, aes(x = RandomGenes, y = FDR)) +
  geom_bar(colour = "black", stat = "identity") +
  scale_x_discrete(rev(df_FDR$FDR)) +
  coord_flip() +
  ggtitle("FDR") + 
  theme( 
    legend.key = element_rect(fill = 'NA'),
    legend.key.size = unit(0.4, 'cm'),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8, face = "bold"),
    axis.ticks.x = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "Black"), 
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.background = element_blank()
  ))


library(gridExtra)
grid.arrange(mut, p, ncol = 2, widths = 2:0.5)