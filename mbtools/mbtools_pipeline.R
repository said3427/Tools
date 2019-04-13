#Análisis Family&Friends AlchemyCode
#devtools::install_github("Gibbons-Lab/mbtools")
library("mbtools")

quals <- find_read_files("~/Downloads/AlchemyCode_FamilyFriends/") %>% quality_control()

config <- list(
  threads=4,
  preprocess = config_preprocess(
    trunLen = c(148, 140)  # forward and reverse truncation
  ),
  denoise = config_denoise(minOverlap = 3)  # will only use defaults
)

processed <- quals %>% preprocess(config$preprocess)

#Quality plots
processed$passed %>% quality_control()

denoised <- processed %>% denoise(config$denoise)


ps <- as_phyloseq(denoised)
asv_counts <- taxa_count(ps, lev = NA)