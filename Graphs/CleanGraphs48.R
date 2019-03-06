######################################################################################
#                          RPP Manuscript Figure 1 (ViolinQtilePlots)                #
#                                                                                    #
# Created by [Fred Hasselman](https://osf.io/ujgs6/)  on behalf of the RPP intiative #
######################################################################################

# FUNCTIONS ---------------------------------------------------------------

## GET DATA

 df.Clean <- function(df,Sep="."){
  nms   <- colnames(df)
  rws   <- rownames(df)

  # Change punctuation and blankss in variable names to points
  nmsP  <- gsub("([[:punct:]]|[[:blank:]])+","+",nms)
  nmsPP <- gsub("(^[+]|[+]$)+","",nmsP)
  nmsPP <- gsub("[+]",Sep,nmsPP)
  # Check for double names
  ifelse(length(unique(nmsPP))==length(nmsPP),{nms <- nmsPP},{
    id2 <- which(plyr::laply(nmsPP,function(n) sum(nmsPP%in%n))>1)
    nms <- nmsPP
    nms[id2] <- paste(nmsPP[id2],id2,sep=".")})

  colnames(df) <- nms
  df      <- dplyr::select(df,which(nms%in%nms[nms!=""]))
  df[ ,1] <- paste0("Row.",seq(1,nrow(df)))
  colnames(df)[1] <- paste("Local","ID",sep=Sep)
  return(list(df=df,
              nms=nms,
              rws=rws))
}

get.OSFfile <- function(# Function to download OSF file modified from code by Sacha Epskamp
  code,  #Either "https://osf.io/XXXXX/" or just the code
  dir = tempdir(), # Output location
  scanMethod, #  "readLines" or "RCurl". Leave missing to automatically chose
  downloadMethod = c("httr","downloader","curl"), # First one is chosen
  dataFrame = TRUE,
  sep = ',',
  dfCln = FALSE
){
  # Check if input is code:
  if (!grepl("osf\\.io",code)){
    URL <- sprintf("https://osf.io/%s/",code)
  } else URL <- code

  # Scan page:
  if (grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE)){
    try(setInternet2(TRUE))
  }

  if (missing(scanMethod)){
    scanMethod <- ifelse(grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE), "readLines", "RCurl")
  }
  if (scanMethod == "readLines"){
    Page <- paste(readLines(URL),collapse="\n")
  } else if (scanMethod == "RCurl"){
    library("RCurl")
    Page <- RCurl::getURL(URL)
  } else if (scanMethod == "httr"){
    Page <- httr::GET(URL)
    Page <- paste(Page,collapse="\n")
  } else stop("Invalid scanMethod")


  # Create download link:
  URL <- gsub("/$","",URL)
  #   Link <- paste0(URL,"/?action=download&version=1")
  Link <- paste0(URL,"/?action=download")

  # Extract file name:
  FileName <- regmatches(Page,gregexpr("(?<=\\OSF \\| ).*?(?=\\)", Page, perl=TRUE))[[1]]
  FullPath <- paste0(dir,"/",FileName)

  info <- NULL
  # Download file:
  if (downloadMethod[[1]]=="httr"){
    library("httr")
    info <- httr::GET(Link, httr::write_disk(FullPath, overwrite = TRUE))
  } else if (downloadMethod[[1]]=="downloader"){
    library("downloader")
    downloader:::download(Link, destfile = FullPath, quiet=TRUE)
  } else if (downloadMethod[[1]]=="curl"){
    system(sprintf("curl -J -L %s > %s", Link, FullPath), ignore.stderr = TRUE)
  }  else stop("invalid downloadMethod")

  df <- NULL
  if(dataFrame==TRUE){
    if(grepl('xls',FileName)){
      df <- tbl_df(read.xlsx2(file=FullPath,sheetIndex=1))
    } else {
      df <- tbl_df(read.table(FullPath,stringsAsFactors=F,fill = T,header=T,sep=sep, comment.char = "",quote = "\""))
    }
    if(dfCln==TRUE){df <- df.Clean(df)} else {df$df <- df}

    return(list(df   = df$df,
                info = list(FilePath=FullPath,
                            Info=info,
                            ori.Colnames=tbl_df(data.frame(ori.colnames=df$nms)),
                            ori.Rownames=tbl_df(data.frame(ori.rownames=df$rws))
                )))
  } else {
    # Return location of file:
    return(FilePath=FullPath)
  }
}


## PLOTS

gg.theme <- function(type=c("clean","noax")[1],useArial = F, afmPATH="~/Dropbox"){
  require(ggplot2)
  if(useArial){
    set.Arial(afmPATH)
    bf_font="Arial"
  } else {bf_font="Helvetica"}

  switch(type,
         clean = theme_bw(base_size = 16, base_family=bf_font) +
           theme(axis.text.x     = element_text(size = 14),
                 axis.title.y    = element_text(vjust = +1.5),
                 panel.grid.major  = element_blank(),
                 panel.grid.minor  = element_blank(),
                 legend.background = element_blank(),
                 legend.key = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.line  = element_line(colour = "black")),

         noax = theme(line = element_blank(),
                      text  = element_blank(),
                      title = element_blank(),
                      plot.background = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
  )
}

set.Arial <- function(afmPATH="~/Dropbox"){
  # Set up PDF device on MAC OSX to use Arial as a font in Graphs
  if(nchar(afmPATH>0)){
    if(file.exists(paste0(afmPATH,"/Arial.afm"))){
      Arial <- Type1Font("Arial",
                         c(paste(afmPATH,"/Arial.afm",sep=""),
                           paste(afmPATH,"/Arial Bold.afm",sep=""),
                           paste(afmPATH,"/Arial Italic.afm",sep=""),
                           paste(afmPATH,"/Arial Bold Italic.afm",sep="")))
      if(!"Arial" %in% names(pdfFonts())){pdfFonts(Arial=Arial)}
      if(!"Arial" %in% names(postscriptFonts())){postscriptFonts(Arial=Arial)}
      return()
    } else {disp(header='useArial=TRUE',message='The directory did not contain the *.afm version of the Arial font family')}
  } else {disp(header='useArial=TRUE',message='Please provide the path to the *.afm version of the Arial font family')}
}

#function to create geom_ploygon calls
fill_viol<-function(gr.df,gr,qtile,probs){
  # SETUP VIOLIN QUANTILE PLOTS -----------------------------------
  # This is adapted from: http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot

  ifelse(is.null(qtile),{
    cuts <- cut(gr.df$y, breaks = quantile(gr.df$y, probs, na.rm=T, type=3, include.lowest = T, right = T), na.rm=T)},{
      cuts <- cut(gr.df$y, breaks = qtile, na.rm=T)
    }
  )
  quants <- mutate(gr.df,
                   x.l=x-violinwidth/2,
                   x.r=x+violinwidth/2,
                   cuts=cuts)

  plotquants <- data.frame(x=c(quants$x.l,rev(quants$x.r)),
                           y=c(quants$y,rev(quants$y)),
                           id=c(quants$cuts,rev(quants$cuts)))

  #cut by quantile to create polygon id
  geom <- geom_polygon(aes(x=x,y=y,fill=factor(id)),data=plotquants,alpha=1)

  return(list(quants=quants,plotquants=plotquants,geom=geom))
}

vioQtile <- function(gg=NULL,qtiles=NULL,probs=seq(0,1,.25),labels=paste(probs[-1]*100),withData=FALSE){
  require(ggplot2)
  # SETUP VIOLIN QUANTILE PLOTS -----------------------------------
  # This is adapted from: http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot
  #
  # Changed:
  # - Deal with 'empty' quantile groups
  # - Deal with original data
  # - More input, more output
  g.df <- ggplot_build(gg)$data[[1]]    # use ggbuild to get the outline co-ords

  ifelse(is.null(qtiles),{
    gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,NULL,probs)$geom)},{
    gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,qtiles[x, ],probs)$geom)}
  )

  gg <- gg + geom_hline(aes(yintercept=0)) +
    scale_fill_grey(name="Quantile\n",labels=labels,guide=guide_legend(reverse=T,label.position="right")) +
    stat_summary(fun.y=median, geom="point", size=8, color="grey80",shape=21,fill="white")

  if(withData){
    ifelse(is.null(qtiles),{
      ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,NULL,probs))},{
        ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,qtiles[x,],probs))
      }
    )
    return(list(ggGraph=gg,ggData=ggData))
  } else {
    return(gg)
  }
}

# MULTIPLOT FUNCTION ------------------------------------------------------------------------------------------------------------------
#
# [copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ]
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multi.PLOT <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# MAIN CODE ---------------------------------------------------------------

# SETUP local R-------------------------------------------------------------------
require('ggplot2')
require('RColorBrewer')
require('lattice')
require('gridExtra')
require('plyr')
require('dplyr')
require('httr')
require('RCurl')

# Read the data from the OSF storage
# Note: get.OSFfile() returns a list with the .csv data (df) and information (info) containing the URL download timestamp and original column and rownames (these names will be changed if dfCln=TRUE).
RPPdata <- get.OSFfile(code='https://osf.io/fgjvw/',dfCln=T)$df

## If you dowloaded the csv file to your harddrive use this code:
#  RPPdata<-read.csv('rpp_data.csv',stringsAsFactors=F )
#  RPPdata<-df.Clean(RPPdata)
#  RPPdata<-RPPdata$df

# Select the completed replication studies
RPPdata <- dplyr::filter(RPPdata, !is.na(T.pval.USE.O),!is.na(T.pval.USE.R))
# We have 99 studies for which p-values and effect sizes could be calculated
nrow(RPPdata)
# We have 97 studies for which p-values of the original effect were significantly below .05
idOK <- complete.cases(RPPdata$T.r.O,RPPdata$T.r.R)
sum(idOK)

# Get ggplot2 themes predefined in C-3PR
mytheme <- gg.theme("clean")

############
# FIGURE 1
# VIOLIN QUANTILE PLOTS (VQP) -----------------------------------------------
############

# Restructure the data to "long" format: Study type will be a factor
df <- dplyr::select(RPPdata,starts_with("T."))
df <- data.frame(EffectSize=as.numeric(c(df$T.r.O,df$T.r.R)),p.value=as.numeric(c(df$T.pval.USE.O,df$T.pval.USE.R)),grp=factor(c(rep("Original Studies",times=length(df$T.r.O)),rep("Replications",times=length(df$T.r.R)))))

# Create some variables for plotting
df$grpN <- as.numeric(df$grp)
probs   <- seq(0,1,.25)

# VQP PANEL A: p-value -------------------------------------------------

# Get p-value quantiles and frequencies from data
qtiles <- ldply(unique(df$grpN),function(gr) quantile(round(df$p.value[df$grpN==gr],digits=4),probs,na.rm=T,type=3))
freqs  <- ldply(unique(df$grpN),function(gr) table(cut(df$p.value[df$grpN==gr],breaks=qtiles[gr,],na.rm=T,include.lowest=T,right=T)))
labels <- sapply(unique(df$grpN),function(gr)levels(cut(round(df$p.value[df$grpN==gr],digits=4), breaks = qtiles[gr,],na.rm=T,include.lowest=T,right=T)))

# Get regular violinplot using package ggplot2
g.pv <- ggplot(df,aes(x=grp,y=p.value)) + geom_violin(aes(group=grp),scale="width",color="grey30",fill="grey30",trim=T,adjust=.7)
# Cut at quantiles using vioQtile() in C-3PR
g.pv0 <- vioQtile(g.pv,qtiles,probs)
# Garnish
g.pv1 <- g.pv0 + geom_hline(aes(yintercept=.05),linetype=2) +
  ggtitle("A") + xlab("") + ylab("p-value") +
  mytheme + theme(legend.position=c(.5,.5), legend.text=element_text(size=20), legend.title=element_text(size=22), axis.text.x=element_text(size=26),  axis.text.y=element_text(size=22), axis.title.y=element_text(size=30, vjust=2.6), title=element_text(size=27)) 
# View
g.pv1

## Uncomment to save panel A as a seperate file
# ggsave("RPP_F1_VQPpv.eps",plot=g.pv1)

# VQP PANEL B: effect size -------------------------------------------------

# Get effect size quantiles and frequencies from data
qtiles <- ldply(unique(df$grpN),function(gr) quantile(df$EffectSize[df$grpN==gr],probs,na.rm=T,type=3,include.lowest=T))
freqs  <- ldply(unique(df$grpN),function(gr) table(cut(df$EffectSize[df$grpN==gr],breaks=qtiles[gr,],na.rm=T,include.lowest=T)))
labels <- sapply(unique(df$grpN),function(gr)levels(cut(round(df$EffectSize[df$grpN==gr],digits=4), breaks = qtiles[gr,],na.rm=T,include.lowest=T,right=T)))

# Check the Quantile bins!
ori           <-cbind(freq=as.numeric(t(freqs[1,])))
rownames(ori) <- labels[,1]
ori

rep           <-cbind(freq=as.numeric(t(freqs[2,])))
rownames(rep) <- labels[,2]
rep

# Get regular violinplot using package ggplot2
g.es  <- ggplot(df,aes(x=grp,y=EffectSize)) + geom_violin(aes(group=grpN),scale="width",fill="grey40",color="grey40",trim=T,adjust=1)
# Cut at quantiles using vioQtile() in C-3PR
g.es0 <- vioQtile(g.es,qtiles=qtiles,probs=probs)
# Garnish
g.es1 <- g.es0 +
  ggtitle("B") + xlab("") + ylab("Effect Size") +
  scale_y_continuous(breaks=c(-.25,-.5,0,.25,.5,.75,1),limits=c(-.5,1)) + mytheme + theme(legend.position=c(.25,.165), legend.text=element_text(size=20), legend.title=element_text(size=22), axis.text.x=element_text(size=26),  axis.text.y=element_text(size=22), axis.title.y=element_text(size=30, vjust=2.6), title=element_text(size=27)) 
# View
g.es1

# # Uncomment to save panel B as a seperate file
# ggsave("RPP_F1_VQPes.eps",plot=g.es1)

# VIEW panels in one plot using the multi.PLOT() function from C-3PR
multi.PLOT(g.pv1,g.es1,cols=2)