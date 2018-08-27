###################################Multiple plot function#######################################################
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
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

#########################################Summary functions#############################################

genes_remaining_summary <- function(file_plot){
  
  remaining_genes <- dim(file_plot)[1]
  remaining_chr <- length(unique(file_plot$Chr))
  remaining_qtl <-  length(unique(file_plot$QTL))
  paste("Genes: ", remaining_genes, "\t","; Chr: ", remaining_chr, "; QTL: ", remaining_qtl, collapse =  "")
  
}

genes_remaining_plot <- function(file_plot){

chr_repart <-data.frame(table(file_plot$Chr))
colnames(chr_repart) <- c("Chromosome","Remaining genes")

# chr_repart_total <-data.frame(table(file$Chr))
# colnames(chr_repart_total) <- c("Chromosome","Initial genes")

theme_set(theme_bw())

# Draw plot
gg <- ggplot(chr_repart, aes(x=Chromosome, y=`Remaining genes`)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Genes vs Chromosome", subtitle= paste(dim(file_plot)[1], "genes from",length(which(chr_repart$`Remaining genes`!= 0)), "chromosomes")) + 
  theme(axis.text.x = element_text(size= 15, face="bold", angle=65, vjust=0.6))
# gg_total <-ggplot(chr_repart_total, aes(x=Chromosome, y=`Initial genes`)) +
#   geom_bar(stat="identity", width=.5, fill="tomato3") +
#   labs(title="Genes vs Chromosome",
#        subtitle= paste(dim(file)[1], "genes from",dim(chr_repart_total)[1], "chromosomes")) +
#   theme(axis.text.x = element_text(angle=65, vjust=0.6))

return(gg)

}


genes_remaining_plot_qtl <- function(file_plot){

  theme_set(theme_bw())
  
g <- ggplot(file_plot, aes(Chr))
gg <- g + geom_bar(position="dodge", aes(fill=QTL), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="QTL distribution by Chr", 
       subtitle=paste(length(unique(file_plot$QTL)), "QTL from", length(unique(file_plot$Chr)), "chromosomes")) 
return(gg)

}
#########################################GLOBAL FUNCTION###################################

plot_all <- function(file_plot,  categorical, numerical, categorical2){
  

    ggplot_3v(file_plot, categorical, numerical, categorical2)
  
  # if(type_graph == "Correlation"){
  #   ggplot_correlation(file_plot)
  # }

}

ggplot_3v <- function(file_plot, eje_x, eje_y_numeric, extra_classification ){

  x <- file_plot[,which(colnames(file_plot)==eje_x)]
  y <- file_plot[,which(colnames(file_plot)==eje_y_numeric)]
  z <- file_plot[,which(colnames(file_plot)==extra_classification)]
    ggplot(file_plot, 
       aes(x = x, y = y, 
           group = seq(1, nrow(file_plot)), 
           fill = z, label = z)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(size = 2, position = position_stack(vjust = 0.5))+
      labs(x = eje_x, y = eje_y_numeric)+ labs(fill = extra_classification)+
  ggtitle(paste("Value of ", eje_y_numeric, " per", eje_x, "and grouped each one per ", extra_classification))

}

######################################Correlogram##########################################################

ggplot_correlogram <- function(file_plot, correlogram_genes){
  if(correlogram_genes == "Genes"){
    ggplot_correlation_genes(file_plot)
  }
  else{
    ggplot_correlation_expression(file_plot)
  }
}

#Correlogram of expressions
ggplot_correlation_expression <- function(file_plot){
  
  rownames(file_plot) <- file_plot[,1]
  file_plot <- file_plot[,names(which(lapply(file_plot, class)=="numeric"))[c(-1,-2)]]
  corr <- round(cor(file_plot), 1)
  ggcorrplot(corr, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 3, 
             method="circle", 
             colors = c("tomato2", "white", "springgreen3"), 
             title="Correlogram of expressions", 
             ggtheme=theme_bw)
}

ggplot_correlation_genes <- function(file_plot){

  rownames(file_plot) <- file_plot[,1]
  file_plot <- t(file_plot[,names(which(lapply(file_plot, class)=="numeric"))[c(-1,-2)]])
  file_plot <- file_plot[,which(!apply(file_plot,2,sd)==0)]
  corr <- round(cor(file_plot), 1)
  ggcorrplot(corr, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 3, 
             method="circle", 
             colors = c("tomato2", "white", "springgreen3"), 
             title="Correlogram of genes", 
             ggtheme=theme_bw)

}


########################################################CLUSTERING###############################################################
ggplot_clustering <- function(file_plot, clustering_genes, clustering_log){
  
  if(clustering_genes == "Genes"){
    if(clustering_log == TRUE){
      ggplot_clustering_genes_log(file_plot)
    }
    else{
      ggplot_clustering_genes_normal(file_plot)
    }
  }
  else{
    if(clustering_log ==FALSE){
      ggplot_clustering_expression_normal(file_plot)
    }
    else{
      ggplot_clustering_expression_log(file_plot)
    }
    
  }
  
}

#Clustering por genes sin aplicar logaritmos
ggplot_clustering_genes_normal <- function(file_plot){
  
  rownames(file_plot) <- file_plot[,1]
  file_plot <- file_plot[,names(which(lapply(file_plot, class)=="numeric"))[c(-1,-2)]]
  theme_set(theme_bw())
  
  hc <- hclust(dist(file_plot), "average")  # hierarchical clustering
  
  # plot
  ggdendrogram(hc, rotate = TRUE, size = 2)  
}

#Clustering por genes aplicando logaritmos y añadiendo 1 a todos los valores de la matriz
ggplot_clustering_genes_log <- function(file_plot){
  
  rownames(file_plot) <- file_plot[,1]
  file_plot <- log((file_plot[,names(which(lapply(file_plot, class)=="numeric"))[c(-1,-2)]])+1)
  theme_set(theme_bw())
  
  hc <- hclust(dist(file_plot), "average")  # hierarchical clustering
  
  # plot
  ggdendrogram(hc, rotate = TRUE, size = 2)  
}


#Clustering por organo sin aplicar logaritmo
ggplot_clustering_expression_normal <- function(file_plot){
  # browser()
  rownames(file_plot) <- file_plot[,1]
  file_plot <- t(file_plot[,names(which(lapply(file_plot, class)=="numeric"))[c(-1,-2)]])
  
  theme_set(theme_bw())
  
  hc <- hclust(dist(file_plot), "average")  # hierarchical clustering
  # hc$labels <- merge(x = hc$labels, y = df,  by.x = "label", by.y = "State")
  # plot
  ggdendrogram(hc, rotate = TRUE, size = 2)  
}

#Clustering por organo aplicando logaritmo y añadiendo 1 a todos los valores de la matriz
ggplot_clustering_expression_log <- function(file_plot){
  rownames(file_plot) <- file_plot[,1]
  file_plot <- log(t(file_plot[,names(which(lapply(file_plot, class)=="numeric"))[c(-1,-2)]])+1)
  
  theme_set(theme_bw())
  
  hc <- hclust(dist(file_plot), "average")  # hierarchical clustering
  
  # plot
  ggdendrogram(hc, rotate = TRUE, size = 2)  
}

######################################DIVERGING BARS#################################################################


#By adding the multiplot function we can autosize the dim of the plots



# file_plot<-file[round(runif(20,1,3437)),]
ggplot_diverging_bars <- function(file_plot, expression_name){
  
  rownames(file_plot) <- file_plot[,1]
  file_plot <- file_plot[,names(which(lapply(file_plot, class)=="numeric"))[c(-1,-2)]]
  file_plot$`gene name` <- rownames(file_plot)
  myplots <- list()
  for(i in 1:length(expression_name)){
    
    file_plot$`expression z` <- round((file_plot[[expression_name[i]]] - mean(file_plot[[expression_name[i]]]))/sd(file_plot[[expression_name[i]]]), 2)
    file_plot$`expression type` <- ifelse(file_plot$`expression z` < 0, "below", "above")  # above / below avg flag
    file_plot<- file_plot[order(file_plot$`expression z`),]
    file_plot$`gene name` <- factor(file_plot$`gene name`, levels = file_plot$`gene name`)
    
    myplots[[i]] <- ggplot(file_plot, aes(x=`gene name`, y= `expression z`, label=`expression z`)) + 
      geom_bar(stat='identity', aes(fill=`expression type`), width=.5)  +
      scale_fill_manual(name=expression_name[i], 
                        labels = c("Above Average", "Below Average"), 
                        values = c("above"="#00ba38", "below"="#f8766d")) + 
      labs(subtitle="Normalised expression", 
           title= expression_name[i]) + 
      coord_flip()
    
  }
#12 is the maximum plots that can be visualize at once.
  if(length(myplots)<=9){
    myplots <- multiplot(plotlist = myplots,cols=ceiling((length(myplots)/3)))
    return (myplots)
  }
  else{paste("There is a sufficiency in the world for man's need but not for man's greed. The maximum number of displayable plots is 9. At this moment", length(myplots),"expressions are selected")}
  
}

####################################CIRCOS##################################################
# file_plot <- file
# 
# file_circos <- data.frame(Chromosome = file_plot$Chr, ChromStart = file_plot$start, ChromEnd = file_plot$end, Gene = file_plot$Gene_ID ,qtl = file_plot$QTL ,Flower = file_plot$Flower,
#                           Fruit = file_plot$Fruit, Leaf= file_plot$Leaf, Ovary= file_plot$Ovary, Root= file_plot$Root, Seed= file_plot$Seed)
# file_circos$Chromosome <- gsub("chr0", "chr", file_circos$Chromosome)
# library(RCircos)
# data(UCSC.HG19.Human.CytoBandIdeogram);
# ideogram <- data.frame(file_circos[c(1:3,5)],Stain ="gneg")
# 
# 
# chr.exclude <- NULL;
# cyto.info <- ideogram
# tracks.inside <- 10;
# tracks.outside <- 0;
# RCircos.Set.Core.Components(cyto.info, chr.exclude,
#                               + tracks.inside, tracks.outside);
# max_chr <- NULL
# min_chr <- NULL
# for(i in 1:length(unique(file_circos$Chromosome))){
#   max_chr <- c(max_chr, max(file_circos$ChromStart[file_circos$Chromosome==unique(file_circos$Chromosome)[i]]))
#   min_chr <- c(min_chr, min(file_circos$ChromStart[file_circos$Chromosome==unique(file_circos$Chromosome)[i]]))
# }
# 
# circos_output <- data.frame(chr = unique(file_circos$Chromosome), start = min_chr, end = max_chr)
# out.file <- "RCircosMeloGenome.pdf"
# pdf(file=out.file, height=8, width=8, compress=TRUE)
# 
# par(mai=c(0.25, 0.25, 0.25, 0.25));
# plot.new();
# plot.window(c(-2.5,2.5), c(-2.5, 2.5));
# 
# RCircos.Chromosome.Ideogram.Plot()
# 
# pdf(file="RCircos.melo.pdf", height=8, width=8);
# 
#  RCircos.Set.Plot.Area();
# 
#    title("RCircos 2D Track Plot with Cucumbis melo Genome");
# 
#  RCircos.Chromosome.Ideogram.Plot();
# 
#  data(RCircos.Gene.Label.Data);
# 
#  RCircos.Gene.Connector.Plot(genomic.data=file_circos[-5], track.num=1, side="in");
#  RCircos.Gene.Name.Plot(gene.data=file_circos[-5], name.col=4, track.num=2, side="in");
# 
# 
#  RCircos.Heatmap.Plot(heatmap.data=file_circos, data.col=6,track.num=5, side="in");
# 
#  data(RCircos.Scatter.Data);
# 
#  RCircos.Scatter.Data[,1] <- paste0("chr", RCircos.Scatter.Data[,1])
# 
#  RCircos.Scatter.Plot(scatter.data=RCircos.Scatter.Data, data.col=5, 
#                            +                 track.num=6, side="in", by.fold=1);
# 
# 
# 
# 
#  data(RCircos.Line.Data);
# 
#  RCircos.Line.Data[,1] <- paste0("chr", RCircos.Line.Data[,1])
# 
#  RCircos.Line.Plot(line.data=RCircos.Line.Data, data.col=5, 
#                         +                 track.num=7, side="in");
# 
# 
# 
#  data(RCircos.Histogram.Data);
# 
#  RCircos.Histogram.Plot(hist.data=RCircos.Histogram.Data, data.col=4, 
#                              +                 track.num=8, side="in");
# 
# 
#  data(RCircos.Tile.Data);
# 
#  RCircos.Tile.Plot(tile.data=RCircos.Tile.Data, track.num=9, side="in");
# 
# 
# 
#  data(RCircos.Link.Data);
# 
#  RCircos.Link.Plot(link.data=RCircos.Link.Data, track.num=11, 
#                         +                 by.chromosome=FALSE);
# 
# 
#  data(RCircos.Ribbon.Data);
# 
#  RCircos.Ribbon.Plot(ribbon.data=RCircos.Ribbon.Data, track.num=11, 
#                           +                 by.chromosome=FALSE, twist=FALSE);
# 
#  #   Close the graphic device and clear memory
#    #   _________________________________________________________________
#    #   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 
#    dev.off();
# 

#################################Treemap######################################

# plot
# treeMapCoordinates <- treemapify(proglangs,
#                                  area = "value",
#                                  fill = "parent",
#                                  label = "id",
#                                  group = "parent")
# 
# treeMapPlot <- ggplotify(treeMapCoordinates) + 
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   scale_fill_brewer(palette = "Dark2")
# 
# print(treeMapPlot)

#####UI####

#Cuadrado que se puede mover, en el eje x bla, en el y tal y en el z lo que sea, poniendo factor 
#o numeric en cada uno.

# unique(lapply(file_plot, class))
# names(which(lapply(file_plot, class)=="integer"))
# c(names(which(lapply(file_plot, class)=="numeric")),names(which(lapply(file_plot, class)=="integer")))
# 
# c(names(which(lapply(file_plot[,1:which(colnames(file_plot)=="QTL")], class)=="factor")),names(which(lapply(file_plot, class)=="character")))


