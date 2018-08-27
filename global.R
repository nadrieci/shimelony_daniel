do.call(file.remove, list(list.files("www/images", full.names = TRUE)))
file <- read.table("Matrix_prova_variant.csv", header = TRUE, sep=";")
# setwd("C:/Users/Daniel/Desktop/shimelonyo/shimelony/")
# source("https://bioconductor.org/biocLite.R") 
library(magrittr)
library(httpuv)
suppressPackageStartupMessages(library(shinyjs))
library(shinyWidgets)
library(ggplot2)
library(devtools)
library(shinythemes)



library(shiny)
suppressPackageStartupMessages(library(plotly))
# library(viridis)
suppressPackageStartupMessages(library(jsonlite))
library(shiny)
library(RColorBrewer)
library(readxl)
suppressPackageStartupMessages(library(DT))
library(xtable)
library(htmltools)
library(htmlwidgets)
# library(shinyHeatmaply)
suppressPackageStartupMessages(library(dplyr))
library(heatmaply)

library(knitr)
# library(shiny)
# suppressPackageStartupMessages(library(plotly))
# library(viridis)
# suppressPackageStartupMessages(library(jsonlite))
# library(shiny)
# library(RColorBrewer)
# library(readxl)
# library(xtable)
# library(htmltools)
# library(htmlwidgets)
# library(shinyHeatmaply)
# suppressPackageStartupMessages(library(dplyr))
# # suppressPackageStartupMessages(library(heatmaply))
library(ggcorrplot)
library(ggdendro)

# library(shinyFiles)

tope <- 50

# library(RLumShiny)
# options(warn=-1)
# library(circlize)
# library(RColorBrewer)
# library(GenomicRanges)
# library(data.table)
# library(grDevices)
source("functions.R")
source("plots.R")


#####################################################UI DATA########################################################    
index <- which(colnames(file)=="Flower")
i <- 1
organ_name <- colnames(file)[index]
organ_group_name <- NULL
while(organ_name != "Tendril"){
  organ_name <- colnames(file)[index]
  organ_group_name[[i]] <- colnames(file)[grep(organ_name,colnames(file))]
  index <- index+1 
  i <- i+1
}
organ_list <- unlist(organ_group_name) #This will be used later to change column class
organ_group_name <- sapply(organ_group_name, function(x) c(paste("ALL",x[1]),x))
names(organ_group_name)<- sapply(organ_group_name, function(x) x[2])
organ_group_name <- sapply(organ_group_name, function(x) x[-2])
organ_group_name$Som_embryo <- colnames(file)[grep("Somatic",colnames(file))]#Somatic pattern is different to others.
organ_group_name$Fruit <- list('Fruit Epicarp' = organ_group_name$Fruit[grep("epicarp", organ_group_name$Fruit)],
                               'Fruit Flesh' = organ_group_name$Fruit[grep("flesh", organ_group_name$Fruit)])
organ_group_name$Fruit$`Fruit Epicarp` <- c("ALL Fruit Epicarp",organ_group_name$Fruit$`Fruit Epicarp`)
organ_group_name$Fruit$`Fruit Flesh` <- c("ALL Fruit Flesh",organ_group_name$Fruit$`Fruit Flesh`)

#Nos servir? para buscar el m?nimo y el m?ximo de cada grupo (para el threshold de expression)
flower_values <- as.numeric(unlist(file[,colnames(file)[grep("Flower",colnames(file))]]))
fruit_values <- as.numeric(unlist(file[,colnames(file)[grep("Fruit",colnames(file))]]))
leaf_values <- as.numeric(unlist(file[,colnames(file)[grep("Leaf",colnames(file))]]))
ovary_values <- as.numeric(unlist(file[,colnames(file)[grep("Ovary",colnames(file))]]))
root_values <- as.numeric(unlist(file[,colnames(file)[grep("Root",colnames(file))]]))
seed_values <- as.numeric(unlist(file[,colnames(file)[grep("Seed",colnames(file))]]))
som_embryo_values <- as.numeric(unlist(file[,colnames(file)[grep("Som",colnames(file))]]))
stem_values <- as.numeric(unlist(file[,colnames(file)[grep("Stem",colnames(file))]]))
tendril_values <- as.numeric(unlist(file[,colnames(file)[grep("Tendril",colnames(file))]]))

##########################################Class converter###################################################

file$start <- as.numeric(file$start)
file$end <- as.numeric(file$end)

#En principio ya no sirve porque lo hago con el bucle for de abajo
# file$Flower <- as.numeric(gsub(",",".",file$Flower))
# file$Fruit <- as.numeric(gsub(",",".",file$Fruit))
# file$Leaf <- as.numeric(gsub(",",".",file$Leaf))
# file$Ovary <- as.numeric(gsub(",",".",file$Ovary))
# file$Root <- as.numeric(gsub(",",".",file$Root))
# file$Seed <- as.numeric(gsub(",",".",file$Seed))
# file$Som_embryo <- as.numeric(gsub(",",".",file$Som_embryo))
# file$Stem <- as.numeric(gsub(",",".",file$Stem))
# file$Tendril <- as.numeric(gsub(",",".",file$Tendril))
# file$Flower_female.early_.Charentais.Gy. <- as.numeric(gsub(",",".",file$Flower_female.early_.Charentais.Gy.))
# file$Flower_female.late_.Charentais.Gy. <- as.numeric(gsub(",",".",file$Flower_female.late_.Charentais.Gy.))

#We convert the input characters into a numeric class that we will be able to deal with.
for(i in 1:length(organ_list)){
file[[organ_list[i]]] <- as.numeric(gsub(",",".", file[[organ_list[i]]]))
}


################################MARKDOWN##############################3

#######Summary function###################
markdown_table_summary <- function(file){
unique_chr <- unique(file$Chr)
table_genes <- NULL
for(i in 1:length(unique_chr)){
  table_genes[[i]] <- subset(file$QTL, file$Chr == unique_chr[i])
}
names(table_genes) <- unique_chr

qtl_table <- NULL
for(i in 1:length(table_genes)){
  qtl_table[[i]] <- table(table_genes[i])[which(table(table_genes[i])!=0)]
}
names(qtl_table) <- unique_chr

names(which(unlist(qtl_table) == max(unlist(qtl_table))))
splited_max <- strsplit(names(which(unlist(qtl_table) == max(unlist(qtl_table)))),"")

max_qtl <- max(unlist(qtl_table))
max_qtl_chr_name <- paste0(splited_max[[1]][1:5], collapse = "")#Chr of the maximum qtl
max_qtl_name <- paste0(splited_max[[1]][7:length(splited_max[[1]])], collapse = "") #Maximum qtl

new_names <- NULL
total_genes <- NULL
max_chr <- length(table_genes[[1]])
name_max_chr <- names(table_genes[1])
for(i in 1:length(names(qtl_table))){
  new_names <- c(new_names, paste(names(qtl_table)[i],"QTL"),paste(names(qtl_table)[i],"genes"))
  total_genes <- c(total_genes, paste("Total",names(table_genes[i])),length(table_genes[[i]]))
  if(length(table_genes[[i]])> max_chr){
    max_chr <- length(table_genes[[i]])
    name_max_chr <- names(table_genes[i])
  }
}
initial_parameters <- c(max_qtl,max_qtl_chr_name, max_qtl_name, max_chr, name_max_chr)
ifelse(initial_parameters[2]==initial_parameters[5], initial_parameters <- c(initial_parameters,"likewise"),initial_parameters <- c(initial_parameters,"unlike"))
qtl_table <- lapply(qtl_table, `length<-`, max(lengths(qtl_table)))#Make all the rows of the same length by adding NA's

data_table <- data.frame(names(qtl_table[[1]]),qtl_table[1])
if(length(names(qtl_table))>1){#If the user only picks one chr the bucle for can't be performed
for(i in 2:length(names(qtl_table))){
  data_table <- data.frame(data_table,names(qtl_table[[i]]),qtl_table[i])
}}
data_table <- (data_table[,-grep("Var",colnames(data_table))])
data_table <- t(data_table)

rownames(data_table) <- new_names
colnames_data_table <- NULL
for(i in 1:length(colnames(data_table))){
  colnames_data_table <- c(colnames_data_table, paste0("QTL",i))
}
colnames_data_table <- c(colnames_data_table,"Total")
data_table <- data.frame(data_table, total_genes)
colnames(data_table) <- colnames_data_table

return(list(data_table, initial_parameters))
}

# initial_table_summary <- markdown_table_summary(file)
# final_table_summary <- markdown_table_summary(filtered_table)

#############Filtering recount#######################


##################################OTHERS##########################################

