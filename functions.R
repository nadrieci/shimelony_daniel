
######################################################DABIGFUNCTION########################################################################

filter <- function(file, number, starting_chr, ending_chr, gene_ontology, qtl,
                   geneID, transcriptID, fDescription, exclude_fDescription,
                   variant_family, v_effect, threshold_v_effect, v_consequence, threshold_v_consequence, v_provean, threshold_v_provean,
                   detailed_expr, flower_threshold, fruit_threshold,leaf_threshold,ovary_threshold,root_threshold,seed_threshold,
                   som_embryo_threshold,stem_threshold,tendril_threshold, ath, tom){

  file$start <- as.numeric(file$start)
  file$end <- as.numeric(file$end)
  file$Flower <- as.numeric(gsub(",",".",file$Flower))
  file$Fruit <- as.numeric(gsub(",",".",file$Fruit))
  file$Leaf <- as.numeric(gsub(",",".",file$Leaf))
  file$Ovary <- as.numeric(gsub(",",".",file$Ovary))
  file$Root <- as.numeric(gsub(",",".",file$Root))
  file$Seed <- as.numeric(gsub(",",".",file$Seed))
  file$Som_embryo <- as.numeric(gsub(",",".",file$Som_embryo))
  file$Stem <- as.numeric(gsub(",",".",file$Stem))
  file$Tendril <- as.numeric(gsub(",",".",file$Tendril))
  
  result_expression_independently <<- filter_expression(file, detailed_expr, flower_threshold, fruit_threshold,leaf_threshold,ovary_threshold,root_threshold,seed_threshold,
                                                        som_embryo_threshold,stem_threshold, tendril_threshold)
  result_expression <<- filter_expression(file, detailed_expr, flower_threshold, fruit_threshold,leaf_threshold,ovary_threshold,root_threshold,seed_threshold,
                              som_embryo_threshold,stem_threshold, tendril_threshold)
  
  if(variant_family == "SCPS"){
    result <- filter_variant_SCPS(result_expression)
    
  }
  if(variant_family == "VEDPS"){
    result <- filter_variant_VEDPS(result_expression)
    
  }
  result_chromosome_independently <<- filter_chromosome(file, number, starting_chr, ending_chr, gene_ontology, qtl)
  result_chromosome <<- filter_chromosome(result, number, starting_chr, ending_chr, gene_ontology, qtl)
  
  result_annotation_independently <<- filter_annotation(file, geneID, transcriptID, fDescription, exclude_fDescription)
  result_annotation <<- filter_annotation(result_chromosome, geneID, transcriptID, fDescription, exclude_fDescription)
  
  result_variant_independently  <<- filter_variant(file, v_effect, threshold_v_effect, v_consequence, threshold_v_consequence, v_provean, threshold_v_provean)
  result_variant  <<- filter_variant(result_annotation, v_effect, threshold_v_effect, v_consequence, threshold_v_consequence, v_provean, threshold_v_provean)
  orto <- intersect(rownames(result_variant),rownames(file))
  result <- cbind(result_variant,file[orto,(length(colnames(file))-1):length(colnames(file))])
  
  result_orthologs_independently <<- filter_orthologs(file, ath, tom)
  result_orthologs <<- filter_orthologs(result, ath, tom)
  
  result$Gene_ID <- createLink(result_orthologs$Chr, result_orthologs$start, result_orthologs$end)
  result <- result_orthologs
  colnames(result) <-gsub("_", " ",colnames(result))
  
  return(result)
  
}




######################################################CHROMOSOME###########################################################################

filter_chromosome <- function(file, number, starting_chr, ending_chr, gene_ontology, qtl){
  
  result <- filter_chromosome_number(file, number)
  result <- filter_chromosome_position(result, starting_chr, ending_chr)
  result <- filter_chromosome_qtl(result, qtl)
  result <- filter_chromosome_GO(result, gene_ontology)

  return(result)
}

  
  ##Chromosome number
  filter_chromosome_number <- function(file, number){
  
    ifelse(number == "ALL",result<-file,result<-subset(file, Chr %in% number))
    return(result)
  }
  
  ##Chromosome position
  filter_chromosome_position <- function(file, starting_chr, ending_chr) {
  
    starting_chr <- as.numeric(starting_chr)
    ending_chr <- as.numeric(ending_chr)
    result<-file[(starting_chr<=file$start) & (ending_chr>=file$end) ,]
    return(result)
  }
  
  #Chromosome GO
  filter_chromosome_GO <- function(file, gene_ontology){
    options(warn = -1)
    file$GO <- as.vector(file$GO)
    ifelse(gene_ontology == "ALL", result <- file, result <- file[unlist(sapply(gene_ontology, grep, file$GO, USE.NAMES = F)), ])
    return(result)
    
  }
  
  ##Chromosome qtl
  filter_chromosome_qtl <- function(file, qtl) {
    
    ifelse(qtl == "ALL",result <- file,result <- subset(file, QTL %in% qtl))
    return(result)
  }


######################################################ANNOTATION##########################################################################

filter_annotation <- function(file, geneID, transcriptID, fDescription, exclude_fDescription){
  
  result <- filter_annotation_geneID(file, geneID)
  result <- filter_annotation_transcriptID(result, transcriptID)
  result <- filter_annotation_fDescription(result, fDescription)
  result <- filter_annotation_excludefDescription(result, exclude_fDescription)
  return(result)
}

  filter_annotation_geneID <- function(file, geneID){
    
    ifelse(geneID == "ALL", result <- file, result <- subset(file, Gene_ID %in% geneID))
    return(result)
    
  }
  
  filter_annotation_transcriptID <- function(file, transcriptID){
  
    file$Transcript_ID <- as.vector(file$Transcript_ID)
    if(transcriptID == "ALL"){
      result <- file
    }
    else{
      result <- file[which(sapply(file$Transcript_ID, function(x) substring(x,nchar(x))) == transcriptID),]
    }
    return(result)
  }
  
  filter_annotation_fDescription <- function(file, fDescription){
    
    ifelse(fDescription == "ALL", result <- file, result <- subset(file, functional_description %in% fDescription))
    return(result)
    
  }

  filter_annotation_excludefDescription <- function(file, exclude_fDescription){
    
    ifelse(exclude_fDescription == "NONE", result <- file, result <- subset(file, functional_description != exclude_fDescription))
    return(result)
    
  }

#######################################################VARIANT#######################################################################

####################################################VARIANT SCPS VS VEDPS###############################################################
  
#Exchange SCPS columns by VEDPS columns  
filter_variant_VEDPS <- function(file){
  start <- which(colnames(file)=="MODIFIER") #The first variant column has to be call "MODIFIER"
  end   <- grep("MODIFIER", colnames(file)[(colnames(file) != colnames(file)[start])]) 
  #We seek the next column with the word "MODIFIER"
  colnames(file)[(end+1):length(colnames(file))] <-  colnames(file)[start:end]
  result <- file[,-start:-end]
  return(result)
} 

#Delete the VEDPS in order to show only the SCPS genes.  
filter_variant_SCPS <- function(file){
  start <- which(colnames(file)=="MODIFIER")
  end   <- grep("MODIFIER", colnames(file)[(colnames(file) != colnames(file)[start])])
  result <- file[,1:end]
  return(result)
}  

####################################################VARIANT FUNCTION####################################################################

filter_variant <- function(file, v_effect, threshold_v_effect, v_consequence, threshold_v_consequence, v_provean, threshold_v_provean){

  result <- filter_variant_effect(file, v_effect, threshold_v_effect)
  result <- filter_variant_consecuence(result, v_consequence, threshold_v_consequence)
  result <- filter_variant_provean(result, v_provean, threshold_v_provean, v_effect)

   return(result)
}

  filter_variant_effect <- function(file, v_effect, threshold_v_effect){
    if(length(v_effect)==1){
      if(v_effect== "ANY"){
        rows <- which(file[,which(colnames(file) %in% colnames(file)[(which(colnames(file)=="MODIFIER")):(which(colnames(file)=="MODIFIER")+3)])] >= threshold_v_effect, arr.ind = TRUE)
        rows <- unique(rows[,"row"])
        result <- file[rows,] 
    }
      else{
        rows <- which(file[,which(colnames(file) %in% v_effect)] >= threshold_v_effect)
        cols <- colnames(file[,(which(colnames(file)=="MODIFIER")):(which(colnames(file)=="MODIFIER")+3)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, v_effect))))
        result <- file[rows,cols]
      }
    }
    else{
      rows <- which(file[,which(colnames(file) %in% v_effect)] >= threshold_v_effect, arr.ind = TRUE)
      rows <- unique(rows[,"row"])
      cols <- colnames(file[,(which(colnames(file)=="MODIFIER")):(which(colnames(file)=="MODIFIER")+3)])
      cols <-  which( !(colnames(file)  %in% (setdiff(cols, v_effect))))
      result <- file[rows,cols]
      
    }
    return(result)
  }

  
  
  filter_variant_consecuence <- function(file, v_consequence, threshold_v_consequence){

    if(length(v_consequence)==1){
      if(v_consequence== "ANY"){
        rows <- which(file[,colnames(file)[(which(colnames(file)=="X3_prime_UTR")):(which(colnames(file)=="X3_prime_UTR")+19)]] >= threshold_v_consequence, arr.ind = TRUE)
        rows <- unique(rows[,"row"])
        result <- file[rows,] 
      }
      else{
        rows <- which(file[,which(colnames(file) %in% v_consequence)] >= threshold_v_consequence)
        cols <- colnames(file[,(which(colnames(file)=="X3_prime_UTR")):(which(colnames(file)=="X3_prime_UTR")+19)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, v_consequence))))
        result <- file[rows,cols]
        
      }
    }
    else{
      rows <- which(file[,which(colnames(file) %in% v_consequence)] >= threshold_v_consequence, arr.ind = TRUE)
      rows <- unique(rows[,"row"])
      cols <- colnames(file[,(which(colnames(file)=="X3_prime_UTR")):(which(colnames(file)=="X3_prime_UTR")+19)])
      cols <-  which( !(colnames(file)  %in% (setdiff(cols, v_consequence))))
      result <- file[rows,cols]
      
    }
    return(result)
  }  
  
  filter_variant_provean <- function(file, v_provean, threshold_v_provean, v_effect){
  if((!"HIGH" %in% v_effect)&(!"MODERATE" %in% v_effect)){
    result <- file[,-(length(colnames(file))-1):(-(length(colnames(file))))]
  }  
  
  else{      
    if(length(v_provean)==1){
      if(v_provean== "ANY"){
        rows <- which(file[,colnames(file)[(which(colnames(file)=="N_DELETERIOUS...2.")):(which(colnames(file)=="N_DELETERIOUS...2.")+1)]] >= threshold_v_provean, arr.ind = TRUE)
        rows <- unique(rows[,"row"])
        result <- file[rows,] 
      }
      else{
        rows <- which(file[,which(colnames(file) %in% v_provean)] >= threshold_v_provean)
        result <- file[rows,]
        
      }
    }
    else{
      rows <- which(file[,which(colnames(file) %in% v_provean)] >= threshold_v_provean, arr.ind = TRUE)
      rows <- unique(rows[,"row"])
      result <- file[rows,]
      
    }
  }
    return(result)
  }     

####################################################EXPRESSION####################################################################


filter_expression <- function(file, detailed_expr, flower_threshold, fruit_threshold,leaf_threshold,ovary_threshold,root_threshold,seed_threshold,
                              som_embryo_threshold, stem_threshold, tendril_threshold){
  
  result <- filter_expression_flower(file, detailed_expr, flower_threshold, fruit_threshold,leaf_threshold,ovary_threshold,root_threshold,seed_threshold,
                                     som_embryo_threshold,stem_threshold,tendril_threshold)

  return(result)
}



filter_expression_flower <- function(file, detailed_expr, flower_threshold, fruit_threshold,leaf_threshold,ovary_threshold,root_threshold,seed_threshold,
                                     som_embryo_threshold, stem_threshold, tendril_threshold){


  if(is.null(detailed_expr)){
    rows <- which(as.double(gsub(",",".",file[,"Flower"])) >= flower_threshold)
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Fruit"])) >= fruit_threshold))
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Leaf"])) >= leaf_threshold))
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Ovary"])) >= ovary_threshold))
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Root"])) >= root_threshold))
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Seed"])) >= seed_threshold))
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Som_embryo"])) >= som_embryo_threshold))
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Stem"])) >= stem_threshold))
    rows <- intersect(rows,which(as.double(gsub(",",".",file[,"Tendril"])) >= tendril_threshold))
    
    result <- file[rows,c(1:which(colnames(file)=="Tendril"),which(colnames(file)=="MODIFIER"):length(colnames(file)))]
  }
  
  else{
    grep.flower <- detailed_expr[grep("Flower",detailed_expr)]
    grep.fruit.epicarp <- detailed_expr[grep("picarp",detailed_expr)]
    grep.fruit.flesh <- detailed_expr[grep("lesh",detailed_expr)]
    grep.leaf <- detailed_expr[grep("Leaf",detailed_expr)]
    grep.ovary <- detailed_expr[grep("Ovary",detailed_expr)]
    grep.root <- detailed_expr[grep("Root",detailed_expr)]
    grep.seed <- detailed_expr[grep("Seed",detailed_expr)]
    grep.som_embryo <- detailed_expr[grep("Som_embryo",detailed_expr)]
    grep.stem <- detailed_expr[grep("Stem",detailed_expr)]
    grep.tendril <- detailed_expr[grep("Tendril",detailed_expr)]
    
        ####Flower####
    if(length(grep("ALL", grep.flower)) == 1){
      grep.all.flower <- colnames(file)[grep("Flower",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.flower)){
        rows <- c(rows,which(file[,grep.all.flower[i]] >= flower_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }
    else{
      
      if(length(grep.flower)==0){
        rows <- which(as.double(file[,"Flower"]) >= flower_threshold)
        cols <- setdiff(grep("Flower_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.flower)){
          rows <- c(rows,which(file[,grep.flower[i]] >= flower_threshold))
          if(i!=1){
          rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Flower",colnames(file))[2]):(grep("Fruit",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.flower))))
        file <- file[rows,cols]
      }
    }
    ####Fruit####
    rows <- which(as.double(gsub(",",".",file[,"Fruit"])) >= fruit_threshold)
    ####Fruit Epicarp####
    if(length(grep("ALL",grep.fruit.epicarp))== 1){
      file <- file[rows,]

    }
    else{

      if(length(grep.fruit.epicarp)==0){
        cols <- setdiff(grep("Fruit_epicarp", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        cols <- colnames(file[,(grep("Fruit_epicarp",colnames(file))[2]):(grep("Fruit_flesh",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.fruit.epicarp))))
        file <- file[rows,cols]
      }
    }
    ####Fruit Flesh####
    if(length(grep("ALL",grep.fruit.flesh))== 1){
      file <- file[rows,]

    }
    else{

      if(length(grep.fruit.flesh)==0){
        cols <- setdiff(grep("Fruit_flesh", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        cols <- colnames(file[,(grep("Fruit_flesh",colnames(file))[2]):(grep("Leaf",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.fruit.flesh))))
        file <- file[rows,cols]
      }
    }
    ####Leaf####
    if(length(grep("ALL", grep.leaf)) == 1){
      grep.all.leaf <- colnames(file)[grep("Leaf",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.leaf)){
        rows <- c(rows,which(file[,grep.all.leaf[i]] >= leaf_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }
    else{
      
      if(length(grep.leaf)==0){
        rows <- which(as.double(file[,"Leaf"]) >= leaf_threshold)
        cols <- setdiff(grep("Leaf_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.leaf)){
          rows <- c(rows,which(file[,grep.leaf[i]] >= leaf_threshold))
          if(i!=1){
            rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Leaf",colnames(file))[2]):(grep("Ovary",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.leaf))))
        file <- file[rows,cols]
      }
    }

    ####Ovary####

    if(length(grep("ALL", grep.ovary)) == 1){
      grep.all.ovary <- colnames(file)[grep("Ovary",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.ovary)){
        rows <- c(rows,which(file[,grep.all.ovary[i]] >= ovary_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }
    else{
      
      if(length(grep.ovary)==0){
        rows <- which(as.double(file[,"Ovary"]) >= ovary_threshold)
        cols <- setdiff(grep("Ovary_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.ovary)){
          rows <- c(rows,which(file[,grep.ovary[i]] >= ovary_threshold))
          if(i!=1){
            rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Ovary",colnames(file))[2]):(grep("Root",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.ovary))))
        file <- file[rows,cols]
      }
    }
    
    ####Root####
    if(length(grep("ALL", grep.root)) == 1){
      grep.all.root <- colnames(file)[grep("Root",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.root)){
        rows <- c(rows,which(file[,grep.all.root[i]] >= root_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }
    else{
      
      if(length(grep.root)==0){
        rows <- which(as.double(file[,"Root"]) >= root_threshold)
        cols <- setdiff(grep("Root_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.root)){
          rows <- c(rows,which(file[,grep.root[i]] >= root_threshold))
          if(i!=1){
            rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Root",colnames(file))[2]):(grep("Seed",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.root))))
        file <- file[rows,cols]
      }
    }
    ####Seed####

    if(length(grep("ALL", grep.seed)) == 1){
      grep.all.seed <- colnames(file)[grep("Seed",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.seed)){
        rows <- c(rows,which(file[,grep.all.seed[i]] >= seed_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }
    else{
      
      if(length(grep.seed)==0){
        rows <- which(as.double(file[,"Seed"]) >= seed_threshold)
        cols <- setdiff(grep("Seed_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.seed)){
          rows <- c(rows,which(file[,grep.seed[i]] >= seed_threshold))
          if(i!=1){
            rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Seed",colnames(file))[2]):(grep("Som_embryo",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.seed))))
        file <- file[rows,cols]
      }
    }
    
    ####Som_embryo####
    
    if(length(grep("ALL", grep.som_embryo)) == 1){
      grep.all.som_embryo <- colnames(file)[grep("Som_embryo",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.som_embryo)){
        rows <- c(rows,which(file[,grep.all.som_embryo[i]] >= som_embryo_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }

    else{
      
      if(length(grep.som_embryo)==0){
        rows <- which(as.double(file[,"Som_embryo"]) >= som_embryo_threshold)
        cols <- setdiff(grep("Somatic_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.som_embryo)){
          rows <- c(rows,which(file[,grep.som_embryo[i]] >= som_embryo_threshold))
          if(i!=1){
            rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Som_embryo",colnames(file))[2]):(grep("Stem",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.som_embryo))))
        file <- file[rows,cols]
      }
    }
    
    ####Stem####
    
    if(length(grep("ALL", grep.stem)) == 1){
      grep.all.stem <- colnames(file)[grep("Stem",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.stem)){
        rows <- c(rows,which(file[,grep.all.stem[i]] >= stem_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }
    
    else{
      
      if(length(grep.stem)==0){
        rows <- which(as.double(file[,"Stem"]) >= flower_threshold)
        cols <- setdiff(grep("Stem_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.stem)){
          rows <- c(rows,which(file[,grep.stem[i]] >= stem_threshold))
          if(i!=1){
            rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Stem",colnames(file))[2]):(grep("Tendril",colnames(file))[2] -1)])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.stem))))
        file <- file[rows,cols]
      }
    }
    ####Tendril####
    if(length(grep("ALL", grep.tendril)) == 1){
      grep.all.tendril <- colnames(file)[grep("Tendril",colnames(file))][-1]
      rows <- NULL
      for(i in 1:length(grep.all.tendril)){
        rows <- c(rows,which(file[,grep.all.tendril[i]] >= tendril_threshold))
        if(i!=1){
          rows <- rows[which(duplicated(rows))]
        }
      }
      file <- file[rows,] }
    
    else{
      
      if(length(grep.tendril)==0){
        rows <- which(as.double(file[,"Tendril"]) >= tendril_threshold)
        cols <- setdiff(grep("Tendril_", colnames(file)),colnames(file))
        file <- file[rows,-cols]
      }
      else {
        rows <- NULL
        for(i in 1:length(grep.tendril)){
          rows <- c(rows,which(file[,grep.tendril[i]] >= tendril_threshold))
          if(i!=1){
            rows <- rows[which(duplicated(rows))]
          }
        }
        
        cols <- colnames(file[,(grep("Tendril",colnames(file))[2]):(colnames(file)["MODIFIER"])])
        cols <-  which( !(colnames(file)  %in% (setdiff(cols, grep.tendril))))
        file <- file[rows,cols]
      }
    }
    

    result <- file

    }
  return(result)
}

#######################################################ORTHOLOGS##################################################################

filter_orthologs <- function(file, ath, tom){
  
  result <- filter_orthologs_ath(file, ath)
  result <- filter_orthologs_tom(result, tom)
  return(result)
}

filter_orthologs_ath <- function(file, ath){
  options(warn = -1)
  file$Ath_Orthologs <- as.vector(file$Ath_Orthologs)
  ifelse(ath == "ALL", result <- file, result <- file[unlist(sapply(ath, grep, file$Ath_Orthologs, USE.NAMES = F)), ])
  return(result)
  
}

filter_orthologs_tom <- function(file, tom){
  options(warn = -1)
  file$Tom_Orthologs <- as.vector(file$Tom_Orthologs)
  ifelse(tom == "ALL", result <- file, result <- file[unlist(sapply(tom, grep, file$Tom_Orthologs, USE.NAMES = F)), ])
  return(result)
  
}
######################################################GENELINK####################################################################


####This function creates a google search link####
# createLink <- function(val) {
# 
#   sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-link"> Search </a>',val)
#   # sprintf('<a href="https://www.melonomics.net/melonomics.html#/jbrowse?chr="',chr,'&start=',start,'&end=', end, 'target="_blank" class="btn btn-link"> Search </a>')
# 
# }

createLink <- function(chr,start,end) {
  sprintf('<a href="https://www.melonomics.net/melonomics.html#/jbrowse? 
chr=%s&start=%s&end=%s" "target="_blank" class="btn btn-link"> Search </a>',chr,start,end)
}

###################################################DOWNLOAD###################################################################

savePNG <- function(name_plot,plot) {
  filename <- paste("C:/Users/Daniel/Desktop/shimelony/www/images",name_plot,".png", sep="")
  dev.copy(png,filename)
  dev.off()

}

