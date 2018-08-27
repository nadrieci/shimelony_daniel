#VERSIoN 1.4

#scp -r /home/jortega/Desktop/shimelony root@10.200.21.46:/srv/shiny-server/
#ssh root@10.200.21.46
#sudo -i R
#rm -rf /srv/shiny-server//Shimelony/
#mv /root/shimelony /srv/shiny-server/
#rm -rf /var/log/shiny-server/*.*

# suppressPackageStartupMessages(library(DT))
# setwd(paste(getwd(),"/data", sep=""))
d=data(package='datasets')$results[,'Item']
d=d[which(d=='mtcars')]
d<- c("expressions", d)

# options(warn=-1)
server <- function(input, output, session) {
  
  
  observe({
    toggle(condition = input$foo, selector = "#navlistPanel li a")
    # toggle(condition = input$foo, selector = "#navlistPanel li a[data-value= Position]")
    
  })
  
  #### Expression panel to UI####
  output$expression_bottom <- renderUI({
    vector <- NULL
    if(input$flower_detailed == TRUE){
      vector <- c(vector, "Flower")
    }
    if(input$fruit_detailed == TRUE){
      vector <- c(vector, "Fruit")
    }
    if(input$leaf_detailed == TRUE){
      vector <- c(vector, "Leaf")
    }
    if(input$ovary_detailed == TRUE){
      vector <- c(vector, "Ovary")
    }
    if(input$root_detailed == TRUE){
      vector <- c(vector, "Root")
    }
    if(input$seed_detailed == TRUE){
      vector <- c(vector, "Seed")
    }
    if(input$som_embryo_detailed == TRUE){
      vector <- c(vector, "Som_embryo")
    }
    if(input$stem_detailed == TRUE){
      vector <- c(vector, "Stem")
    }
    if(input$tendril_detailed == TRUE){
      vector <- c(vector, "Tendril")
    }
    organ_group_name <- organ_group_name[vector]
    conditionalPanel(condition="input.flower_detailed == true || input.fruit_detailed == true
                     || input.leaf_detailed == true || input.root_detailed == true 
                     || input.seed_detailed == true || input.som_embryo_detailed == true  
                     || input.stem_detailed == true || input.tendril_detailed == true || input.ovary == true ",
                     selectizeInput("detailed_expr", "Select an expression: ", organ_group_name, multiple = TRUE)
    )
    
  })
  
  
  
  ####This is da function nigga.####
  filtering <-reactive({
    
    
    #Validation comprises some user friendly error messages.
    shiny::validate(
      need(input$chr != "", "Please select one or more chromosomes (even all).")
    )
    shiny::validate(
      need(input$start_chr < max(file$end), "There are no chromosomes starting at such a high position.")
    )
    shiny::validate(
      need(input$end_chr > min(file$start), "There are no chromosomes ending at such a low position.")
    )
    shiny::validate(
      need(input$GO != "", "Please select one or more gene ontology terms (even all). ")
    )
    shiny::validate(
      need(input$qtl != "", "Please select one or more qtl (even all).")
    )
    shiny::validate(
      need(input$geneID != "", "Please select one or more gene ID's (even all).")
    )
    shiny::validate(
      need(input$transcriptID != "", "Please select one or more gene transcripts ID's (even all).")
    )
    shiny::validate(
      need(input$fDescription != "", "Please select one or more functional descriptions (even all). ")
    )
    shiny::validate(
      need(input$v_effect != "", "Please select one or more variant efects (even all). ")
    )
    
    
    
    # if (is.null(v$data)) return(v$data <- file)
    #Our filter function is composed by another functions stored in functions.R file, it requires all the UI inputs.
    filtered_table <<- filter(file,
                              input$chr, input$start_chr, input$end_chr, input$GO, input$qtl,
                              input$geneID, input$transcriptID, input$fDescription, input$exclude_fDescription,
                              input$variant_family, input$v_effect, input$threshold_v_effect, input$v_consequence,
                              input$threshold_v_consequence, input$v_provean, input$threshold_v_provean
                              , input$detailed_expr, input$flower_threshold, input$fruit_threshold,input$leaf_threshold, input$ovary_threshold, input$root_threshold,input$seed_threshold,
                              input$som_embryo_threshold,input$stem_threshold, input$tendril_threshold,
                              input$ath, input$tom
    )
    
  })
  
  
  ####This function shows the GO link####
  output$link_GO <- renderUI({
    
    shiny::validate(
      need(input$GO != "", "Please select one GO. ")
    )
    href <- input$GO
    href <- paste("http://amigo.geneontology.org/amigo/term/",href, sep="")
    url <- a("GO", href=href)
    
    if(input$GO =="ALL"){
      paste("I can link you to any GO")
    }
    else{
      tagList(paste("Search for",input$GO), url)}
  })
  
  ####This function shows the functional description link####
  output$link_fDescription <- renderUI({
    
    shiny::validate(
      need(input$fDescription != "", "Please select one functional descriptions. ")
    )
    
    href <- paste(unlist(strsplit(input$fDescription, " ")), collapse = "+")
    href <- paste("http://www.uniprot.org/uniprot/?query=",href,"&sort=score")
    url <- a("Uniprot database", href=href)
    
    
    if(input$fDescription =="ALL"){
      paste("I can link you to any function in uniprot")
    }
    else{
      tagList(paste("Search for",input$fDescription), url)}
  })
  
  
  
  ####The output table####
  output$table <- DT::renderDataTable({
    if(input$update == 0){my_table <- file}
    else{
      input$update
      isolate({
        my_table <- datatable(filtering(), escape = FALSE,extensions = 'Buttons',
                              options = list( dom = 'Blfrtip', #Each letter is related to an element, in order to show the length of the list I add the l. 
                                              #detailed explanation here : https://datatables.net/reference/option/dom
                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(10,25, 50, 100, -1), list('10', '25', '50','100', 'All')), paging = T)
        ) 
        # %>% formatStyle(colnames(filtering())[c(4:6,8:9)],backgroundColor = 'grey', Color= 'blue') %>%
        #   formatStyle(colnames(filtering())[c(1:3,7)],backgroundColor = '#999999', Color= '#3366CC') %>%
        #   formatStyle(colnames(filtering())[10:18],backgroundColor = '#d5f4e6', Color= 'green')
      })}
    return(my_table)
  }
  ,extensions = 'Buttons',
  options = list( dom = 'Blfrtip', #Each letter is related to an element, in order to show the length of the list I add the l. 
                  #detailed explanation here : https://datatables.net/reference/option/dom
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu = list(c(10,25, 50, 100, -1), list('10', '25', '50','100', 'All')), paging = T)
  
  )
  
  #### Plot panel to UI####
  #####Previsualization tab#####
  
  output$remaining_genes_summary <- renderText({
    input$update
    isolate({
      genes_remaining_summary(filtering())
    })
  })
  
  output$remaining_genes_plot <- renderPlot({
    input$update
    isolate({
      genes_remaining_plot(filtering())
    })
  })
  
  output$remaining_genes_plot_qtl <- renderPlot({
    input$update
    isolate({
      genes_remaining_plot_qtl(filtering())
    })
  })
  
  
  
  ####Multiple windows plotting####
  tabIndex <- reactiveVal(0)
  observeEvent(input$newTab, {
    tabIndex(tabIndex() + 1)
    appendTab("myTabs", tabPanel(paste("Plot",tabIndex()),
                                 
                                 output[[paste("plot_download_pdf", tabIndex(), sep="_")]] <- downloadHandler(
                                   filename = function() {
                                     paste("plot",tabIndex(),"pdf", sep=".")
                                   },
                                   content = function(file) {
                                     file.copy(paste("plot",tabIndex(),"pdf", sep="."), file, overwrite=TRUE)
                                   }
                                 ),
                                 
                                 conditionalPanel(
                                   condition = "input.plot_1 == 1",
                                   
                                   tags$div(img(src = paste("images/plot",tabIndex(),".png",sep=""), height = 750, width = 1000))
                                   
                                 ),
                                 conditionalPanel(
                                   condition = "input.plot_1 == 0",
                                   tagList( 
                                     actionButton(paste("plot",tabIndex(), sep="_"), paste("Fix plot",tabIndex(), sep=" ")),
                                     
                                     
                                     selectizeInput(
                                       inputId = paste("type_graph",tabIndex(), sep="_"), 
                                       label = "Choose one type of graph", 
                                       choices = c("Stacked","Histogram","Snpeff","Correlation"), selected = "Stacked",
                                       multiple = TRUE),
                                     
                                     selectizeInput(paste("categorical",tabIndex(), sep="_"), "Categorical Variable: ",
                                                    choices = c(names(which(lapply(filtering()[,1:which(colnames(filtering())=="QTL")], class)=="factor")),names(which(lapply(filtering(), class)=="character")))
                                                    ,
                                                    selected = "Chr"),
                                     selectizeInput(paste("numerical",tabIndex(), sep="_"), "Numerical Variable: ",
                                                    choices = c(names(which(lapply(filtering(), class)=="numeric")),names(which(lapply(filtering(), class)=="integer")))
                                                    ,
                                                    selected = "Flower"),
                                     
                                     conditionalPanel(condition="input.type_graph.indexOf('Stacked') != -1",
                                                      selectizeInput(paste("categorical2",tabIndex(), sep="_"), "Second Categorical Variable: ",
                                                                     choices = c(names(which(lapply(filtering()[,1:which(colnames(filtering())=="QTL")], class)=="factor")),names(which(lapply(filtering(), class)=="character")))
                                                                     ,
                                                                     selected = "QTL")
                                     )
                                   )
                                   
                                   
                                   ,output[[paste("plot", tabIndex(), sep="_")]] <- renderPlot({
                                     
                                     
                                     
                                     shiny::validate(
                                       need(input[[paste("type_graph",tabIndex(), sep="_")]] != "", "Things that are white: Snow, coconut meat, Alaska and Walter and even this area unless you choose some type of graph ")
                                     )
                                     ggsave(paste("www/images/plot",tabIndex(),".png", sep=""), plot_all(filtering(),input[[paste("type_graph",tabIndex(), sep="_")]],input[[paste("categorical",tabIndex(), sep="_")]],
                                                                                                         input[[paste("numerical",tabIndex(), sep="_")]], input[[paste("categorical2",tabIndex(), sep="_")]])  )
                                     plot_all(filtering(),input[[paste("type_graph",tabIndex(), sep="_")]],input[[paste("categorical",tabIndex(), sep="_")]],
                                              input[[paste("numerical",tabIndex(), sep="_")]], input[[paste("categorical2",tabIndex(), sep="_")]])
                                     
                                   })
                                   
                                   
                                   
                                   
                                   
                                 )  #end of tag list
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 , select=TRUE
                                 
                                 
    ))
    
    
  })
  
  observeEvent(input$removeTab, {
    removeTab("myTabs", target=input$myTabs)
  })
  
  ################HEATMAP#####################
  
  TEMPLIST<-new.env()
  TEMPLIST$d<-d
  #Annotation Variable UI ----
  observeEvent(data.sel(),{
    output$annoVars<-renderUI({
      data.in=data.sel()
      NM=NULL
      
      if(any(sapply(data.in,class)=='factor')){
        NM=names(data.in)[which(sapply(data.in,class)=='factor')]  
      } 
      column(width=4,
             selectizeInput('annoVar','Annotation',choices = names(data.in),selected=NM,multiple=T,options = list(placeholder = 'select columns',plugins = list("remove_button")))
      )
    })
    
    #Sampling UI ----  
    output$sample<-renderUI({
      list(
        column(4,textInput(inputId = 'setSeed',label = 'Seed',value = sample(1:10000,1))),
        column(4,numericInput(inputId = 'selRows',label = 'Number of Rows',min=1,max=pmin(500,nrow(data.sel())),value = pmin(500,nrow(data.sel())))),
        column(4,selectizeInput('selCols','Columns Subset',choices = names(data.sel()),multiple=T))
      )
    })
  })
  
  #Data Selection UI ----
  output$data=renderUI({
    # browser()
    if(!is.null(input$mydata)) TEMPLIST$d=c(input$mydata$name,TEMPLIST$d)
    selData=head(TEMPLIST$d,1)
    selectInput("data","Select Data",TEMPLIST$d,selected = selData)
  })
  
  
  #Color Pallete UI ----
  output$colUI<-renderUI({
    
    colSel='Vidiris'
    if(input$transform_fun=='cor') colSel='RdBu'
    if(input$transform_fun=='is.na10') colSel='grey.colors'
    
    selectizeInput(inputId ="pal", label ="Select Color Palette",
                   choices = c('Vidiris (Sequential)'="viridis",
                               'Magma (Sequential)'="magma",
                               'Plasma (Sequential)'="plasma",
                               'Inferno (Sequential)'="inferno",
                               'Magma (Sequential)'="magma",
                               'Magma (Sequential)'="magma",
                               
                               'RdBu (Diverging)'="RdBu",
                               'RdYlBu (Diverging)'="RdYlBu",
                               'RdYlGn (Diverging)'="RdYlGn",
                               'BrBG (Diverging)'="BrBG",
                               'Spectral (Diverging)'="Spectral",
                               
                               'BuGn (Sequential)'='BuGn',
                               'PuBuGn (Sequential)'='PuBuGn',
                               'YlOrRd (Sequential)'='YlOrRd',
                               'Heat (Sequential)'='heat.colors',
                               'Grey (Sequential)'='grey.colors'),
                   selected=colSel)
  })
  
  #Manual Color Range UI ----
  output$colRng=renderUI({
    if(!is.null(data.sel())) {
      rng=range(data.sel(),na.rm = TRUE)
    }else{
      rng=range(mtcars) # TODO: this should probably be changed
    }
    # sliderInput("colorRng", "Set Color Range", min = round(rng[1],1), max = round(rng[2],1), step = .1, value = rng)  
    n_data = nrow(data.sel())
    
    min_min_range = ifelse(input$transform_fun=='cor',-1,-Inf)
    min_max_range = ifelse(input$transform_fun=='cor',1,rng[1])
    min_value = ifelse(input$transform_fun=='cor',-1,rng[1])
    
    max_min_range = ifelse(input$transform_fun=='cor',-1,rng[2])
    max_max_range = ifelse(input$transform_fun=='cor',1,Inf)
    max_value = ifelse(input$transform_fun=='cor',1,rng[2])
    
    a_good_step = 0.1 # (max_range-min_range) / n_data
    
    list(
      numericInput("colorRng_min", "Set Color Range (min)", value = min_value, min = min_min_range, max = min_max_range, step = a_good_step),
      numericInput("colorRng_max", "Set Color Range (max)", value = max_value, min = max_min_range, max = max_max_range, step = a_good_step)
    )
    
  })
  
  #Import/Select Data ----
  data.sel=eventReactive(input$data,{
    if(input$data == "expressions"){
      
      data.in = filtering()[1:tope,10:18]
      rownames(data.in) <- filtering()[1:tope,2]
    }
    else{
      if(input$data%in%d){
        eval(parse(text=paste0('data.in=as.data.frame(datasets::',input$data,')')))
      }else{
        data.in=importSwitch(input$mydata[input$mydata$name%in%input$data,])
      }
    }
    data.in=as.data.frame(data.in)
    # data.in=data.in[,sapply(data.in,function(x) class(x))%in%c('numeric','integer')] # no need for this
    return(data.in)
  })  
  
  #Building heatmaply ----
  interactiveHeatmap<- reactive({
    data.in=data.sel()
    if(input$showSample){
      shiny::validate(
        need(input$selRows != "", "Please select some rows to subset.")
      )
      if(!is.null(input$selRows)){
        set.seed(input$setSeed)
        if((input$selRows >= 2) & (input$selRows <= nrow(data.in))){
          # if input$selRows == nrow(data.in) then we should not do anything (this save refreshing when clicking the subset button)
          # browser()
          shiny::validate(
            need(length(input$selCols) != 1, "A one-column heatmap is as useless as a wooden frying-pan")
          )
          if(length(input$selCols)<1) data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRows)),]
          
          if(length(input$selCols)>1) data.in=data.in[sample(1:nrow(data.in),pmin(500,input$selRows)),input$selCols]
        }
        
      }
      
    }
    # ss_num = sapply(data.in,function(x) class(x)) %in% c('numeric','integer') # in order to only transform the numeric values
    
    if(length(input$annoVar)>0){
      if(all(input$annoVar%in%names(data.in))) 
        data.in <- data.in%>%mutate_at(funs(factor),.vars=vars(input$annoVar))
    } 
    
    ss_num =  sapply(data.in, is.numeric) # in order to only transform the numeric values
    
    if(input$transpose) data.in=t(data.in)
    if(input$transform_fun!='.'){
      
      if(input$transform_fun=='log') {
        shiny::validate(
          need( length(which(data.in == 0)) < 2, "You shall not pass this asymptote!... I mean, log 0 is not defined and some of the data values are 0")
        )
        
        data.in[, ss_num]= apply(data.in[, ss_num],2,log)}
      if(input$transform_fun=='sqrt') data.in[, ss_num]= apply(data.in[, ss_num],2,sqrt) 
      if(input$transform_fun=='normalize') data.in=heatmaply::normalize(data.in)
      if(input$transform_fun=='scale') data.in[, ss_num] = scale(data.in[, ss_num])
      if(input$transform_fun=='percentize') data.in=heatmaply::percentize(data.in)
      
    } 
    
    if(input$transform_fun_correlation=='Pearson') data.in = 1-cor((data.in), method = "pearson")
    if(input$transform_fun_correlation=='Spearman') data.in = 1-cor((data.in), method = "spearman")
    
    
    
    if(!is.null(input$tables_true_search_columns)) 
      data.in=data.in[activeRows(input$tables_true_search_columns,data.in),]
    if(input$colRngAuto){
      ColLimits=NULL 
    }else{
      ColLimits=c(input$colorRng_min, input$colorRng_max)
    }
    
    distfun_row = function(x) dist(x, method = input$distFun_row)
    distfun_col =  function(x) dist(x, method = input$distFun_col)
    
    hclustfun_row = function(x) hclust(x, method = input$hclustFun_row)
    hclustfun_col = function(x) hclust(x, method = input$hclustFun_col)
    
    p <- heatmaply(data.in,
                   main = input$main,xlab = input$xlab,ylab = input$ylab,
                   row_text_angle = input$row_text_angle,
                   column_text_angle = input$column_text_angle,
                   dendrogram = input$dendrogram,
                   branches_lwd = input$branches_lwd,
                   seriate = input$seriation,
                   colors=eval(parse(text=paste0(input$pal,'(',input$ncol,')'))),
                   distfun_row =  distfun_row,
                   hclustfun_row = hclustfun_row,
                   distfun_col = distfun_col,
                   hclustfun_col = hclustfun_col,
                   k_col = input$c, 
                   k_row = input$r,
                   limits = ColLimits) %>% 
      layout(margin = list(l = input$l, b = input$b, r='0px'))
    # p$elementId <- NULL
    p$x$layout$xaxis$ticktext[1:length(colnames(data.in))]<- colnames(data.in)[as.numeric(p$x$layout$xaxis$ticktext[1:length(colnames(data.in))])]
    p$x$layout$yaxis2$ticktext[1:length(rownames(data.in))]<- rownames(data.in)[as.numeric(p$x$layout$yaxis2$ticktext[1:length(rownames(data.in))])]
    p$x$layout$yaxis2$categoryarray[1:length(rownames(data.in))]<- rownames(data.in)[as.numeric(p$x$layout$yaxis2$categoryarray[1:length(rownames(data.in))])]
    p
  })
  
  #Render Plot ----
  observeEvent(input$data,{
    output$heatout <- renderPlotly({
      if(!is.null(input$data))
        interactiveHeatmap()
    })
  })
  
  #Render Data Table ----
  output$tables=DT::renderDataTable(data.sel(),server = T,filter='top',
                                    extensions = c('Scroller','FixedHeader','FixedColumns','Buttons','ColReorder'),
                                    options = list(
                                      dom = 't',
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'),
                                      colReorder = TRUE,
                                      scrollX = TRUE,
                                      fixedColumns = TRUE,
                                      fixedHeader = TRUE,
                                      deferRender = TRUE,
                                      scrollY = 500,
                                      scroller = TRUE
                                    ))
  
  #Clone Heatmap ----
  observeEvent({interactiveHeatmap()},{
    h<-interactiveHeatmap()
    
    l<-list(main = input$main,xlab = input$xlab,ylab = input$ylab,
            row_text_angle = input$row_text_angle,
            column_text_angle = input$column_text_angle,
            dendrogram = input$dendrogram,
            branches_lwd = input$branches_lwd,
            seriate = input$seriation,
            colors=paste0(input$pal,'(',input$ncol,')'),
            distfun_row =  input$distFun_row,
            hclustfun_row = input$hclustFun_row,
            distfun_col = input$distFun_col,
            hclustfun_col = input$hclustFun_col,
            k_col = input$c, 
            k_row = input$r,
            limits = paste(c(input$colorRng_min, input$colorRng_max),collapse=',')
    )
    
    #l=l[!l=='']
    l=data.frame(Parameter=names(l),Value=do.call('rbind',l),row.names = NULL,stringsAsFactors = F)
    l[which(l$Value==''),2]='NULL'
    paramTbl=print(xtable::xtable(l),type = 'html',include.rownames=FALSE,print.results = F,html.table.attributes = c('border=0'))
    
    
    h$width='100%'
    h$height='800px'
    s<-tags$div(style="position: relative; bottom: 5px;",
                HTML(paramTbl),
                tags$em('This heatmap visualization was created using',
                        tags$a(href="https://github.com/yonicd/shinyHeatmaply/",target="_blank",'shinyHeatmaply'),
                        Sys.time()
                )
    )
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("heatmaply-", gsub(' ','_',Sys.time()), ".html", sep="")
      },
      content = function(file) {
        libdir <- paste(tools::file_path_sans_ext(basename(file)),"_files", sep = "")
        
        htmltools::save_html(htmltools::browsable(htmltools::tagList(h,s)),file=file,libdir = libdir)
        if (!htmlwidgets:::pandoc_available()) {
          stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
               "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
        }
        
        htmlwidgets:::pandoc_self_contained_html(file, file)
        
        unlink(libdir, recursive = TRUE)
      }
    )
  })
  
  #################################################REPORTS####################################################################
  
  
  
  # output$downloadReport <- downloadHandler(
  #   filename = function() {
  #     paste('my-report', sep = '.', switch(
  #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #     ))
  #   }
  # )
  
  output$download_table <- downloadHandler(
    filename = function(){"thename.csv"},
    content = function(fname){
      write.csv(my_table, fname)
    }
  )
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      # browser()
      params <- list(n = input$slider, variant = input$variant_family)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
}