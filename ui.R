#VERSIoN 1.5


# source("chooser.R")
navbarPage(title = "Shimelony",
           tabPanel("FILTERING",
                    fluidPage(
                      theme = "mystyle.css",
                      shinythemes::themeSelector(),
                      
                      #######################################################FILTERING################################################################################                      
                      
                      
                      useShinyjs(),
                      checkboxInput("foo", "Show panel", TRUE),
                      navlistPanel(
                        ######################################################CHROMOSOME###########################################################################
                        id = "navlistPanel",
                        tabPanel(HTML("<table><tr><td><strong>Position</strong></td>
                                                                <td>
                                                                <div class='help-tip'>
                                                                <p>Filter genes candidates acording to its location acording to chromosome number, starting and ending position or qtl.</p>
                                                                </div></td></tr>
                                                                </table>"),
                                 
                                 sidebarPanel(
                                   
                                   selectizeInput('chr',HTML("<table><tr><td><strong>Chromosome</strong></td>
                                                                <td>
                                                                <div class='help-tip'>
                                                                <p>Choose if you want to filter by all chromosome or a specific one. By default All the chromosomes are selected, but you can pick any chromosome individually or even more than one.</p>
                                                                </div></td></tr>
                                                                </table>"), selected = "ALL", choices = c("ALL",as.character(unique(file$Chr))), multiple = TRUE
                                                  
                                   ),
                                   helpText("Select a chromosome"),
                                   br(),
                                   
                                   textInput("start_chr", HTML("<table><tr><td><strong>Start</strong></td>
                                                                <td>
                                                                <div class='help-tip'>
                                                                <p>Select de minimum starting position of all the genes at their correspondent chromosome. By default the displayed value is the minimum of all the genes</p>
                                                                </div></td></tr>
                                                                </table>"), value = min(file$start),width = 1000),
                                   helpText("Select the minimum","starting position"),
                                   br(),
                                   
                                   # knobInput(
                                   #   inputId = "myKnob",
                                   #   label = "jQuery knob example:",
                                   #   value = 2,
                                   #   min = 1,
                                   #   max = 24,
                                   #   displayPrevious = TRUE,
                                   #   lineCap = "round",
                                   #   fgColor = "#428BCA",
                                   #   inputColor = "#428BCA"
                                   # ),
                                   
                                   textInput("end_chr", HTML("<table><tr><td><strong>End</strong></td>
                                                                <td>
                                                             <div class='help-tip'>
                                                             <p>Select the maximum ending position of all the genes at their correspondent chromosome. By default the displayed value is the maximum of all the genes</p>
                                                             </div></td></tr>
                                                             </table>"), value = max(file$end)),
                                   helpText("Select the maximum", "ending position"),
                                   br(),
                                   
                                   selectizeInput(
                                     'qtl', HTML("<table><tr><td><strong>QTL</strong></td>
                                                                <td>
                                                 <div class='help-tip'>
                                                 <p>Select a quantitative trait locus (QTL). By default All the QTL are selected, but you can pick any QTL individually or even more than one.  </p>
                                                 </div></td></tr>
                                                 </table>"), choices = c("ALL",as.character(unique(file$QTL))),selected = "ALL", multiple = TRUE
                                   ),
                                   helpText("Select a QTL.","If you need", "more info:"),
                                   tags$a(href = "https://es.wikipedia.org/wiki/QTL", "Click Here!"),
                                   br()
                                 )),           
                        ######################################################ANNOTATION##########################################################################
                        
                        tabPanel(HTML("<table><tr><td><strong>Annotation</strong></td>
                                                                <td>
                                                                <div class='help-tip'>
                                                                <p>Annotation comprises the gene and transcript ID, in addition to the functional description, gene ontology, KEGG and related enzymes. Nomenclature derives from a improved assembly of the melon genome (v3.6.1) and a new genome annotation (v4.0) .</p>
                                                                </div></td></tr>
                                                                </table>"),
                                 sidebarPanel(
                                   
                                   #Annotation
                                   selectizeInput(
                                     'geneID', HTML("<table><tr><td><strong>Gene ID</strong></td>
                                                                <td>
                                                    <div class='help-tip'>
                                                    <p>Select the gene ID (v4.0). Each gene has a different ID.  </p>
                                                    </div></td></tr>
                                                    </table>"), choices = c("ALL",as.character(unique(file$Gene_ID))),selected = "ALL", multiple = TRUE
                                     
                                   ),
                                   helpText("Select a gene ID"),
                                   br(),
                                   
                                   selectizeInput(
                                     'transcriptID', HTML("<table><tr><td><strong>Transcript ID</strong></td>
                                                                <td>
                                                    <div class='help-tip'>
                                                    <p>Select the transcript ID. DNA can be transcribed in more than one way. That's why we need to differenciate among them by using a extra number according to the transcript (v4.0). </p>
                                                    </div></td></tr>
                                                    </table>"), 
                                     choices = c("ALL",unique(sapply(as.vector(file$Transcript_ID), function(x) substring(x,nchar(x))))),
                                     #We take one number per transcript (corresponding to the last char of each value of the transcript_ID )  
                                     selected = "ALL", multiple = TRUE
                                     
                                   ),
                                   helpText("Select the number of","the transcript"),
                                   br(),
                                   
                                   selectizeInput(
                                     'fDescription', HTML("<table><tr><td><strong>Functional description</strong></td>
                                                                <td>
                                                    <div class='help-tip'>
                                                    <p>Each gene codes for a molecule that has a function (
it can simply be the rna or it can go further and be the protein). All these functions are set in this panel. By default All the functional description are selected, but you can pick any of them individually or even more than one.   </p>
                                                    </div></td></tr>
                                                    </table>"), 
                                     choices = c("ALL",as.character(unique(file$functional_description))),selected = "ALL", multiple = TRUE
                                     ,size = 10
                                   ),
                                   helpText("Select the function associated to the gene.","If you need", "more info about gene function:"),
                                   tags$a(href = paste("https://en.wikipedia.org/wiki/","Gene", sep=""), "Click Here!"),
                                   br(),
                                   helpText("If you want to search the function, follow the next link:"),
                                   uiOutput("link_fDescription"),
                                   
                                   br(),
                                   selectizeInput(
                                     'exclude_fDescription', HTML("<table><tr><td><strong>Exclude Functional description</strong></td>
                                                                <td>
                                                    <div class='help-tip'>
                                                    <p>Each gene codes for a molecule that has a function (
it can simply be the rna or it can go further and be the protein). All these functions are set in this panel. By default All the functional description are selected, but you can exclude any of them individually or even more than one.   </p>
                                                    </div></td></tr>
                                                    </table>"), 
                                     choices = c("NONE",as.character(unique(file$functional_description))),selected = "NONE", multiple = TRUE
                                     ,size = 10
                                   ),
                                   
                                   selectizeInput(
                                     'GO',  HTML("<table><tr><td><strong>Gene Ontology</strong></td>
                                                                <td>
                                                 <div class='help-tip'>
                                                 <p>The Gene Ontology (GO) provides a comprehensive resource currently available for computable knowledge regarding the functions of genes and gene products.
                                                    By default All the GO are selected, but you can pick any of them individually or even more than one.
                                                 </p>
                                                 </div></td></tr>
                                                 </table>"),
                                     choices = c("ALL",unique(sort(gsub(" ","", unlist(strsplit(as.vector(file$GO), ",")))))),
                                     selected = "ALL", multiple = TRUE
                                   ),
                                   helpText("Select the GO."),
                                   uiOutput("link_GO"),
                                   br(),
                                   
                                   selectizeInput(
                                     'kegg', HTML("<table><tr><td><strong>Kegg</strong></td>
                                                                <td>
                                                  <div class='help-tip'>
                                                  <p>KEGG is a database resource for understanding high-level functions and utilities of the biological system, such as the cell, the organism and the ecosystem, from molecular-level information, especially large-scale molecular datasets generated by genome sequencing and other high-throughput experimental technologies.
                                                    By default All the GO are selected, but you can pick any of them individually or even more than one.
                                                  </p>
                                                  </div></td></tr>
                                                  </table>"),
                                     choices = c("ALL","NONE"),
                                     selected = "ALL", multiple = TRUE
                                   ),
                                   helpText("Select the KEGG."),
                                   br(),
                                   
                                   selectizeInput(
                                     'enzimes', HTML("<table><tr><td><strong>Enzimes</strong></td>
                                                                <td>
                                                     <div class='help-tip'>
                                                     <p>
                                                     </p>
                                                     </div></td></tr>
                                                     </table>"),
                                     choices = c("ALL","NONE"),
                                     selected = "ALL", multiple = TRUE
                                   ),
                                   helpText("Select the enzimes"),
                                   br() 
                                   
                                 )), 
                        
                        
                        
                        #######################################################VARIANT#######################################################################
                        
                        tabPanel(HTML("<table><tr><td><strong>Variants</strong></td>
                                                                <td>
                                      <div class='help-tip'>
                                      <p> An alteration in the most common DNA nucleotide sequence. The term variant can be used to describe an alteration that may be benign, pathogenic, or of unknown significance.  
                                      </p>
                                      </div></td></tr>
                                      </table>"),
                                 sidebarPanel(
                                   
                                   radioButtons("variant_family", HTML("<table><tr><td><strong>Variant family</strong></td>
                                                                <td>
                                                                       <div class='help-tip'>
                                                                       <p> SCPS comprises the offspring variants of Songwham charmi (SC) and Piel de sapo (PS) parentals, whilst VEDPS contains the Vedrantais (VED) and also Piel de sapo (PS).
                                                                          Radio buttons allow to swap between those two families and only the columns belonging to the selected one will be shown.
                                                                       </p>
                                                                       </div></td></tr>
                                                                       </table>"), c("SCPS", "VEDPS")),
                                   
                                   hr(),
                                   
                                   selectizeInput('v_effect', HTML("<table><tr><td><strong>Variant effect</strong></td>
                                                                <td>
                                                                   <div class='help-tip'>
                                                                   <p>The effect takes into account the impact of each variant in the phenotype. If 'moderate' or 'high' options are choosen, a provean panel will appear. 
                                                                   </p>
                                                                   </div></td></tr>
                                                                   </table>"),
                                                  choices = c("ANY",colnames(file)[which(colnames(file)=="MODIFIER"):(which(colnames(file)=="MODIFIER")+3)]),
                                                  selected = "ANY",multiple = TRUE, width = 1000),
                                   
                                   numericInput("threshold_v_effect", HTML("<table><tr><td><strong>Threshold</strong></td>
                                                                <td>
                                                                   <div class='help-tip'>
                                                                   <p> Genes with a variant effect value greater than the threshold will be displayed. 
                                                                   </p>
                                                                   </div></td></tr>
                                                                   </table>"), 0, min = 0,
                                                max=max(file[,colnames(file)[which(colnames(file)=="MODIFIER"):(which(colnames(file)=="MODIFIER")+3)]])),
                                   
                                   hr(),
                                   
                                   selectizeInput('v_consequence', HTML("<table><tr><td><strong>Variant consequence</strong></td>
                                                                <td>
                                                                        <div class='help-tip'>
                                                                        <p>The consequence has to do with the changes derived from the variant. 
                                                                        </div></td></tr>
                                                                        </table>"),
                                                  choices = c("ANY", colnames(file)[(which(colnames(file)=="X3_prime_UTR")):(which(colnames(file)=="X3_prime_UTR")+19)]),
                                                  selected = "ANY", multiple= TRUE, width = 1000),
                                   
                                   numericInput("threshold_v_consequence",  HTML("<table><tr><td><strong>Threshold</strong></td>
                                                                <td>
                                                                                 <div class='help-tip'>
                                                                                 <p> Genes with a variant consequence value greater than the threshold will be displayed. 
                                                                                 </p>
                                                                                 </div></td></tr>
                                                                                 </table>"), 0, min = 0,
                                                max=max(file[,colnames(file)[(which(colnames(file)=="X3_prime_UTR")):(which(colnames(file)=="X3_prime_UTR")+19)]])),
                                   
                                   hr(),
                                   
                                   conditionalPanel(condition="input.v_effect.indexOf('HIGH') != -1 || input.v_effect.indexOf('MODERATE') != -1" ,
                                                    selectInput('v_provean', HTML("<table><tr><td><strong>Variant provean</strong></td>
                                                                <td>
                                                                                  <div class='help-tip'>
                                                                                  <p> Variant provean predicts whether an amino acid substitution or indel has an impact on the biological function of a protein.
                                                                                  </p>
                                                                                  </div></td></tr>
                                                                                  </table>"), 
                                                                choices = c("ANY","N_DELETERIOUS...2.","N_TOLERATED....2."), multiple = TRUE, selected ="ANY",
                                                                width = 1000),
                                                    
                                                    numericInput("threshold_v_provean", HTML("<table><tr><td><strong>Threshold</strong></td>
                                                                <td>
                                                                                             <div class='help-tip'>
                                                                                             <p> Genes with a variant provean value greater than the threshold will be displayed. 
                                                                                             </p>
                                                                                             </div></td></tr>
                                                                                             </table>"), 0, min = 0,
                                                                 max=max(file[,colnames(file)[(which(colnames(file)=="N_DELETERIOUS...2.")):(which(colnames(file)=="N_DELETERIOUS...2.")+1)]]))                                
                                                    
                                   )
                                   
                                   
                                   
                                 )),                            
                        
                        #################################################EXPRESSION####################################################################
                        
                        
                        tabPanel(HTML("<table><tr><td><strong>Expression</strong></td>
                                                                <td>
                                                                                  <div class='help-tip'>
                                                                                  <p> Gene expression is the process by which information from a gene is used in the synthesis of a functional gene product.
                                                                                     This expression can be quantified. By clicking on each organ expression checkbox a new panel containing a broken down list of their respective gene will be displayed.
Expressions with a value greater than the threshold will be displayed.If some broken down options have been choosen, this threshold will be applied to them instead of the mean
                                                                                  </p>
                                                                                  </div></td></tr>
                                                                                  </table>"),
                                 sidebarPanel(
                                   #checkboxGroupInput will create a group of checkboxes that can be used to toggle multiple choices independently.
                                   #The large code in selected is due to the columns may change but they relative position not, so we take from
                                   #the position where "Flower" is up to seven positions more.
                                   titlePanel(fluidRow(
                                     column(7, titlePanel( "Detailed")),
                                     column(5, titlePanel( "Expressions"))
                                   )),
                                   
                                   #splitLayout allows to set on one hand, a detailed drop-down list and on the other hand a minimum threshold
                                   splitLayout(
                                     checkboxInput("flower_detailed", "Flower"), numericInput("flower_threshold", NULL, 0 , min = 0, max = max(flower_values))),
                                   splitLayout(
                                     checkboxInput("fruit_detailed", "Fruit"), numericInput("fruit_threshold", NULL, 0, min = 0, max = max(fruit_values))),
                                   splitLayout(
                                     checkboxInput("leaf_detailed", "Leaf"), numericInput("leaf_threshold", NULL, 0, min = 0, max = max(leaf_values))),
                                   splitLayout(
                                     checkboxInput("ovary_detailed", "Ovary"), numericInput("ovary_threshold", NULL, 0, min = 0, max = max(ovary_values))),
                                   splitLayout(
                                     checkboxInput("root_detailed", "Root"), numericInput("root_threshold", NULL, 0, min = 0, max = max(root_values))),
                                   splitLayout(
                                     checkboxInput("seed_detailed", "Seed"), numericInput("seed_threshold", NULL, 0, min = 0, max = max(seed_values))),
                                   splitLayout(
                                     checkboxInput("som_embryo_detailed", "Somatic embryogenesis"), numericInput("som_embryo_threshold", NULL, 0, min = 0, max = 5000)),
                                   splitLayout(
                                     checkboxInput("stem_detailed", "Stem"), numericInput("stem_threshold", NULL, 0, min = 0, max = max(stem_values))),
                                   splitLayout(
                                     checkboxInput("tendril_detailed", "Tendril"), numericInput("tendril_threshold", NULL, 0, min = 0, max = max(tendril_values))),
                                   
                                   
                                   uiOutput("expression_bottom")
                                   
                                   
                                 )),
                        #################################################ORTHOLOGS####################################################################
                        
                        tabPanel(HTML("<table><tr><td><strong>Orthologs</strong></td>
                                                                <td>
                                      <div class='help-tip'>
                                      <p> Orthologs are genes in different species that evolved from a common ancestral gene by speciation. Normally, orthologs retain the same function in the course of evolution. Identification of orthologs is critical for reliable prediction of gene function in newly sequenced genomes. 
                                      </p>
                                      </div></td></tr>
                                      </table>"),
                                 sidebarPanel(
                                   selectizeInput(
                                     'ath', HTML("<table><tr><td><strong>A. thaliana</strong></td>
                                                                <td>
                                                 <div class='help-tip'>
                                                 <p> Orthologs genes from Arabidopsis thaliana. 
                                                 </p>
                                                 </div></td></tr>
                                                 </table>"),
                                     choices = c("ALL",unique(sort(gsub(" ","", unlist(strsplit(as.vector(file$Ath_Orthologs), ",")))))),
                                     selected = "ALL", multiple = TRUE
                                   ),
                                   selectizeInput(
                                     'tom', HTML("<table><tr><td><strong>S. lycopersicum</strong></td>
                                                                <td>
                                                 <div class='help-tip'>
                                                 <p> Orthologs genes from Solanum lycopersicum. 
                                                 </p>
                                                 </div></td></tr>
                                                 </table>"),
                                     choices = c("ALL",unique(sort(gsub(" ","", unlist(strsplit(as.vector(file$Tom_Orthologs), ",")))))),
                                     selected = "ALL", multiple = TRUE
                                   )

                                 )),
                        
                        
                        #################################################MAIN PANEL#############################################################
                        
                        mainPanel(
                          
                          actionButton("update", "Update View"),
                          tabsetPanel(
                            
                            tabPanel("Previsualization", 
                                     h3(textOutput("remaining_genes_summary")),
                                     plotOutput("remaining_genes_plot", width = "150%", height = "600px"),
                                     plotOutput("remaining_genes_plot_qtl", width = "200%", height = "700px")
                                     ),
                            tabPanel("Table",     
                                     
                                     DT::dataTableOutput("table")
                                     
                            ),
                            
                            
                            tabPanel("Plot", 
                                     
                                     fluidRow(
                                       actionLink("newTab", "Append tab"),
                                       actionLink("removeTab", "Remove current tab")
                                     ),
                                     tabsetPanel(id="myTabs", type="pills")
                                     
                            )
                            
                          )
                          
                        ) 
                        ,widths = c(12,8,12)
                      )#END of navListPanel
                    )#END of tabPanel filtering
                    
                    
                    ############################################################HEATMAP#######################################################################################


           )
,
tabPanel("HEATMAP",


         fluidPage(
           sidebarLayout(
             sidebarPanel(width=4,
                          h4('Data Selection'),
                          fileInput(inputId="mydata", label = "Import Data",multiple = T),
                          checkboxInput(inputId = "sintope",label = HTML("<table><tr><td><strong>Uncap genes</strong></td>
                                                                <td>
                                                                         <div class='help-tip'>
                                                                         <p>By default, the maximum number of genes that will be shown is 50. Trying to plot more than that will be very slow. </p>
                                                                         </div></td></tr>
                                                                         </table>"), value = FALSE),
                          uiOutput('data'),
                          checkboxInput('showSample','Subset Data'),
                          conditionalPanel('input.showSample',uiOutput('sample')),
                          hr(),h4('Data Preprocessing'),
                          column(width=4,selectizeInput('transpose','Transpose',choices = c('No'=FALSE,'Yes'=TRUE),selected = FALSE)),
                          column(width=4,selectizeInput("transform_fun", "Transform", c(Identity=".",Sqrt='sqrt',log='log',Scale='scale',Normalize='normalize',Percentize='percentize')
                                                        ,selected = '.')),

                          uiOutput('annoVars'),
                          selectizeInput("transform_fun_correlation", "Correlation", c("None","Pearson", "Spearman")
                          ),

                          br(),hr(),h4('Row dendrogram'),
                          column(width=6,selectizeInput("distFun_row", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
                          column(width=6,selectizeInput("hclustFun_row", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
                          column(width=12,sliderInput("r", "Number of Clusters", min = 1, max = 15, value = 2)),
                          #column(width=4,numericInput("r", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),

                          br(),hr(),h4('Column dendrogram'),
                          column(width=6,selectizeInput("distFun_col", "Distance method", c(Euclidean="euclidean",Maximum='maximum',Manhattan='manhattan',Canberra='canberra',Binary='binary',Minkowski='minkowski'),selected = 'euclidean')),
                          column(width=6,selectizeInput("hclustFun_col", "Clustering linkage", c(Complete= "complete",Single= "single",Average= "average",Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",Ward.D= "ward.D",Ward.D2= "ward.D2"),selected = 'complete')),
                          column(width=12,sliderInput("c", "Number of Clusters", min = 1, max = 15, value = 2)),
                          #column(width=4,numericInput("c", "Number of Clusters", min = 1, max = 20, value = 2, step = 1)),

                          br(),hr(),  h4('Additional Parameters'),




                          hr(),
                          h4('Color Manipulation'),
                          uiOutput('colUI'),
                          sliderInput("ncol", "Set Number of Colors", min = 1, max = 256, value = 256),
                          checkboxInput('colRngAuto','Auto Color Range',value = T),
                          conditionalPanel('!input.colRngAuto',uiOutput('colRng'))
                          ,

                          column(3,checkboxInput('showMargin','Layout')),
                          column(3,checkboxInput('showDendo','Dendrogram')),
                          hr(),

                          conditionalPanel('input.showDendo==1',
                                           hr(),
                                           h4('Dendrogram Manipulation'),
                                           selectInput('dendrogram','Dendrogram Type',choices = c("both", "row", "column", "none"),selected = 'both'),
                                           selectizeInput("seriation", "Seriation", c(OLO="OLO",GW="GW",Mean="mean",None="none"),selected = 'OLO'),
                                           sliderInput('branches_lwd','Dendrogram Branch Width',value = 0.6,min=0,max=5,step = 0.1)
                          ),

                          conditionalPanel('input.showMargin==1',
                                           hr(),
                                           h4('Widget Layout'),
                                           column(4,textInput('main','Title','')),
                                           column(4,textInput('xlab','X Title','')),
                                           column(4,textInput('ylab','Y Title','')),
                                           sliderInput('row_text_angle','Row Text Angle',value = 0,min=0,max=180),
                                           sliderInput('column_text_angle','Column Text Angle',value = 45,min=0,max=180),
                                           sliderInput("l", "Set Margin Width", min = 0, max = 200, value = 130),
                                           sliderInput("b", "Set Margin Height", min = 0, max = 200, value = 40)
                          )
             ),

             mainPanel(
               tabsetPanel(
                 tabPanel("Heatmaply",
                          tags$a(id = 'downloadData', class = paste("btn btn-default shiny-download-link",'mybutton'), href = "", target = "_blank", download = NA, icon("clone"), 'Download Heatmap as HTML'),
                          tags$head(tags$style(".mybutton{color:white;background-color:blue;} .skin-black .sidebar .mybutton{color: green;}") ),
                          plotlyOutput("heatout",height=paste(15*tope,"px",sep=""))
                 ),
                 tabPanel("Heatmap data",
                          DT::dataTableOutput('tables')
                 )
               )
             )
           )
         )





)
####END OF HEATMAP####
,
tabPanel("DOWNLOAD",
         downloadButton("Download_table", "Download table"),
         downloadButton("report", "Generate report")
         
         
)

)