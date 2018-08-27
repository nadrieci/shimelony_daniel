GLOBAL:
plot_all <- function(type_graph, numerical){
  
  if(type_graph == "Stacked"){
    plot(rnorm(numerical,2,1))
    
  }

  else{plot(iris$Sepal.Length)}
}

SERVER:
tabIndex <- reactiveVal(0)
observeEvent(input$newTab, {
  tabIndex(tabIndex() + 1)
  appendTab("myTabs", tabPanel(paste("Plot",tabIndex()),
                               
                               tagList(   
                                 
                                 numericInput("numerical", value = 5, min = 1, max=6),

                                 output[[paste("plot", tabIndex(), sep="_")]] <- renderPlot({

                                   plot_all(input[[paste("type_graph",tabIndex(), sep="_")]],
                                            input[[paste("numerical",tabIndex(), sep="_")]])
                                 })
                               ), select=TRUE
  ))
})

observeEvent(input$removeTab, {
  removeTab("myTabs", target=input$myTabs)
})


UI

mainPanel(

             fluidRow(
               actionLink("newTab", "Append tab"),
               actionLink("removeTab", "Remove current tab")
             ),
             tabsetPanel(id="myTabs", type="pills")
) 