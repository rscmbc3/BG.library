shinyPlot<-function(libraryPath, filePath){
  #load functions
  devtools::load_all(libraryPath,recompile = FALSE) 
  
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  #metronic csv data import
  dataImport.list<-dataImport(filePath,libraryPath)
  unPackList(lists = list(dataImport.list = dataImport.list),
             parentObj = list(NA)) 
  data<-allData
  
  #file path to plotList
  plotListFile<-paste0(libraryPath,"/data/plotList")
  
  #load plotList
  load(file = plotListFile)
  
  shinyApp(  ui=shinyUI(
    
    fluidPage(tags$head(
      tags$style("h5{color: red}")),
      titlePanel(
        h1("Rshiny Interactive BG Plots")),
      
      sidebarLayout(
        sidebarPanel(width=4,
                     #plot actionButtons
                     fluidRow(actionButton("goPlot","Generate Plot"),
                       actionButton("goHistory","Generate History Report"),
                              actionButton("goReport","Generate BG Report")),
                     
                     h4("Plot Specifications                     "),
                     br(),
                     #top level user input
                     selectInput("shPlotType","Select Plot Type",
                                 choices = c("scatter","bar","box","heatmap","Saved Plot")),  
                     
                     
                     uiOutput("outputUI"),
                     uiOutput("historyUI")
                     
                     
        ),#end sidebar
        
        mainPanel(width = 8,
                  plotlyOutput("plotOne", width=900,height=900)
        )#end main panel
      )#end sidebar layout
    )#end fluid page
  ),#end shiny ui
  
  
  server=shinyServer(function(input, output,session) {
    #render UIs
    observe({
      if (input$shPlotType=="scatter"){
        output$outputUI<-renderUI({
          scatterUI()
        })
      }else if (input$shPlotType=="box" | input$shPlotType=="bar"){
        output$outputUI<-renderUI({
          boxBarUI(data)
        })
      }else if (input$shPlotType=="heatmap"){
        output$outputUI<-renderUI({
          heatmapUI(data)
        })
      }else{#saved plot
        output$outputUI<-renderUI({
          savedUI(plotList)
        })
      }
      
    })
    
    observe({
      output$historyUI<-renderUI({
        historyUI()
      })
    })
    
    observe({
      #saved plot description  
      output$description <- renderText({
        eval(parse(text = paste0("plotList$",input$plotName,"$description")))
      })  
    })
    
    


    #plot
    p<-eventReactive(input$goPlot,{
      goShinyPlot(input, output, session,
                  libraryPath, filePath,
                  data)
    })   
        #interactive plot
    observe({
          output$plotOne  <- renderPlotly({
        #goShinyPlot(input, output, session,
        #            libraryPath, filePath,
        #            data)
            p()
      })#end renderplot
    }) 
    
    #generate reports
    hs<-eventReactive(input$goHistory,{
      shinyReport(input, output, session,
                  libraryPath, filePath,
                  data, reportType = "history")
    })      
    
    bg<-eventReactive(input$goReport,{
      shinyReport(input, output, session,
                  libraryPath, filePath,
                  data,reportType = "BG")
    })
    
    observe({   
      hs()
      bg()
      
    })
    
    
    
    
    
    
  })#end shiny server
  )#end shiny app
}#end function