shinyPlot2<-function(libraryPath, path, fileName){
  #load functions
  devtools::load_all(libraryPath,recompile = FALSE) 
  
  #get pumpSettings
  pumpSettings.list<-makePumpSettings(libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  #metronic csv data import
  dataImport.list<-dataImport(path,fileName,libraryPath)
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
        sidebarPanel(width=6,
                     #plot actionButtons
                     fluidRow(actionButton("goPlot","Generate Plot"),
                              actionButton("goHistory","Generate History Report"),
                              actionButton("goReport","Generate BG Report")),
                     
                     h4("Plot Specifications                     "),
                     br(),
                     #top level user input
                     selectInput("shPlotType","Select Plot Type",
                                 choices = c("scatter","bar","box","heatmap","Saved Plot")),  
                     
                     
                       uiOutput("outputUI")
                     
                     
        ),#end sidebar
        
        mainPanel(width = 6,
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
        savedUI()
      })
  }
      })
  
    #saved plot description  
  output$description <- renderText({
      eval(parse(text = paste0("plotList$",input$plotName,"$description")))
    })  
  
  #interactive plot
    output$plotOne  <- renderPlotly({
      p<-eventReactive(input$goPlot, {
        goShinyPlot(input, output, session,
                    libraryPath, path, fileName,
                    data)
      })()#end render plot
    })#end renderplot
    
    #history sequence output
    observe({
      p<-eventReactive(input$goHistory,{
        shinyReport(input, output, session,
                        libraryPath, path, fileName,
                        data, reportType = "history")
      })()#end go history plots
    })
    
    #BG report output
    observe({
      p<-eventReactive(input$goReport,{
        shinyReport(input, output, session,
                        libraryPath, path, fileName,
                        data,reportType = "BG")
      })()#end go history plots
    })

    
  })#end shiny server
  )#end shiny app
}#end function