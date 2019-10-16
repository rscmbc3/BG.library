#'@title goShinyPlot
#'@description Format shiny input parameters and generate interactive 
#'plot in shiny application\\cr \\cr
#'@param input shiny user inputs as list
#'@param output shiny output as list
#'@param session shiny session info
#'@param libraryPath character string path to BG.library code 
#'@param path character path to directory in which import file is located.
#'@param fileName charcter string csv file name
#'@param data data.frame to be used to generate plots
#'@examples 
#'shinyApp(  ui=shinyUI(
#'fluidPage(
#'  titlePanel(
#'    h1("Rshiny Interactive BG Plots")),
#'  
#'  sidebarLayout(
#'    sidebarPanel(width=6,
#'                 #top level user input
#'                 selectInput("shPlotType","Select Plot Type",
#'                             choices = c("scatter","bar","box","heatmap","Saved Plot"),
#'                             selected = 'bar'),  
#'                 uiOutput("outputUI")
#'    ),#end sidebar
#'    mainPanel(width = 6,
#'              plotlyOutput("plotOne", width=900,height=900)
#'    )#end main panel
#'  )#end sidebar layout
#')#end fluid page
#'),#end shiny ui
#'
#'server=shinyServer(function(input, output,session) {
#'  #render UIs
#'  observe({
#'    if (input$shPlotType=="scatter"){
#'      output$outputUI<-renderUI({
#'        scatterUI()
#'      })
#'    }else if (input$shPlotType=="box" | input$shPlotType=="bar"){
#'      output$outputUI<-renderUI({
#'        boxBarUI(data)
#'      })
#'    }else if (input$shPlotType=="heatmap"){
#'      output$outputUI<-renderUI({
#'        heatmapUI(data)
#'      })
#'    }else{#saved plot
#'      output$outputUI<-renderUI({
#'        savedUI()
#'      })
#'    }
#'  })
#'  
#'  #interactive plot
#'  observe({
#'    output$plotOne  <- renderPlotly({
#'      goShinyPlot(input, output, session,
#'                  libraryPath, path, fileName,
#'                  data)
#'    })#end renderplot
#'  }) 
#'})#end shiny server
#')#end shiny app

goShinyPlot<-function(input, output, session,
                      libraryPath, path, fileName,
                      data){    
  
  #setup shiny inputs for plot params
  shinySetup.list<-setupShinyParams(input, output, session,
                             libraryPath, path, fileName,
                             data)
    unPackList(lists = list(shinySetup.list = shinySetup.list),
               parentObj = list(NA)) 
    
  if (input$shPlotType!="Saved Plot"){
    #execute plot
    execStr<-paste0("suppressWarnings(",plotType,"(",paramStr,"))")    
    eval(parse(text = execStr))

  }else{#saved plot
    changeParam.list = list(fromChange = fromChange,
                            startDate = startDate,
                            endDate = endDate)
    executeSavedPlot(data = data, plotName = plotName, libraryPath = libraryPath,numberDays = numberDays,
                     changeParam.list = changeParam.list)
  }

}