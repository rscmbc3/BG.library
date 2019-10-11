shinyPlot<-function(libraryPath, path, fileName){
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
                     h4("Plot Specifications                     "),
                     br(),
                     
                     #top level user input
                     selectInput("shPlotType","Select Plot Type",
                                 choices = c("scatter","bar","box","heatmap","Saved Plot")),
                     
                     conditionalPanel(#for plotLine_ly
                       condition = "input.shPlotType == 'scatter'",
                       
                       h3("Date and Time Parameters"),
                       checkboxInput("fromChange","Use data from most recent pump settings change to present",value = TRUE),
                       
                       conditionalPanel(#only use numberDays and start End dates if not fromChange
                         condition = "input.fromChange != 1",
                         numericInput("numberDays","Number of Days from Latest Date",value = NA),
                         dateRangeInput("daterange", "Date range",
                                        format = "mm/dd/yy",
                                        separator = " - ")),
                       
                       sliderInput("timeRange","Time Range",min = 0, max = 23, value = c(0,23)),
                       #fluidRow(selectInput("timeStep","Time Step",choices = c("hour","day"),selected  = "hour"),
                      #          numericInput("period","Time Step length (period)",value = 1)),
                       
                       h3("Add data to plot"),
                       h4("BG and SG data"),
                       fluidRow(checkboxInput("addSensor","Add Sensor Datas lines to plot",value = TRUE),
                                checkboxInput("scatterOnly","Points Only",value = FALSE)
                       ),
                       selectInput("plotSummary","Data type to summarize by min, mean, max lines (leave blank for daily sensor lines)",
                                   choices = c("","Sensor.Glucose..mg.dL.","BG.Reading..mg.dL."),
                                   selected = "Sensor.Glucose..mg.dL."),
                       checkboxInput("addBG","Add BG values as points to plot",value = TRUE),
                       
                       checkboxInput("addFasting","Add line indicating mean fasting BG value", value = TRUE),
                       checkboxInput("addFastingAnnot","Add arrow and text to plot indicating mean fasting BG line",value = TRUE),
                       
                       
                       
                        checkboxGroupInput("addPercentBG", "Add grouped BG or SG percentages to plot as text", 
                                                         choices =  c("low","good","high","very high"),
                                                         selected = c("low","good","high","very high"),
                                                         inline=TRUE),
                       
                       selectInput("addPercentType","Type of reading used to get summary percentages per group (either BG or SG)",
                                   choices = c("" ,"Sensor.Glucose..mg.dL.","BG.Reading..mg.dL."), selected = "BG.Reading..mg.dL."),
                       checkboxInput("addGoodRange","Add colored bar indicating good BG range <80 and >150",value = TRUE),
                       
                       
                       h4("Bolus and Carb Data"),
                       selectInput("addBolusType","Type of insulin delivery points to add",
                                   choices  = c("","Bolus.Volume.Delivered..U.","BWZ.Correction.Estimate..U.","BWZ.Food.Estimate..U.")),
                       #checkboxInput("barSubPlot","Barplot subplot of mean carbs consumed per hour",value = FALSE),
                       #barSubPlot should be set as variable TRUE if plotType is 'scatter' and addBarSub = TRUE
                       checkboxInput("addBarSub","Add subplot of mean carbs consumed per hour",value = FALSE),
                       conditionalPanel(#only  if addBarSub
                         condition = "input.addBarSub == TRUE",
                         sliderInput("percentBar","Percentage of plot area to dedicate to mean carb subplot (0-100)'",
                                     min = 0, max = 100, value = 30)),
                       
                       
                       
                       h4("Pump Settings"),
                       #selectInput("addSetting","Add Pump Settings to plot",
                       #             choices = c("","basal","corrFactor","carbRatio")),
                       

                                      checkboxGroupInput("addSetting", "Add Pump Settings", 
                                                         choices =  c("basal","corrFactor","carbRatio"),
                                                         selected = c("basal","corrFactor","carbRatio"),
                                                         inline=TRUE),
                       conditionalPanel(#only use numberDays and start End dates if not fromChange
                         condition = "input.addSetting.includes('basal') || 
                         input.addSetting.includes('corrFactor') ||
                         input.addSetting.includes('carbRatio')",
                       checkboxInput("settingOverlay","Overlay pump settings on top of data plot 
              (if blank settings will plot as subplot below main plot)",value = FALSE),
                       sliderInput("percentSetting","Percentage of plot area to dedicate to setting subplot (0-100)",
                                   min = 0, max = 100,value = 30)
                       ),
                       
                       
                       
                       h3("General Plotting Parameters"),
                       numericInput("pointSize","Point Size",value = 10, min = 1),
                       
                       conditionalPanel(#only use numberDays and start End dates if not fromChange
                         condition = "input.plotSummary!='Sensor.Glucose..mg.dL.' & input.addSensor==1",
                         textInput("colorPalleteDaily","Select color pallete for daily sensor lines",value = "rainbow")),
                       
                       textInput("filterCond","Filter data according to R syntax statement 
          using 'data' as the data.frame name and column names from input file",value = ""),
                       
                       numericInput("legendInset","Value to place legend below plot (must be negative)",max = 0, value = -0.2),
                       textInput("description","Optional plot description to output below plot area",value = ""),
                       numericInput("descInset","Value to place plot description below plot area (must be negative)",max = 0, value = -0.15)
                      ),#end conditional panel for plotLine_ly
                      
                    #saved plots
                      conditionalPanel(#for plotLine_ly
                       condition = "input.shPlotType == 'Saved Plot'",
                      selectInput("plotName","Select saved plot by name",choices = names(plotList)),
                      textOutput("description")
                       ),
                       
                     
                     #historytSeq
                      h3("Historical Sequence Plot Parameters"),
                      checkboxInput("outPutHistorySeq","Output a sequence of historical plots as html",value = TRUE),
                      conditionalPanel(
                        condition = "input.outPutHistorySeq==1",
                        textInput("reportTitle","Title for output html document"),
                        textInput("outPath","Path to directory in which to output html document"),
                        h6("Blank will output file to ~BG.library/reports"),
                        textInput("outFileName","Name of output file"),
                        selectInput("seqType","Type of historical sequence to output",choices = c("change","days"),
                                    selected = "change"),
                        numericInput("seqLength", "Number of repetitions of historical sequence plots", value = 2)
                      ),
                     
                      
                     
                     #plot actionButtons
                     fluidRow(actionButton("goPlot","Generate Plot"),
                              actionButton("goHistory","Generate History Report"))
                     
                     
        ),#end sidebar
        
        mainPanel(width = 6,
                  plotlyOutput("plotOne", width=900,height=900)
        )#end main panel
      )#end sidebar layout
    )#end fluid page
  ),#end shiny ui
  
  
  server=shinyServer(function(input, output,session) {
    
    
    output$plotOne  <- renderPlotly({
      p<-eventReactive(input$goPlot, {
        goShinyPlot(input, output, session,
                    libraryPath, path, fileName,
                    data)
      })()#end render plot
    })#end renderplot
    
    observeEvent(input$goHistory,{
      shinyHistorySeq(input, output, session,
                                libraryPath, path, fileName,
                                data)
      })#end go history plots
    
    output$description <- renderText({
      eval(parse(text = paste0("plotList$",input$plotName,"$description")))
    })
    
  })#end shiny server
  )#end shiny app
}#end function