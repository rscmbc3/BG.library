boxBarUI<-function(data){
  ui=shinyUI(
    fluidPage(
     
      h3("Date and Time Parameters"),
      checkboxInput("fromChange","Use data from most recent pump settings change to present",value = TRUE),
      
      conditionalPanel(#only use numberDays and start End dates if not fromChange
        condition = "input.fromChange != 1",
        numericInput("numberDays","Number of Days from Latest Date",value = NA),
        dateRangeInput("daterange", "Date range",
                       format = "mm/dd/yy",
                       separator = " - ")
      ),
      
      sliderInput("timeRange","Time Range",min = 0, max = 23, value = c(0,23)),
      fluidRow(selectInput("timeStep","Time Step",choices = c("hour","day"),selected  = "hour"),
               numericInput("period","Time Step length (period)",value = 1)),
      
      h3("Add data to plot"),
      h4("BG and SG data"),
      
      selectInput("stackedBar","Stacked Bar plot column name (blank for regular barplot)",
                  choices = c("","insulin","BG","SG"),selected = ""),
      checkboxInput("uniqueDT","Get only unique dateTime values",value = TRUE),
      
      selectInput("plotSummary2","Data to summarize",
                  choices = c("",names(data)),
                  selected = "Sensor.Glucose..mg.dL."),
      textInput("sumFunc","Summary Function to be applied per time step",value = "mean"),
      
      checkboxInput("replaceNAs","Replace missing values with zeros",value = FALSE),
      checkboxInput("ignoreNAs","Remove rows with missing data",value = TRUE),
      
      checkboxInput("addBG","Add BG values as points to plot",value = FALSE),
      
      h4("Pump Settings"),
      
      
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
      textInput("filterCond","Filter data according to R syntax statement 
          using 'data' as the data.frame name and column names from input file",value = ""),
      
      numericInput("legendInset","Value to place legend below plot (must be negative)",max = 0, value = -0.2),
      textInput("description","Optional plot description to output below plot area",value = ""),
      numericInput("descInset","Value to place plot description below plot area (must be negative)",max = 0, value = -0.15),
      
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
      )
      
            
      
       
    ))
  return(ui)
  
}