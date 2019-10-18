heatmapUI<-function(data){
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
      
     # fluidRow(selectInput("timeStep","Time Step",choices = c("hour","day"),selected  = "hour"),
    #           numericInput("period","Time Step length (period)",value = 1)),
      
      h3("Add data to plot"),
      selectInput("plotSummary2","Data to summarize",
                  choices = c("",names(data)),
                  selected = "Sensor.Glucose..mg.dL."),
      textInput("sumFunc","Summary Function to be applied per time step",value = "mean"),
      
      textInput("brks","Set breakpoints for heatmap colors (must not exceed number of colors in pallete)",
                value = "0,50,80,150,240,300,400,500"),
      checkboxInput("addHist","Add histogram of color groups in heatmap", value = TRUE),
      
      checkboxInput("replaceNAs","Replace missing values with zeros",value = FALSE),
      checkboxInput("ignoreNAs","Remove rows with missing data",value = TRUE),
      
      h3("General Plotting Parameters"),
      selectInput("brewerPallete","Select RColorBrewer pallete for heatmap (pallete must have numberColors>= number of breakpoints",
                  choices = rownames(RColorBrewer::brewer.pal.info),
                  selected = "RdBu"),
      checkboxInput("revPallete","Reverse color pallete",value = TRUE),
      textInput("textCol","Color of text in heatmap cells",value = "black"),
      
      textInput("filterCond","Filter data according to R syntax statement 
          using 'data' as the data.frame name and column names from input file",value = ""),
      
      numericInput("legendInset","Value to place legend below plot (must be negative)",max = 0, value = -0.2),
      textInput("description","Optional plot description to output below plot area",value = ""),
      numericInput("descInset","Value to place plot description below plot area (must be negative)",max = 0, value = -0.15)
      
      
    ))
  return(ui)
  
}