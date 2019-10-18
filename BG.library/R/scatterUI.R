scatterUI<-function(){
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
  
  h3("Add data to plot"),
  h4("BG and SG data"),
  
  checkboxInput("addSensor","Add Sensor Datas lines to plot",value = TRUE),
  checkboxInput("scatterOnly","Points Only",value = FALSE),
  
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
  checkboxInput("addBarSub","Add subplot of mean carbs consumed per hour",value = FALSE),
  conditionalPanel(#only  if addBarSub
    condition = "input.addBarSub == TRUE",
    sliderInput("percentBar","Percentage of plot area to dedicate to mean carb subplot (0-100)'",
                min = 0, max = 100, value = 30)),
  
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
  
  conditionalPanel(#only use numberDays and start End dates if not fromChange
    condition = "input.plotSummary!='Sensor.Glucose..mg.dL.' & input.addSensor==1",
    textInput("colorPalleteDaily","Select color pallete for daily sensor lines",value = "rainbow")
  ),
  
  textInput("filterCond","Filter data according to R syntax statement 
          using 'data' as the data.frame name and column names from input file",value = ""),
  
  numericInput("legendInset","Value to place legend below plot (must be negative)",max = 0, value = -0.2),
  textInput("description","Optional plot description to output below plot area",value = ""),
  numericInput("descInset","Value to place plot description below plot area (must be negative)",max = 0, value = -0.15)
  

  ))
  return(ui)

}