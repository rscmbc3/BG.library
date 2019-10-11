
plotLine_ly<-function(data,
                      scatterOnly = FALSE,pointSize = 10,
                      numberDays = NA, startDate = NA, endDate = NA,
                      startTime = "00:00", endTime = "23:00",
                      timeStep = "hour",period = 1,fromChange = TRUE,libraryPath,
                      colorPalleteDaily = "rainbow", 
                      addSensor = TRUE, addBG = TRUE, 
                      addPercentBG = c("low","good","high","very high"),
                      addBolusType = "Bolus.Volume.Delivered..U.",
                      plotSummary = "Sensor.Glucose..mg.dL.",
                      addSetting ="",settingOverlay = FALSE,percentSetting = 30,
                      barSubPlot = TRUE,addBarSub, percentBar = 30,addPercentType = "BG.Reading..mg.dL.",
                      filterCond = "",addGoodRange = TRUE,
                      addFasting = TRUE,addFastingAnnot = TRUE,
                      legendInset = -0.2,description = "",descInset = -0.15){
  

  #subset data by date and filterCond
  data<-subsetData(data,numberDays,startDate,endDate,filterCond,timeStep,period, fromChange,libraryPath)

  #subset settings
  pumpSettings.list<-subsetSetting(data,libraryPath)
  unPackList(lists = list(pumpSettings.list = pumpSettings.list),
             parentObj = list(NA)) 
  
  #if filtered data exists
  if(nrow(data)!=0){
    
    
    #format time in decimal hours
    xticks.list<-xTicks(data, startTime,endTime,timeStep,period)
    unPackList(lists = list(xticks.list = xticks.list),
               parentObj = list(NA)) 
    
    #get yaxis code string
    yaxisStr.list<-makeYaxes(addBolusType, addSetting,settingOverlay,
                             percentSetting,addBarSub,percentBar,yTitle = "")
    #print(names(yaxisStr.list))
    unPackList(lists = list(yaxisStr.list = yaxisStr.list),
               parentObj = list(NA)) 
    unPackList(lists = list(ay.list = ay.list),
               parentObj = list(NA))
    ay.list<-yaxisStr.list$ay.list
    

    #get xAxis str
    xaxisStr<-makeXaxis(addSetting, settingOverlay,xDomain)
    
    #make title str
    titleStr<-paste0(min(data$Date2)," -to- ",max(data$Date2))

    ##make layoutstr
    layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange,
                          description = description,descInset = descInset)

    #initialize plot
    p<-plot_ly(data)
    #add layout
    eval(parse(text = layoutStr))
    

    if (plotSummary!="Sensor.Glucose..mg.dL." & addSensor & !scatterOnly){#daily sensor data
        if (is.na(numberDays)){
        numberDays<-as.numeric(max(data$Date2)-min(data$Date2))
      }
      #daily colors
      eval(parse(text = paste0("cl <- ",colorPalleteDaily,"(numberDays)")))
    
      
      for (i in 1:numberDays){
        p <- p %>% add_trace(data=data[data$Date2==unique(data$Date2)[i],], 
                             x=~hours, y=~Sensor.Glucose..mg.dL., mode='lines',
                             type="scatter", color=cl[i], 
                             hoverinfo = 'text',
                             text = ~paste('</br> Date: ',Date2,
                                           '</br> Time: ',time2,
                                           '</br> Sensor value :',Sensor.Glucose..mg.dL.),
                             name = as.character(unique(data$Date2)[i]))
      }
      
      
      
    }else if (!scatterOnly){#min, max, mean #if daily sensor
      p<-summaryLinePlot_ly(data, plotSummary, p)
      
    }
   
    #add bG values
    p<-addBGpoints_ly(data, p,yAxis = 'y', addBG, pointSize)
    
    #addBolusPOints
    p<-addBolusPoints_ly(data, p, addBolusType,pointSize)
    
    #add pump Settings
    p<-addPumpSetting_ly(p,addSetting, settingOverlay, basal,corrFactor,carbRatio,ay.list,
                         legendInset,startTime,endTime,xticks,yaxisStr)
    
    
    #addPercentBG as text 
    p<-addPercentBG_ly(data,p,addPercentBG,addPercentType,fromChange = fromChange,libraryPath = libraryPath)
    
    #add addBarSub of carb intake
    p<-summaryPlot_ly(p, data,
                      barSubPlot,ay.list$ayCarb,
                     addBarSub,
                     numberDays, filterCond,
                     startDate, endDate,
                     startTime, endTime,
                     plotSummary, sumFunc = "length", stackedBar = "",
                     addBG, libraryPath = libraryPath,
                     addSetting,settingOverlay,percentSetting,
                     legendInset)
  

    #add fasting
    p<-addFasting_ly(p, data, addFasting,addFastingAnnot)
    
    #plot it
    p
    
   
    
    
    
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
  
}#end function