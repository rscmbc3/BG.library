
plotLine_ly<-function(data,  scatterOnly = FALSE,pointSize = 10,
                      numberDays = NA, startDate = NA, endDate = NA,
                      startTime = "00:00", endTime = "23:00",
                      colorPalleteDaily = "rainbow", 
                      addSensor = TRUE, addBG = TRUE, 
                      addPercentBG = c("low","good","high","very high"),
                      addBolusType = "Bolus.Volume.Delivered..U.",
                      plotSummary = "Sensor.Glucose..mg.dL.",
                      addSetting ="",settingOverlay = FALSE,percentSetting = 30,
                      barSubPlot = TRUE,addBarSub, percentBar = 30,addPercentType = "BG.Reading..mg.dL.",
                      filterCond = "",addGoodRange = TRUE,
                      addFasting = TRUE,addFastingAnnot = TRUE,
                      legendInset = -0.2){
  
  #subset data by date and filterCond
  data<-subsetData(data,numberDays,startDate,endDate,filterCond)
  
  #if filtered data exists
  if(nrow(data)!=0){
    
    
    #format time in decimal hours
    xticks.list<-xTicks(data, basal, startTime,endTime)
    unPackList(lists = list(xticks.list = xticks.list),
               parentObj = list(NA)) 
    
    #get yaxis code string
    yaxisStr.list<-makeYaxes(addBolusType, addSetting,settingOverlay,
                             percentSetting,addBarSub,percentBar)
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
    layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange)

    #initialize plot
    p<-plot_ly(data)
    #add layout
    eval(parse(text = layoutStr))
    
    
    if (plotSummary!="Sensor.Glucose..mg.dL." & addSensor){#daily sensor data
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
      p<-summaryPlot_ly(data, plotSummary, p)
    }
    
    #add bG values
    p<-addBGpoints_ly(data, p, addBG, pointSize)
    
    #addBolusPOints
    p<-addBolusPoints_ly(data, p, addBolusType,pointSize)
    
    #add pump Settings
    p<-addPumpSetting_ly(p,addSetting, settingOverlay, basal,corrFactor,carbRatio,ay.list,
                         legendInset,startTime,endTime,xticks,yaxisStr)
    
    
    #addPercentBG as text 
    p<-addPercentBG_ly(data,p,addPercentBG,addPercentType)
    
    #add addBarSub of carb intake
    p<-barSubPlot_ly(p, data, barSubPlot,ay.list$ayCarb,
                     addBarSub,
                     numberDays, filterCond,
                     startDate, endDate,
                     startTime = "00:00", endTime = "23:00",
                     plotSummary, sumFunc = "length", stackedBar = "",
                     addBG, 
                     addSetting,settingOverlay,percentSetting,
                     legendInset)
    
    #add fasting
    assign("dataFasting",data,envir = .GlobalEnv)
    p<-addFasting_ly(p, data, addFasting,addFastingAnnot)
    
    #plot it
    p 
    
    
    
    
    
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
  
}#end function