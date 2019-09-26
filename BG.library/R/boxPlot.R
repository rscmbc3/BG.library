boxPlot<-function(data,basal, corrFactor,carbRatio, numberDays, filterCond = "",
                  plotSummary, scatterOnly = FALSE,
                  addBG = TRUE, addSetting = c("basal","corrFactor","carbRatio"),subSensorBG = FALSE,
                  legendInset = -0.5, margins = c(12,4,2,15)){
  #subset for numberDays
  data<-data[data$Date2>=max(data$Date2)-numberDays+1,]
  
  
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  if(nrow(data)!=0){
    if (subSensorBG){
      data[is.na(data$BG.Reading..mg.dL.),]$BG.Reading..mg.dL.<-data[is.na(data$BG.Reading..mg.dL.),]$Sensor.Glucose..mg.dL.
    }
    
    dataOrig<-data # for addBG
    mainTitle <- paste0(plotSummary," ",min(data$Date2)," -to- ",max(data$Date2))
    
    #make generic temp column for plotSummary
    data$temp<-eval(parse(text = paste0("data$",plotSummary)))
    
    data<-data[c("time2","temp")]
    data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
    data<-data[c("time3","temp")]
    
    legendChar<-character(0)
    legendCol<-character(0)
    legendLty<-numeric(0)
    legendpch<-numeric(0)
    legendFill<-character(0)
    legendBoxCol<-character(0)
    axisLevel<-0
    
    par(xpd = FALSE,mar = margins, new = FALSE)
    boxplot(temp~format(time3,"%H:%M"), data = data, las = 2, 
            main = mainTitle)
    
    
    #add points
    if (addBG){
      axisLevel<-0
      plotTracker<- addBGpoints(dataOrig,axisLevel,legendChar,legendCol,legendLty,legendpch,legendFill,legendBoxCol,
                                scatterOnly)
      
      unPackList(lists = list(plotTracker = plotTracker),
                 parentObj = list(NA)) 
    }
    
    #add pumpSettings
    if (length(grep("basal",addSetting))==1){
      plotTracker<- addPumpSetting(basal, "green", legendName = "basal rate",
                                   bigTick = 0.1, littleTick = 0.05,
                                   axisLevel,
                                   legendChar, legendCol, legendFill,legendLty,legendpch,legendBoxCol)
      unPackList(lists = list(plotTracker = plotTracker),
                 parentObj = list(NA)) 
    }
    
    if (length(grep("corrFactor",addSetting))==1){
      plotTracker<- addPumpSetting(corrFactor, "blue", legendName = "correction factor",
                                   bigTick = 10, littleTick = 5,
                                   axisLevel,
                                   legendChar, legendCol, legendFill,legendLty,legendpch,legendBoxCol)
      unPackList(lists = list(plotTracker = plotTracker),
                 parentObj = list(NA)) 
    }
    if (length(grep("carbRatio",addSetting))==1){
      plotTracker<- addPumpSetting(carbRatio, "red", legendName = "carb ratio",
                                   bigTick = 1, littleTick = 0.5,
                                   axisLevel,
                                   legendChar, legendCol, legendFill,legendLty,legendpch,legendBoxCol)
      unPackList(lists = list(plotTracker = plotTracker),
                 parentObj = list(NA)) 
    }
    
    par(xpd = TRUE)
    legend("bottom",inset = c(0,legendInset),legend = legendChar,col = legendCol, lwd = 2, 
           lty =legendLty,pch = legendpch, pt.bg = legendFill,  ncol = 2)
    
}else{#no data for filter
  message("filtered data contains 0 rows")
}
}#end func