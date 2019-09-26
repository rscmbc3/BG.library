barPlot<-function(data,basal, corrFactor,carbRatio, numberDays, filterCond = "",scatterOnly = FALSE,
                  plotSummary, sumFunc = "length", stackedBar = "",
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
  mainTitle <- paste0(min(data$Date2)," -to- ",max(data$Date2))
  
  if (stackedBar!="insulin"){
  #make generic temp column for plotSummary
  data$temp<-eval(parse(text = paste0("data$",plotSummary)))
  
  data<-data[c("time2","temp")]
  data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
  data<-data[c("time3","temp")]
  }

  legendChar<-character(0)
  legendCol<-character(0)
  legendLty<-numeric(0)
  legendpch<-numeric(0)
  legendFill<-character(0)
  legendBoxCol<-character(0)
  axisLevel<-0
  
  par(xpd = FALSE,mar = margins, new = FALSE)
  
  if(stackedBar=="") {
    if (sumFunc!="length"){
      sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                        sumFunc,"),na.rm = TRUE))")
    }else if (sumFunc=="length"){
      sumString<-paste0("as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(",
                        sumFunc,")))")
      
    }else if(addGoodRange) {
      
    }
    

    data<-eval(parse(text = sumString))
        if(plotSummary=="BG.Reading..mg.dL." & sumFunc!="length"){
      yLim<-c(0,400)
    }else{
      yLim<-c(0,max(data$temp, na.rm = TRUE))}

          barplot(data$temp, 
              names.arg = format(data$time3,"%H:%M"), 
              las = 2, 
              ylim = yLim, col = "light blue",
              main = mainTitle,
              axis.lty = 1)

      
      legendChar<-c(legendChar,paste0(sumFunc,"_",plotSummary))
      legendCol<-c(legendCol, NA)
      legendLty<-c(legendLty,NA)
      legendpch<-c(legendpch, NA)
      legendFill<-c(legendFill, NA)
      legendBoxCol<-c(legendBoxCol,"light blue")
      
  }else if (stackedBar=="BGrange"){#plot BGrange stacked bar
    data$high<-ifelse(data$temp>150,1,0)
    data$good<-ifelse(data$temp>=80 & data$temp<=150,1,0)
    data$low<-ifelse(data$temp<80,1,0)
    data<-data[,names(data)!="temp"]
    data<-as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(sum),na.rm=TRUE))
    orderVector<-c("time3","low","good","high")
    data<-data[,match(names(data),orderVector)]

    barplot(t(data[c("low","good","high")]), las = 2,col=c("blue","white","red"),
            names.arg = format(data$time3,"%H:%M"),
            main = mainTitle)
    
    legendChar<-c(legendChar,c("low","good","high"))
    legendCol<-c(legendCol, rep(NA, 3))
    legendLty<-c(legendLty,rep(NA, 3))
    legendpch<-c(legendpch, rep(NA, 3))
    legendFill<-c(legendFill, rep(NA, 3))
    legendBoxCol<-c(legendBoxCol,c("blue","white","red"))

  }else if (stackedBar=="insulin"){#plot stacked bar of basal, food, correction
    data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
    
    basal$rate<-basal[[length(basal)]]
    data<-merge(data, basal, by.x = "time3", by.y = "time")
    data<-data[c("time3","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.","rate")]
    data<-as.data.frame(data %>% group_by(time3) %>% summarise_all(funs(mean),na.rm=TRUE))
    
    orderVector<-c("time3","rate","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
    data<-data[,match(orderVector, names(data))]

    names(data)[2]<-"basal rate"
    
    barplot(t(data[, names(data)!="time3"]), las = 2,col=c("white","gray","black"),
            names.arg = format(data$time3,"%H:%M"),axes = TRUE,
            main = mainTitle)
    
    legendChar<-c(legendChar,names(data)[names(data)!="time3"])
    legendCol<-c(legendCol, rep(NA, 3))
    legendLty<-c(legendLty,rep(NA, 3))
    legendpch<-c(legendpch, rep(NA, 3))
    legendFill<-c(legendFill, rep(NA, 3))
    legendBoxCol<-c(legendBoxCol,c("white","gray","black"))

  }#end stacked bar

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
           lty =legendLty,pch = legendpch, pt.bg = legendFill,  ncol = 2, 
           fill = legendBoxCol,border = ifelse(!is.na(legendBoxCol),"black","white"))
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}#end func