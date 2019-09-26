#'@title plotSensor
#'@description Function to plot sensor and pump data, with options to summarize sensor data, and add pump settings. \\cr \\cr
#'@param data the data.frame containing both pump and sensor data to be used for plotting
#'@param numberDays number of days to plot
#'@param colorPalleteDaily charcter string for color pallete used to plot daily sensor data lines
#'@param addBG TRUE/FALSE indicating whether BG values should be plotted
#'@param addBolus TRUE/FALSE indicating whether insulin bolus data should be plotted
#'@param addBolusType vector of character strings for column names of insulin bolus columns to be plotted
#'@param plotSummary character str indicating what values should be plotted as min, mean, max 
#'(i.e."Sensor.Glucose..mg.dL.","BG.Reading..mg.dL.","BWZ.Carb.Input..grams.","Bolus.Volume.Delivered..U.")  
#'Only one column can be summarized.
#'@param addBasal TRUE/FALSE indicating whether basal settings should be plotted
#'@param addCorrfactor TRUE/FALSE indicating whether correction factor settings should be plotted
#'@param addCarbRatio TRUE/FALSE indicating whether carb ratio settings should be plotted

plotLine<-function(data, numberDays, scatterOnly = FALSE,
                     colorPalleteDaily = "rainbow", 
                     addSensor = TRUE, addBG = TRUE, 
                     addBolus = TRUE, addBolusType = "Bolus.Volume.Delivered..U.",
                     plotSummary = "Sensor.Glucose..mg.dL.",
                     addSetting ="",filterCond = "",
                     legendInset = -0.5, margins = c(12,4,2,15)){
 
  data<-data[data$Date2>=max(data$Date2)-numberDays+1,]
  
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  if(nrow(data)!=0){
  #set right axis level
  axisLevel<-0
  
  #initialize legend settings
  legendChar<-character(0)
  legendCol<-character(0)
  legendLty<-numeric(0)
  legendpch<-numeric(0)
  legendFill<-character(0)
  
  if (plotSummary!="Sensor.Glucose..mg.dL." & addSensor){#daily sensor data
    par(xpd = FALSE,mar = margins)
    plot(as.POSIXct(data$time2,format="%H:%M"),data$Sensor.Glucose..mg.dL.,xaxt="n",
         type = "n", ylim = c(0,400), xlab = "",ylab="BG", yaxt = "n",
         main = paste0(min(data$Date2)," -to- ",max(data$Date2)))
    r <- as.POSIXct(round(range(as.POSIXct(data$time2,format="%H:%M")), "hours"))
    axis.POSIXct(1, as.POSIXct(data$time2,format="%H:%M"), at = seq(r[1], r[2], by = "hour"), format="%H:%M", las = 2)
    axis(side = 2, las = 2)
    eval(parse(text = paste0("cl <- ",colorPalleteDaily,"(numberDays)")))
    for (i in 1:numberDays){
      subdata<-data[data$Date2==unique(data$Date2)[i],]
      lines( y =subdata$Sensor.Glucose..mg.dL.,x = as.POSIXct(subdata$time2,format="%H:%M"),col = cl[i], lwd = 2)
    }
    
    legendChar<-c(legendChar,as.character(unique(data$Date2)))
    legendCol<-c(legendCol,rainbow(numberDays))
    legendLty<-c(legendLty,rep(1,numberDays))
    legendpch<-c(legendpch,rep(NA,numberDays))
    legendFill<-c(legendFill,rep(NA,numberDays))
    
  }else if (!scatterOnly){#min, max, mean 

    plotTracker<- summaryPlot(data, plotSummary,margins,
                              legendChar, legendCol, legendFill,legendLty,legendpch)
    unPackList(lists = list(plotTracker = plotTracker),
               parentObj = list(NA)) 

  }else{
    par(xpd = FALSE,mar = margins)
    plot(0,0,xaxt="n",
         type = "n", ylim = c(0,400), xlab = "",ylab="BG", yaxt = "n",
         main = paste0(min(data$Date2)," -to- ",max(data$Date2)))
    r <- as.POSIXct(round(range(as.POSIXct(data$time2,format="%H:%M")), "hours"))
    axis.POSIXct(1, as.POSIXct(data$time2,format="%H:%M"), at = seq(r[1], r[2], by = "hour"), format="%H:%M", las = 2)
    axis(side = 2, las = 2) 

  }
  

  if (addBG){
    plotTracker<- addBGpoints(data,axisLevel,
                              legendChar,legendCol,legendLty,legendpch,legendFill,NA,
                              scatterOnly)
    
    unPackList(lists = list(plotTracker = plotTracker),
               parentObj = list(NA)) 


  }
  if (addBolus){
    #axisLevel<-1
    axisLevel<-axisLevel+3
    maxB<-paste0("data$",addBolusType)
    maxB<-eval(parse(text = paste0("c(",paste0(maxB,collapse = ","),")")))
    maxB<-max(maxB,na.rm=TRUE)
    minB<-min(maxB, na.rm = TRUE)
    cls<-brewer.pal(length(addBolusType),"Set2")
    
    for (b in addBolusType){
      
      cl<-cls[which(addBolusType==b)]
      shape<-c(21, 22, 24)[which(addBolusType==b)]
      
      par(new=TRUE)
      data$tempBolus<-eval(parse(text = paste0("data$",b)))
      plot(y = data$tempBolus,x = as.POSIXct(data$time2,format="%H:%M"), 
           xaxt = "n",yaxt = "n", pch = shape, bg = cl, xlab = "",ylab= "" )
      if (b==addBolusType[1]){
        axis(side = 4,line=axisLevel,tck=-0.05,
             at = seq(0,maxB,0.5), 
             col = cl, col.ticks = cl,las = 2)
        axis(side = 4,line=axisLevel,labels = FALSE,tck=-0.01,
             at = seq(0,maxB,0.1), 
             col = cl, col.ticks = cl,las = 2)
      }
      legendChar<-c(legendChar,b)
      legendCol<-c(legendCol,"black")
      legendFill<-c(legendFill,cl)
      legendLty<-c(legendLty,NA)
      legendpch<-c(legendpch,shape)     
    }
    
  }
  

  if (length(grep("basal",addSetting))==1){
    
    plotTracker<- addPumpSetting(basal, "green", legendName = "basal rate",
                             bigTick = 0.1, littleTick = 0.05,
                             axisLevel = axisLevel,
                             legendChar, legendCol, legendFill,legendLty,legendpch,NA)
    unPackList(lists = list(plotTracker = plotTracker),
               parentObj = list(NA)) 

  }
  
  if (length(grep("corrFactor",addSetting))==1){
    plotTracker<- addPumpSetting(corrFactor, "blue", legendName = "correction factor",
                                 bigTick = 10, littleTick = 5,
                                 axisLevel = axisLevel,
                                 legendChar, legendCol, legendFill,legendLty,legendpch,NA)
    unPackList(lists = list(plotTracker = plotTracker),
               parentObj = list(NA)) 

  }
  if (length(grep("carbRatio",addSetting))==1){
    plotTracker<- addPumpSetting(carbRatio, "red", legendName = "carb ratio",
                                 bigTick = 1, littleTick = 0.5,
                                 axisLevel = axisLevel,
                                 legendChar, legendCol, legendFill,legendLty,legendpch,NA)
    unPackList(lists = list(plotTracker = plotTracker),
               parentObj = list(NA)) 
  }
  
  
  par(xpd = TRUE)

  legend("bottom",inset = c(0,legendInset),legend = legendChar,col = legendCol, lwd = 2, 
         lty =legendLty,pch = legendpch, pt.bg = legendFill,  ncol = 2)
  }else{#no data for filter
    message("filtered data contains 0 rows")
  }
}