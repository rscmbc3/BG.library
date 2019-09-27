#'@title barSubPlot_ly
#'@description Add subplot of mean carb intake per hour as barplot  \\cr \\cr
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param p current plot_ly plot
#'@param barSubPlot TRUE/FALSE whether subplot should be added to main plot
#'@param ayCarb list of y axis specifications for carb barplot
#'@return `p` plot_ly interactive plot


barSubPlot_ly<-function(p, data, barSubPlot = FALSE,ayCarb,
                        addBarSub,basal,
                        numberDays, filterCond = "",
                        startDate = NA, endDate = NA,
                        startTime = "00:00", endTime = "23:00",
                        plotSummary, sumFunc = "length", stackedBar = "",
                        addBG = TRUE, 
                        addSetting = "",settingOverlay = FALSE,percentSetting = 30,
                        legendInset = -0.2){
  if (barSubPlot & addBarSub){#barplot is not main plot and add subplot
    
    #format data
      data<-data[,names(data) %in% c("hours","BWZ.Carb.Input..grams.")]
      data$hours<-round(data$hours)
      data$BWZ.Carb.Input..grams.<-as.numeric(ifelse(data$BWZ.Carb.Input..grams.==0,NA,data$BWZ.Carb.Input..grams.))
      data<-as.data.frame(data %>% group_by(hours) %>% summarise_all(funs(mean),na.rm = TRUE))

      #create plot
      p <- p %>% add_bars(data = data, x = ~hours, y = ~BWZ.Carb.Input..grams., 
                          name = "mean BWZ.Carb.Input..grams.",
                          yaxis = "y6")

  return(p)
      }else if (barSubPlot){#barplot is not main plot
        return(p)
    }else{#barplot is main plot
      #subset data by date and filterCond
      data<-subsetData(data,numberDays,startDate,endDate,filterCond)
      #data$time3<-as.POSIXct(round(as.POSIXct(data$time2,format="%H:%M"),"hours"))
      
      #if filtered data exists
      if(nrow(data)!=0){
      #save data for xticks
      dataOrig<-data
      
      if (stackedBar!="insulin"){
      data$barplot<-eval(parse(text = paste0("data$",plotSummary)))
      data<-data[!is.na(data$barplot),]
      #data<-data[c("time3","barplot")]
      data<-data[c("hour","barplot")]
      
      #set initial y axis range
      if(plotSummary=="BG.Reading..mg.dL." & sumFunc!="length"){
        initYrange<-c(0,450)
        
      }else{
        initYrange<-c(0,max(data$barplot, na.rm = TRUE))
        }
      yTitle<-plotSummary
     dataFormat<-data
      }else{#stacked insulin
        basal$rate<-basal[[length(basal)]]
        
        #format time
        #basal$time2<-as.POSIXlt(basal$time,format="%H:%M")
        basal$hours<- basal$time$hour + basal$time$min/60 
        
        data<-merge(data, basal, by = "hours")
        data<-data[c("hours","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.","rate")]
        data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(mean),na.rm=TRUE))
        
        orderVector<-c("hours","rate","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
        data<-data[,match(orderVector, names(data))]
        
        names(data)[2]<-"basal rate"
        
        #set initial y axis range
        data$totalInsulin<-apply(data[-c(1)],1, function(x) sum(x,na.rm = TRUE))
        initYrange<-c(0,max(data$totalInsulin, na.rm = TRUE))
        data<-data[-c(length(data))]
        yTitle<-"Insulin Units Delivered"
        
        dataFormat<-data
      }
      

      
        #format time in decimal hours
        xticks.list<-xTicks(data = dataOrig, basal, startTime,endTime)
        unPackList(lists = list(xticks.list = xticks.list),
                   parentObj = list(NA)) 
        
        #get yaxis code string
        yaxisStr.list<-makeYaxesBar(addSetting, settingOverlay, percentSetting,barSubPlot,
                                              initYrange,yTitle)
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
        layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange = FALSE)
        
        #initialize plot
        p<-plot_ly()
        #add layout
        eval(parse(text = layoutStr))
        #get formatted data
        data<-dataFormat
        
        if(stackedBar=="") {#general bar plot
          
          #set up summary function string
          if (sumFunc!="length"){
            sumString<-paste0("as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(",
                              sumFunc,"),na.rm = TRUE))")
          }else if (sumFunc=="length"){
            sumString<-paste0("as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(",
                              sumFunc,")))")
            
          }#end sumString

          #apply sumString
          data<-eval(parse(text = sumString))

          #create plot
          p <- p %>% add_bars(data = data, x = ~hour, y = ~barplot, 
                              name = paste0(sumFunc,"_",plotSummary))
          
          #add pump Settings
          p<-addPumpSetting_ly(p,addSetting, settingOverlay, basal,corrFactor,carbRatio,ay.list,
                               legendInset,startTime,endTime,xticks,yaxisStr)
          
        }else if (stackedBar=="insulin"){
          
        }else{#stacked bar BG
          
        }
        
        p
        
      }else{#no data for filter
        message("filtered data contains 0 rows")
      }
    
      }#end barplot is main plot
}