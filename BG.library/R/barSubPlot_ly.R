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
                        uniqueDT = TRUE,replaceNAs = TRUE,ignoreNAs = FALSE,
                        addBG = TRUE, pointSize = 10,
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
      data$hours<- data$hour + data$min/60
        dataOrig<-data
       
      basalOrig<-basal
      if (stackedBar!="insulin"){
      data$barplot<-eval(parse(text = paste0("data$",plotSummary)))


      #get uniques
      if (stackedBar=="" & uniqueDT){
        NAMES<-c("Date2","hours","hour","barplot")
        data<-uniqueDateTime(data, NAMES, replaceNAs)
      }

      data<-data[c("hour","barplot")]
      

      #ignoreNAs
     if (ignoreNAs){
        data<-data[!is.na(data$barplot),]
      }
      
      #set initial y axis range
      if(plotSummary %in% c("BG.Reading..mg.dL.","Sensor.Glucose..mg.dL.") & sumFunc!="length"){
        initYrange<-c(0,450)
        
      }else if (sumFunc=="length"){
        rangeData<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(length)))
        initYrange<-c(0,max(rangeData$barplot, na.rm = TRUE))
        }else{
        initYrange<-c(0,max(data$barplot, na.rm = TRUE))
        }
      
      yTitle<-ifelse(sumFunc!="length",paste0(sumFunc,"_",plotSummary),paste0("number_",plotSummary))
     dataFormat<-data

      }else{#stacked insulin
        basal$rate<-basal[[length(basal)]]
        
        #format time
        basal$time2<-as.POSIXlt(basal$time,format="%H:%M")
        basal$hours<- basal$time2$hour + basal$time2$min/60 
        basal$hour<-basal$time2$hour

        #get on hour values only
       basal2<-basal[,c("hours","hour","rate")]
        basal2<-as.data.frame(basal2 %>% group_by(hour) %>% summarise_all(funs(max),na.rm=TRUE))

        data$hours<- data$hour + data$min/60 
        
        #only look at pump data
        data<-data[is.na(data$Sensor.Glucose..mg.dL.),]
       
        #get unique values (max) per Date2 and hours
        NAMES<-c("Date2","hours","hour","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
        data<-uniqueDateTime(data, NAMES, replaceNAs = TRUE)
        
        
        #merge with basal
        data<-merge(data, basal2, by = "hour", all = TRUE)
        data<-data[c("hour","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.","rate")]
        names(data)[1]<-"hour"
        
        
        
        #summarize by mean
        data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(mean),na.rm=TRUE))
        
        orderVector<-c("hour","rate","BWZ.Food.Estimate..U.","BWZ.Correction.Estimate..U.")
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
        yaxisStr.list<-makeYaxesBar(addSetting, settingOverlay, percentSetting,barSubPlot,addBG,
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
        layoutStr<-makeLayout(titleStr,xDomain,xaxisStr,yaxisStr,addGoodRange = FALSE,stackedBar = stackedBar)

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
          p <- p %>% add_trace(data = data, x = ~hour, y = ~barplot,type = 'bar', 
                              name = paste0(sumFunc,"_",plotSummary))
          
          
          
        }else if (stackedBar=="insulin"){
          cls<-c("white","gray","black")
         p <- addStackbar_ly(p,data,cls) 
         
        }else{#stacked bar BG
          data$veryHigh<-ifelse(data$barplot>240,1,0)
          data$high<-ifelse(data$barplot>150 & data$barplot<=240,1,0)
          data$good<-ifelse(data$barplot>=80 & data$barplot<=150,1,0)
          data$low<-ifelse(data$barplot<80,1,0)
          data<-data[,names(data)!="barplot"]
          data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(sum),na.rm=TRUE))
          orderVector<-c("hour","low","good","high","veryHigh")
          data<-data[,match(names(data),orderVector)]
          cls<-c("blue","white","red","darkred")
          p <- addStackbar_ly(p,data,cls) 
          
        }
        #add pump Settings
          p<-addPumpSetting_ly(p,addSetting, settingOverlay, basalOrig,corrFactor,carbRatio,ay.list,
                               legendInset,startTime,endTime,xticks,yaxisStr)
          
          #add bG values
          p<-addBGpoints_ly(data = dataOrig, p,yAxis = 'y7', addBG, pointSize)
        p
        
      }else{#no data for filter
        message("filtered data contains 0 rows")
      }
    
      }#end barplot is main plot
}