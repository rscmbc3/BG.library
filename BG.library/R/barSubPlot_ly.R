#'@title barSubPlot_ly
#'@description Add subplot of mean carb intake per hour as barplot  \\cr \\cr
#'@param data data.frame with BG values in BG.Reading..mg.dL.
#'@param p current plot_ly plot
#'@param barSubPlot TRUE/FALSE whether subplot should be added to main plot
#'@param ayCarb list of y axis specifications for carb barplot
#'@return `p` plot_ly interactive plot


barSubPlot_ly<-function(p, data, barSubPlot = FALSE,ayCarb,
                        addBarSub,
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
      
      #if filtered data exists
      if(nrow(data)!=0){
        
        #format time in decimal hours
        xticks.list<-xTicks(data, basal, startTime,endTime)
        unPackList(lists = list(xticks.list = xticks.list),
                   parentObj = list(NA)) 
        
        #get yaxis code string
        yaxisStr.list<-makeYaxesBar(addBolusType, addSetting,settingOverlay,
                                 percentSetting,addBarSub,percentBar)
        
      }else{#no data for filter
        message("filtered data contains 0 rows")
      }
    
      }#end barplot is main plot
}