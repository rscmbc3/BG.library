#'@title addPumpSetting_ly
#'@description Adds pump settings as line trace to plot_ly interactive plot, either overlaying or subplot \\cr \\cr
#'@param p current plot_ly plot
#'@param addSetting character vector of settings that should be added to current plot
#'@param settingOverlay TRUE/FALSE whether settings should overlay the data or 
#'if FALSE plot settings as subplot below data
#'@param basal data.frame of basal settings
#'@param corrFactor data.frame of correction factors
#'@param carbRatio data.frame of carb ratios
#'@param ay.list list of yaxis specifications for each setting, 
#'settings are given y axis of y3, y4, y5 and ay3, ay4, ay5 respectively
#'@param legendInset numeric value specifying how far below the plot the legend should be placed
#'@param startTime character string of beginning time for plot (typically startTime = "00:00)
#'@param endTime character string of ending time for plot (typically endTime = "23:00)
#'@param xticks numeric vector of tickmarks for the xaxis
#'@param yaxisStr character string for layout of all y axes.
#'@return `p` plot_ly interactive plot with BG values added if addBG

addPumpSetting_ly<-function(p, addSetting,settingOverlay, basal,corrFactor,carbRatio,ay.list,
                            legendInset,startTime,endTime,xticks,yaxisStr){
  #if adding settings to plot
  if(addSetting[1]!=""){
    setColors<-c("green","blue","red")
    
    #unpack ay.list
    unPackList(lists = list(ay.list = ay.list),
               parentObj = list(NA))
    
    #set index for axes
    for (s in 1:length(addSetting)){
      if (s==1){
        i<-3
      }else{
        i<-i+1
      }
    } 
      
      #for each setting to plot 
      for (s in 1:length(addSetting)){
        #set up temporary data
        settingData<-eval(parse(text = addSetting[s]))
        settingData$rate<-settingData[[length(settingData)]]
        
        #format time
        settingData$time2<-as.POSIXlt(settingData$time,format="%H:%M")
        settingData$hours<- settingData$time2$hour + settingData$time2$min/60 
        
        #save index
        if (s==1){
          i<-3
        }else{
          i<-i+1
        }
        
        #set lineText for hover    
        lineText<-paste0("~paste('</br> Time: ',time2,
                         '</br> ",addSetting[s]," :',rate)")
        
        #add line trace
        p <- p %>% add_trace( data = settingData, x = ~hours, y = ~rate, 
                              type = "scatter", 
                              mode = "lines",
                              color = I(setColors[s]),
                              hoverinfo = 'text',
                              text = eval(parse(text = lineText)),
                              name = addSetting[s],
                              yaxis = paste0("y",i))
        
        
      }#for each setting
    }#if add setting
    return(p)
  }#end function