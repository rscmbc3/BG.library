#'@title makeYdomain
#'@description Set up y domains 1,2, and 3 for subplots \\cr \\cr
#'@param percentSetting numeric value (0-100) for percentage of total plot to dedicate to setting subplot
#'@param percentBar numeric value (0-100) for percentage of total plot to dedicate to bar subplot
#'@param addSetting character vector of settings to plot c("basal,"corrFactor","carbRatio")
#'@param settingOverlay TRUE/FALSE whether or not settings overlay main plot
#'@param barSubPlot TRUE/FALSE whether barsubplot is to be included
#'@return `domain.list` named.list(percentSetting, percentBar, yDomain1, yDomain2, yDomain3)


makeYdomain<-function(percentSetting,percentBar,addSetting,settingOverlay,barSubPlot){
  #format percentSubplots
  if (!is.na(percentSetting)){
   percentSetting<-ifelse(percentSetting<=12,20,percentSetting) 
   percentSetting<-percentSetting/100
  }
  if (!is.na(percentBar)){
  percentBar<-ifelse(percentBar<=12,30,percentBar)
  percentBar<-percentBar/100
  }
  
  
  #set yDomain
  if ((addSetting[1]=="" | (settingOverlay & addSetting[1]!="")) & !barSubPlot){#no subplot
    yDomain1<-c(0,1)
    yDomain2<-c(0,1)
    yDomain3<-c(0,0)
  }else if ((addSetting[1]=="" | settingOverlay==TRUE) & barSubPlot){#just bar subplot
    yDomain1<-c(percentBar,1)
    yDomain2<-c(0,percentBar-0.12)
    yDomain3<-c(0,0)
  }else if ((addSetting[1]!="" | settingOverlay==FALSE) & barSubPlot){#bar and setting subplots
    yDomain1<-c(percentSetting+percentBar,1)
    yDomain2<-c(percentSetting-0.12,percentSetting+percentBar-0.12) 
    yDomain3<-c(0,percentSetting-0.12) 
  }else{#just setting subplot
    yDomain1<-c(percentSetting,1)
    yDomain2<-c(0,percentSetting-0.12) 
    yDomain3<-c(0,0)
  }
  
  domain.list<-named.list(percentSetting, percentBar, yDomain1, yDomain2, yDomain3)
  return(domain.list)
}