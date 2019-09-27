#'@title makeYaxesBar
#'@description Set up y axes for barplot as main plot \\cr \\cr
#'@param addSetting character vector of settings to plot c("basal,"corrFactor","carbRatio")
#'@param settingOverlay TRUE/FALSE whether or not settings overlay main plot
#'@param percentSetting numeric value (0-100) for percentage of total plot to dedicate to setting subplot
#'@param barSubPlot TRUE/FALSE whether barsubplot is to be included
#'@param initYrange numeric vector for range of initial y axis on the left
#'@return `yaxisStr.list` named.list(yaxisStr,ay.list, xDomain)


makeYaxesBar<-function(addSetting, settingOverlay, percentSetting,barSubPlot,
                       initYrange,yTitle){
  
  #set yDomain
  percentBar = ""
  addBolusType = ""
  domain.list<-makeYdomain(percentSetting,percentBar,addSetting,settingOverlay,barSubPlot)
  unPackList(lists = list(domain.list = domain.list),
             parentObj = list(NA)) 
  
  #get number axes
  if (addSetting[1]!=""){
    numberAxes<-length(addSetting[addSetting!=""])
  }else{
    numberAxes<-1
  }
  
  #set position
  position<-1
  allPosition<-position
  
  #itialize plotly plot y axis
  yaxisStr<-paste0("yaxis = list(range = c(",paste(initYrange, collapse = ","),"), 
                                 ticks = 'outside', zeroline = FALSE,showline = TRUE,
                                 title = '",yTitle, "',
                                domain = c(",paste(yDomain1,collapse = ","),"))")
  
  
  
  #create y axes for pump settings
  settingAxis.list<-makeYaxesSetting(addSetting, settingOverlay, addBolusType,barSubPlot,
                                     allPosition, position,numberAxes,yDomain2,yDomain3,yaxisStr)
  if (addSetting[1]!=""){
  unPackList(lists = list(settingAxis.list = settingAxis.list),
             parentObj = list(NA)) 
  unPackList(lists = list(ay.list = ay.list),
             parentObj = list(NA)) 
  }
  
  #make list of all ay objects
  ay.list<-list()
  for (a in ls()[startsWith(ls(),"ay")]){
    eval(parse(text = paste0("ay.list$",a,"<-",a)))
  }
  
  #set x domain to min(allPOsition)
  xDomain<-min(allPosition) 
  xDomain<-paste0("domain = c(0,",xDomain,"),")
  
  
  #return axis string and ay objects
  yaxisStr.list<-named.list(yaxisStr,ay.list, xDomain)
  return(yaxisStr.list)
}
