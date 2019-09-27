makeYaxes<-function(addBolusType, addSetting, settingOverlay, percentSetting,barSubPlot,percentBar,yTitle){

  #set yDomain
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
yaxisStr<-paste0("yaxis = list(range = c(0,450), ticks = 'outside', zeroline = FALSE,showline = TRUE,
                               title = '",yTitle, "',
                               domain = c(",paste(yDomain1,collapse = ","),"))")

#add to yaxisstr
if (addBolusType[1]!=""){
  numberAxes<-numberAxes+1
  position<-position - 0.1*(numberAxes-1)
  allPosition<-c(allPosition, position)

  ayInsulin <- list(
    tickfont = list(color = I("black")),
    overlaying = 'y',
    side = 'right',
    title = "Insulin Units" ,
    showline = TRUE,
    anchor = 'free',
    position = position,
    showgrid = FALSE,
    zeroline = FALSE,
   ticks = 'outside',
   domain = yDomain1,
    rangemode =  'tozero')
  
  
  yaxisStr<-paste0(yaxisStr,",",
                   "yaxis2 = ayInsulin")
}#end addBolus for y axis 

#create y axes for pump settings
settingAxis.list<-makeYaxesSetting(addSetting, settingOverlay, addBolusType,barSubPlot,
                                   allPosition, position,numberAxes,yDomain2,yDomain3,yaxisStr)
if (addSetting[1]!=""){
unPackList(lists = list(settingAxis.list = settingAxis.list),
           parentObj = list(NA)) 
unPackList(lists = list(ay.list = ay.list),
           parentObj = list(NA)) 
}

if (barSubPlot){#if add bar
  numberAxes<-numberAxes+1
  position<-position - 0.1*(numberAxes-1)
  allPosition<-c(allPosition, position)
  
  ayCarb <- list(
    tickfont = list(color = I("blue")),
    #overlaying = 'y',
    side = 'right',
    title = "BWZ.Carb.Input..grams." ,
    showline = TRUE,
    anchor = 'free',
    position = position,
    showgrid = FALSE,
    zeroline = TRUE,
    ticks = 'outside',
    domain = yDomain2,
    rangemode =  'tozero')
  
  
  yaxisStr<-paste0(yaxisStr,",",
                   "yaxis6 = ayCarb")
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
