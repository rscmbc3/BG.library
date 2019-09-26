makeYaxes<-function(addBolusType, addSetting, settingOverlay, percentSetting,barSubPlot,percentBar){
  #format percentSetting
  percentSetting<-ifelse(percentSetting<=12,20,percentSetting)
  percentBar<-ifelse(percentBar<=12,30,percentBar)
  percentSetting<-percentSetting/100
  percentBar<-percentBar/100
  #set yDomain
  if ((addSetting[1]=="" | (settingOverlay & addSetting[1]!="")) & !barSubPlot){#just setting subplot
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
    }else{
    yDomain1<-c(percentSetting,1)
    yDomain2<-c(0,percentSetting-0.12) 
    yDomain3<-c(0,0)
    }

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

if (addSetting[1]!=""){
  setColors<-c("green","blue","red")
  for (s in 1:length(addSetting)){ 
    numberAxes<-length(addSetting)
     #set index for axes
    if(s==1 & addBolusType[1]!="" & settingOverlay){
      i<-2+1
      numberAxes<-numberAxes+1
    }else if (s==1){
      i<-3
    }else{
      i<-i+1
    }
   
   
   
  if (s==length(addSetting)){
    position<-1
  }else{
    position<-1 - 0.1*(i-2)
  }  
    allPosition<-c(allPosition, position)
  
    if (barSubPlot){
      domainStr<-yDomain3
    }else{
      domainStr<-yDomain2
    }
    ay <- list(
      tickfont = list(color = setColors[s]),
      color = setColors[s],
      overlaying = 'y',
      side = 'right',
      title = addSetting[s] ,
      showgrid = FALSE,
      showline = TRUE,
    # tickprefix = " ",
      ticks = 'outside',
      anchor = 'free',
     position  = position, 
    domain = domainStr,
    zeroline = FALSE)
     # rangemode =  'tozero')

    if (addSetting[s] %in% c("carbRatio","corrFactor")){
      ay$autorange = 'reversed'
    }

    eval(parse(text = paste0("ay",i,"<-ay")))
    
    if (!settingOverlay & i>3){
      eval(parse(text = paste0("ay",i,"$overlaying<-'y3'")))
    }else if (!settingOverlay & i==3){
      ay3<-ay3[!names(ay3)=="overlaying"]
    }
    
    yStr<-paste0("yaxis",i,"=ay",i)
    
    yaxisStr<-paste0(yaxisStr,",",yStr)
    
  }#for each setting
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
#if (settingOverlay){
 xDomain<-min(allPosition) 

#}#else{
#  xDomain<-1
#}

xDomain<-paste0("domain = c(0,",xDomain,"),")


#return axis string and ay objects
yaxisStr.list<-named.list(yaxisStr,ay.list, xDomain)
return(yaxisStr.list)
}
