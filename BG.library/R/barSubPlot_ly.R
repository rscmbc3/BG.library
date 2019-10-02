barSubPlot_ly<-function(data, p,addBarSub, timeStep, period){
  if (addBarSub){
  #only look at pump data
  data<-data[is.na(data$Sensor.Glucose..mg.dL.),]
  
  #format data
  NAMES<-c("dateTime","Date2","hours","hour","BWZ.Carb.Input..grams.")
  data<-uniqueDateTime(data, NAMES, replaceNAs = TRUE,timeStep = timeStep, period = period)
  
  #get only relavant columns
  data<-data[,names(data) %in% c("hour","BWZ.Carb.Input..grams.")]
  
  #group-by and mean
  data<-as.data.frame(data %>% group_by(hour) %>% summarise_all(funs(mean),na.rm = TRUE))
  
  #create plot
  p <- p %>% add_bars(data = data, x = ~hour, y = ~BWZ.Carb.Input..grams., 
                      name = "mean BWZ.Carb.Input..grams.",
                      yaxis = "y6")
  
  return(p)
  }else{#not adding plot return p as is
    return(p)
  }
}