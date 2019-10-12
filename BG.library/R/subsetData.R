subsetData<-function(data,numberDays,startDate,endDate,filterCond,
                     startTime, endTime,timeStep,period, 
                     fromChange = TRUE,libraryPath){
  
  
  data<-fromChangeDateRange(data,numberDays,fromChange,startDate, endDate, libraryPath)
    
  
  #apply filter condition
  if (filterCond!=""){
    data<-eval(parse(text = filterCond))
  }
  
   #set time range as decimal hours
  startTimeOrig<-startTime
  endTimeOrig<-endTime
  startTime<-as.POSIXlt(startTime,format="%H:%M")
  startTime<- startTime$hour + startTime$min/60 
  endTime<-as.POSIXlt(endTime,format="%H:%M")
  endTime<- endTime$hour + endTime$min/60
  data$hours<- data$hour + data$minute/60 
  data<-data[which(data$hour>=startTime & data$hour<=endTime),]
  
  #regenerate new time/date columns based on timeStep and period
  data<-setTimeStep(data,startTime = startTimeOrig,endTime= endTimeOrig, timeStep, period)


  
  return(data)
}