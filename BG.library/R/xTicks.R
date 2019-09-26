xTicks<-function(data, basal, startTime,endTime){
  xticks<-unique(format(round(as.POSIXct(basal$time,format="%H:%M"),"hours"),"%H:%M"))
  data$time3<-as.POSIXlt(data$time2,format="%H:%M")
  data$hours<- data$time3$hour + data$time3$min/60 
  
  #set time range as decimal hours
  startTime<-as.POSIXlt(startTime,format="%H:%M")
  startTime<- startTime$hour + startTime$min/60 
  endTime<-as.POSIXlt(endTime,format="%H:%M")
  endTime<- endTime$hour + endTime$min/60
  data<-data[which(data$hours>=startTime & data$hours<=endTime),]
  
  #subset xticks
  xticksRange<-as.POSIXlt(xticks,format="%H:%M")
  xticksRange<- xticksRange$hour + xticksRange$min/60
  xticks<-xticks[xticksRange>=startTime & xticksRange<=endTime]

  xticks.list<-named.list(data,xticks,startTime,endTime)
  return(xticks.list)
  }